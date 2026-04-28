#!/usr/bin/env Rscript

# This script screens respondent characteristics for predictors of refusal to
# cut military spending. The LASSO selection step uses weighted logistic LASSO
# because glmnet does not provide a probit LASSO; the selected specifications
# are then refit as weighted probits, matching the main defense-spending
# analysis.

# Setup ----

required_packages <- c("data.table", "fixest", "glmnet", "marginaleffects", "modelsummary")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them or update the project renv before running 04_lasso.R.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(glmnet)
  library(marginaleffects)
  library(modelsummary)
})

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)


# Settings ----

set.seed(202504)

outcome_var <- "militaryexp_binary"
weight_var <- "weights"

core_vars <- c(
  "q30_q2",
  "q35",
  "q19",
  "continuous_age",
  "tranche_revenus",
  "gender"
)

# Respondent characteristics only. Keep compact parent variables and avoid
# question variables or redundant one-hot recodes.
characteristic_vars <- c(
  "continuous_age",
  "taille_agglo",
  "taillecommune",
  "departement",
  "grande_region",
  "idf",
  "milieu_geo",
  "pays_naissance",
  "dept_naissance",
  "matri",
  "pcs",
  "actif",
  "statut1",
  "partisane",
  "diplome",
  "niv_etude",
  "foyer",
  "tranche_revenus",
  "revenus_foyer_1",
  "contrib_principal",
  "typ_hab",
  "statut_logement",
  "gender"
)

min_nonmissing <- 100L
max_missing_rate <- 0.80
max_factor_levels <- 50L
cv_folds <- 10L


# Helper functions ----

empty_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

filter_valid_weights <- function(data) {
  if (!weight_var %chin% names(data)) {
    stop("Column 'weights' is missing from final_survey2025.csv.", call. = FALSE)
  }

  data[, weights := as.numeric(weights)]

  if (all(is.na(data$weights))) {
    stop("Column 'weights' is empty or non-numeric.", call. = FALSE)
  }

  data <- data[!is.na(weights) & weights > 0]

  if (nrow(data) == 0) {
    stop("No observations with valid positive survey weights.", call. = FALSE)
  }

  data
}

relevel_if_present <- function(x, ref) {
  x <- factor(x)

  if (ref %in% levels(x)) {
    x <- relevel(x, ref = ref)
  }

  x
}

relevel_income_factor <- function(x) {
  income_levels <- c(
    "Moins de 12 000€; (soit moins de 1 000€; par mois)",
    "De 12 000€; à moins de 18 000€; (1 000€; à 1 500€; par mois)",
    "De 18 000€; à moins de 24 000€; (1 500€; à 2 000€; par mois)",
    "De 24 000€; à moins de 36 000€; (2 000€; à 3 000€; par mois)",
    "De 36 000€; à moins de 48 000€; (3 000€; à 4 000€; par mois)",
    "De 48 000€; à moins de 60 000€; (4 000€; à 5 000€; par mois)",
    "De 60 000€; à moins de 72 000€; (5 000€; à 6 000€; par mois)",
    "De 72 000€; à moins de 144 000€; (6 000€; à 12 000€; par mois)",
    "144 000€; et plus (12 000€; par mois et plus)",
    "Je ne souhaite pas répondre"
  )

  x_chr <- as.character(x)
  present_levels <- income_levels[income_levels %chin% unique(x_chr)]
  extra_levels <- sort(setdiff(unique(x_chr), c(present_levels, NA_character_)))
  factor(x_chr, levels = c(present_levels, extra_levels))
}

is_supported_predictor <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x) ||
    is.numeric(x) || is.integer(x)
}

build_candidate_metadata <- function(data) {
  candidate_vars <- unique(c(characteristic_vars, core_vars))
  candidate_vars <- intersect(candidate_vars, names(data))

  candidate_vars <- candidate_vars[
    vapply(data[, ..candidate_vars], is_supported_predictor, logical(1))
  ]

  metadata <- data.table(
    variable = candidate_vars,
    class = vapply(data[, ..candidate_vars], function(x) paste(class(x), collapse = "/"), character(1)),
    nonmissing = vapply(data[, ..candidate_vars], function(x) sum(!is.na(x)), integer(1)),
    unique_values = vapply(data[, ..candidate_vars], uniqueN, integer(1), na.rm = TRUE),
    is_numeric = vapply(data[, ..candidate_vars], function(x) is.numeric(x) || is.integer(x), logical(1))
  )

  metadata[, missing_rate := 1 - nonmissing / nrow(data)]
  metadata[
    ,
    keep := nonmissing >= min_nonmissing &
      unique_values >= 2L &
      missing_rate <= max_missing_rate &
      (is_numeric | unique_values <= max_factor_levels)
  ]

  metadata[variable %chin% core_vars, keep := TRUE]

  non_core_q_vars <- setdiff(grep("^q", metadata[keep == TRUE, variable], value = TRUE), core_vars)
  if (length(non_core_q_vars) > 0) {
    stop(
      "Non-core question variables entered the LASSO candidate set: ",
      paste(non_core_q_vars, collapse = ", "),
      call. = FALSE
    )
  }

  metadata[order(variable)]
}

prepare_lasso_data <- function(data, candidate_vars) {
  required_vars <- c(outcome_var, weight_var, candidate_vars)
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      "Required LASSO variables missing from final_survey2025.csv: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  analysis <- copy(data[, ..required_vars])
  variable_map <- data.table(model_var = candidate_vars, source_var = candidate_vars)

  for (var in candidate_vars) {
    x <- analysis[[var]]

    if (is.character(x) || is.factor(x) || is.logical(x)) {
      x <- as.character(x)
      x[is.na(x) | trimws(x) == ""] <- "(Missing)"
      analysis[[var]] <- factor(x)
    } else if (is.numeric(x) || is.integer(x)) {
      missing_indicator <- paste0(var, "_missing")
      x <- as.numeric(x)
      missing_x <- is.na(x)

      if (any(missing_x)) {
        median_x <- median(x, na.rm = TRUE)

        if (is.na(median_x) || is.nan(median_x)) {
          stop("Could not impute numeric variable: ", var, call. = FALSE)
        }

        x[missing_x] <- median_x
        analysis[[missing_indicator]] <- as.integer(missing_x)
        variable_map <- rbind(
          variable_map,
          data.table(model_var = missing_indicator, source_var = var)
        )
      }

      analysis[[var]] <- x
    }
  }

  if ("q30_q2" %chin% names(analysis)) {
    analysis[, q30_q2 := relevel_if_present(q30_q2, "Pas du tout confiance")]
  }

  if ("q35" %chin% names(analysis)) {
    analysis[, q35 := relevel_if_present(q35, "Très élevé")]
  }

  if ("q19" %chin% names(analysis)) {
    analysis[, q19 := relevel_if_present(q19, "Ni trop, ni pas assez élevés")]
  }

  if ("gender" %chin% names(analysis)) {
    analysis[, gender := relevel_if_present(gender, "Femme")]
  }

  if ("tranche_revenus" %chin% names(analysis)) {
    analysis[, tranche_revenus := relevel_income_factor(tranche_revenus)]
  }

  list(
    data = analysis,
    variable_map = unique(variable_map)
  )
}

build_design_matrix <- function(data, model_vars, variable_map) {
  model_formula <- reformulate(model_vars, response = outcome_var)
  design <- model.matrix(model_formula, data = data)
  assign_index <- attr(design, "assign")
  term_labels <- attr(terms(model_formula), "term.labels")

  design <- design[, -1L, drop = FALSE]
  assign_index <- assign_index[-1L]

  mapping <- data.table(
    matrix_col = colnames(design),
    model_var = term_labels[assign_index]
  )
  mapping <- variable_map[mapping, on = "model_var"]

  nonconstant <- vapply(
    seq_len(ncol(design)),
    function(j) uniqueN(design[, j]) > 1L,
    logical(1)
  )

  design <- design[, nonconstant, drop = FALSE]
  mapping <- mapping[nonconstant]

  if (ncol(design) == 0) {
    stop("The LASSO design matrix has no non-constant predictor columns.", call. = FALSE)
  }

  list(
    matrix = design,
    mapping = mapping,
    formula = model_formula
  )
}

build_foldid <- function(y, nfolds) {
  foldid <- integer(length(y))

  for (class_value in sort(unique(y))) {
    idx <- which(y == class_value)
    idx <- sample(idx, length(idx))
    foldid[idx] <- rep(seq_len(nfolds), length.out = length(idx))
  }

  foldid
}

extract_selected_predictors <- function(cv_fit,
                                        mapping,
                                        spec_name,
                                        lambda_choice) {
  lambda_value <- switch(
    lambda_choice,
    lambda.1se = cv_fit$lambda.1se,
    lambda.min = cv_fit$lambda.min,
    stop("Unknown lambda choice: ", lambda_choice, call. = FALSE)
  )

  coef_matrix <- coef(cv_fit, s = lambda_choice)
  selected <- data.table(
    matrix_col = rownames(coef_matrix),
    coefficient = as.numeric(coef_matrix)
  )[
    matrix_col != "(Intercept)" & coefficient != 0
  ]

  if (nrow(selected) == 0) {
    return(data.table(
      model = character(),
      lambda_choice = character(),
      lambda_value = numeric(),
      matrix_col = character(),
      model_var = character(),
      source_var = character(),
      coefficient = numeric()
    ))
  }

  selected <- mapping[selected, on = "matrix_col"]
  selected[
    ,
    `:=`(
      model = spec_name,
      lambda_choice = lambda_choice,
      lambda_value = lambda_value
    )
  ]

  selected[
    ,
    .(model, lambda_choice, lambda_value, matrix_col, model_var, source_var, coefficient)
  ][
    order(model, lambda_choice, source_var, matrix_col)
  ]
}

run_lasso_spec <- function(design,
                           y,
                           weights,
                           mapping,
                           spec_name,
                           unpenalized_source_vars = character()) {
  penalty_factor <- rep(1, ncol(design))

  if (length(unpenalized_source_vars) > 0) {
    penalty_factor[mapping$source_var %chin% unpenalized_source_vars] <- 0
  }

  foldid <- build_foldid(y, min(cv_folds, min(table(y))))

  cv_fit <- cv.glmnet(
    x = design,
    y = y,
    weights = weights,
    family = "binomial",
    alpha = 1,
    foldid = foldid,
    standardize = TRUE,
    type.measure = "deviance",
    penalty.factor = penalty_factor
  )

  selected <- rbindlist(
    lapply(
      c("lambda.1se", "lambda.min"),
      function(lambda_choice) {
        extract_selected_predictors(
          cv_fit = cv_fit,
          mapping = mapping,
          spec_name = spec_name,
          lambda_choice = lambda_choice
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )

  list(
    cv_fit = cv_fit,
    selected = selected
  )
}

selected_model_vars <- function(selected, model_name, lambda_choice = "lambda.1se") {
  requested_lambda <- lambda_choice
  sort(unique(selected[model == model_name & lambda_choice == requested_lambda, model_var]))
}

fit_post_lasso_ame <- function(data, selected_vars, model_label) {
  if (length(selected_vars) == 0) {
    warning("No predictors selected for ", model_label, "; skipping post-LASSO AME table column.")
    return(NULL)
  }

  model_formula <- reformulate(selected_vars, response = outcome_var)
  model <- feglm(
    model_formula,
    family = binomial(link = "probit"),
    vcov = "hetero",
    weights = ~ weights,
    data = data
  )

  ame <- avg_slopes(
    model,
    newdata = data,
    wts = "weights"
  )

  list(
    model = model,
    ame = ame,
    nobs = nobs(model),
    nvars = length(selected_vars),
    label = model_label
  )
}

clean_post_lasso_ame_labels <- function(ame) {
  original_class <- class(ame)
  ame <- as.data.table(ame)

  revenue_label_map <- c(
    "Moins de 12 000€; (soit moins de 1 000€; par mois)" = "< 12k",
    "De 12 000€; à moins de 18 000€; (1 000€; à 1 500€; par mois)" = "12k-18k",
    "De 18 000€; à moins de 24 000€; (1 500€; à 2 000€; par mois)" = "18k-24k",
    "De 24 000€; à moins de 36 000€; (2 000€; à 3 000€; par mois)" = "24k-36k",
    "De 36 000€; à moins de 48 000€; (3 000€; à 4 000€; par mois)" = "36k-48k",
    "De 48 000€; à moins de 60 000€; (4 000€; à 5 000€; par mois)" = "48k-60k",
    "De 60 000€; à moins de 72 000€; (5 000€; à 6 000€; par mois)" = "60k-72k",
    "De 72 000€; à moins de 144 000€; (6 000€; à 12 000€; par mois)" = "72k-144k",
    "144 000€; et plus (12 000€; par mois et plus)" = "144k+",
    "Je ne souhaite pas répondre" = "NSP",
    "Moins de 1 000€; par mois" = "< 1k/mois",
    "De 1 000€; à moins de 1 500€; par mois" = "1k-1,5k/mois",
    "De 1 500€; à moins de 2 000€; par mois" = "1,5k-2k/mois",
    "De 2 000€; à moins de 3 000€; par mois" = "2k-3k/mois",
    "De 3 000€; à moins de 4 000€; par mois" = "3k-4k/mois",
    "De 4 000€; à moins de 5 000€; par mois" = "4k-5k/mois",
    "De 5 000€; à moins de 6 000€; par mois" = "5k-6k/mois",
    "De 6 000€; à moins de 12 000€; par mois" = "6k-12k/mois",
    "12 000€; par mois et plus" = "12k+/mois"
  )

  term_label_map <- c(
    "q19" = "Niveau d'imposition",
    "q30_q2" = "Confiance dans l'État",
    "q35" = "Risque de conflit",
    "gender" = "Genre",
    "continuous_age" = "Age",
    "tranche_revenus" = "Revenus",
    "age2" = "Âge détaillé",
    "tranche_age" = "Tranche d'âge",
    "matri" = "Situation matrimoniale",
    "diplome" = "Diplôme",
    "niv_etude" = "Niveau d'étude",
    "pcs" = "PCS répondant",
    "revenus_foyer_1" = "Revenus mensuels du foyer",
    "statut1" = "Statut d'activité",
    "actif" = "Actif",
    "contrib_principal" = "Contributeur principal",
    "partisane" = "Proximité partisane",
    "grande_region" = "Région",
    "taillecommune" = "Taille commune"
  )

  ame[term %chin% names(term_label_map), term := unname(term_label_map[term])]

  contrast_replacements <- c(
    "Pas assez élevés - Ni trop, ni pas assez élevés" = "Pas assez - Juste",
    "Trop élevés - Ni trop, ni pas assez élevés" = "Trop élevés - Juste",
    "Ni trop, ni pas assez élevés - Trop élevés" = "Juste - Trop élevés",
    "Pas assez élevés - Trop élevés" = "Pas assez - Trop élevés",
    "Plutôt confiance - Pas du tout confiance" = "Plutôt conf. - Pas du tout",
    "Plutôt pas confiance - Pas du tout confiance" = "Plutôt pas conf. - Pas du tout",
    "Tout à fait confiance - Pas du tout confiance" = "Tout à fait conf. - Pas du tout",
    "Vous ne savez pas - Très élevé" = "NSP - Très élevé",
    "Homme - Femme" = "Homme - Femme",
    "Oui - Je ne suis pas concerné(e) / Je vis seul(e)" = "Oui - Non concerné / seul",
    "Non - Je ne suis pas concerné(e) / Je vis seul(e)" = "Non - Non concerné / seul",
    "Non - (Missing)" = "Non - Non réponse",
    "Oui - (Missing)" = "Oui - Non réponse",
    "(Missing)" = "Non réponse",
    "Ne sait pas / Refus" = "NSP / refus",
    "Ecoles de commerce, grandes écoles" = "Grandes écoles",
    "Supérieur à Bac +5" = "> Bac +5",
    "Sans diplôme / Primaire" = "Sans diplôme / primaire",
    "Elève, étudiant" = "Élève / étudiant",
    "Artisan, commerçant et assimilé, chef d’entreprise" = "Artisan / commerçant / chef d'entreprise",
    "Autre, sans activité professionnelle" = "Autre sans activité",
    "Cadre d'entreprise, cadre de la fonction publique, profession intellectuelle et artistique supérieure" = "Cadre / profession intellectuelle sup.",
    "En recherche d'un premier emploi" = "Premier emploi",
    "Femme, homme au foyer" = "Au foyer",
    "Profession intermédiaire (technicien, contremaître, agent de maîtrise, professeur des écoles, instituteur, infirmier, éducateur ...)" = "Profession intermédiaire",
    "Profession libérale et assimilée" = "Profession libérale",
    "Renaissance (ex-La République En Marche)" = "Renaissance",
    "Les Ecologistes (ex-Europe Ecologie Les Verts)" = "Les Ecologistes",
    "Le NPA (Nouveau Parti Anticapitaliste)" = "Le NPA",
    "L’UDI (Union des Démocrates et Indépendants)" = "UDI",
    "Debout la France (de Nicolas Dupont-Aignan)" = "Debout la France",
    "Reconquête ! (d'Éric Zemmour)" = "Reconquête"
  )

  for (long_label in names(c(revenue_label_map, contrast_replacements))) {
    ame[, contrast := gsub(long_label, c(revenue_label_map, contrast_replacements)[[long_label]], contrast, fixed = TRUE)]
  }

  ame[term == "Age", contrast := ""]

  reference_suffixes <- c(
    " - Pas du tout d’accord",
    " - Pas du tout",
    " - Pas du tout confiance",
    " - Juste",
    " - Ni trop, ni pas assez élevés",
    " - Trop élevés",
    " - Très élevé",
    " - Femme",
    " - < 12k",
    " - Bac",
    " - Agriculteur exploitant",
    " - 100 000 habitants et plus",
    " - Nord-Est",
    " - Non réponse",
    " - Non concerné / seul",
    " - 12k+/mois",
    " - 18 à 24 ans",
    " - 18-24 ans",
    " - Autre"
  )

  for (suffix in reference_suffixes) {
    suffix_idx <- !is.na(ame$contrast) & endsWith(ame$contrast, suffix)
    ame$contrast[suffix_idx] <- substring(
      ame$contrast[suffix_idx],
      1L,
      nchar(ame$contrast[suffix_idx]) - nchar(suffix)
    )
  }

  ame[term != "Age" & (is.na(contrast) | contrast == ""), contrast := "Non réponse"]

  ame <- as.data.frame(ame)
  class(ame) <- original_class
  ame
}

order_post_lasso_ame_rows <- function(ame, term_order) {
  contrast_order <- list(
    "Confiance dans l'État" = c("Plutôt conf.", "Plutôt pas conf.", "Tout à fait conf.", "Non réponse"),
    "Niveau d'imposition" = c("Trop élevés", "Pas assez", "Non réponse"),
    "Risque de conflit" = c("Assez élevé", "Plutôt faible", "Très faible", "NSP", "Non réponse"),
    "Genre" = "Homme",
    "Age" = "",
    "Revenus" = c("12k-18k", "18k-24k", "24k-36k", "36k-48k", "48k-60k", "60k-72k", "72k-144k", "144k+", "NSP"),
    "Tranche d'âge" = c("25-34 ans", "35-49 ans", "50-64 ans", "65 ans et plus"),
    "Âge détaillé" = c("25 à 29 ans", "30 à 34 ans", "35 à 39 ans", "40 à 44 ans", "45 à 49 ans", "50 à 54 ans", "55 à 59 ans", "60 à 64 ans", "65 à 69 ans", "70 ans et plus"),
    "Niveau d'étude" = c("Inférieur au Bac", "Bac +2", "Supérieur à Bac +2", "NSP/Refus"),
    "Région" = c("Région Parisienne", "Nord-Ouest", "Sud-Ouest", "Sud-Est"),
    "Taille commune" = c("De 0 à 499 habitants", "De 500 à 999 habitants", "De 1 000 à 1 999 habitants", "De 2 000 à 3 499 habitants", "De 3 500 à 4 999 habitants", "De 5 000 à 9 999 habitants", "De 10 000 à 19 999 habitants", "De 20 000 à 49 999 habitants", "De 50 000 à 99 999 habitants"),
    "Revenus mensuels du foyer" = c("< 1k/mois", "1k-1,5k/mois", "1,5k-2k/mois", "2k-3k/mois", "3k-4k/mois", "4k-5k/mois", "5k-6k/mois", "6k-12k/mois", "NSP")
  )

  contrast_rank <- vapply(
    seq_len(nrow(ame)),
    function(i) {
      term_contrast_order <- contrast_order[[ame$term[i]]]
      if (is.null(term_contrast_order)) {
        return(Inf)
      }

      rank <- match(ame$contrast[i], term_contrast_order)
      ifelse(is.na(rank), length(term_contrast_order) + 1L, rank)
    },
    numeric(1)
  )

  ordering_frame <- data.table(
    term_rank = fifelse(is.na(match(ame$term, term_order)), length(term_order) + 1L, match(ame$term, term_order)),
    contrast_rank = contrast_rank
  )
  order_idx <- do.call(order, ordering_frame)
  ame[order_idx, ]
}

escape_regex <- function(x) {
  gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", x, perl = TRUE)
}

build_coef_omit_pattern <- function(all_terms, keep_terms) {
  omit_terms <- setdiff(all_terms, keep_terms)

  if (length(omit_terms) == 0) {
    return(NULL)
  }

  escaped_terms <- vapply(omit_terms, escape_regex, character(1))
  paste0("^(", paste(escaped_terms, collapse = "|"), ")$")
}

build_post_lasso_add_rows <- function(available) {
  add_rows <- data.frame(
    term = c("Observations", "Variables retenues"),
    contrast = c("", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (result in available) {
    add_rows[[result$label]] <- c(as.character(result$nobs), as.character(result$nvars))
  }

  add_rows
}

post_lasso_model_label <- function(model_label) {
  label_map <- c(
    "Penalize all" = "lasso exhaustif",
    "Unpenalized core" = "variable clefs + lasso"
  )

  ifelse(model_label %chin% names(label_map), unname(label_map[model_label]), model_label)
}

write_post_lasso_table <- function(post_lasso_results,
                                   output_file,
                                   title,
                                   keep_terms,
                                   term_order,
                                   include_add_rows = FALSE) {
  available <- post_lasso_results[
    vapply(post_lasso_results, Negate(is.null), logical(1))
  ]

  if (length(available) == 0) {
    warning("No post-LASSO AME results available; TeX table was not written.")
    return(invisible(FALSE))
  }

  ame_list <- lapply(
    available,
    function(result) order_post_lasso_ame_rows(clean_post_lasso_ame_labels(result$ame), term_order)
  )
  names(ame_list) <- vapply(available, `[[`, character(1), "label")

  has_keep_terms <- vapply(
    ame_list,
    function(ame) any(as.data.table(ame)$term %chin% keep_terms),
    logical(1)
  )

  if (!any(has_keep_terms)) {
    warning("No post-LASSO AME terms available for table: ", title)
    return(invisible(FALSE))
  }

  omitted_model_labels <- post_lasso_model_label(names(ame_list)[!has_keep_terms])

  if (any(!has_keep_terms)) {
    warning(
      "Omitting model column(s) with no selected terms in table '",
      title,
      "': ",
      paste(names(ame_list)[!has_keep_terms], collapse = ", ")
    )
  }

  available <- available[has_keep_terms]
  ame_list <- ame_list[has_keep_terms]

  all_terms <- unique(unlist(lapply(ame_list, function(ame) unique(as.data.table(ame)$term))))
  table_notes <- c(
    "Sélection par LASSO logistique pondéré ; réestimation post-LASSO par probit pondéré.",
    if ("Unpenalized core" %chin% names(ame_list)) {
      "La colonne coeur non pénalisé force les variables du modèle principal et sélectionne les signaux additionnels."
    },
    if (length(omitted_model_labels) > 0) {
      paste0(
        "Colonnes omises faute de variable retenue dans ce thème : ",
        paste(omitted_model_labels, collapse = ", "),
        "."
      )
    },
    "+ p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )

  modelsummary(
    ame_list,
    title = title,
    output = output_file,
    escape = FALSE,
    shape = term + contrast ~ model,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    coef_omit = build_coef_omit_pattern(all_terms, keep_terms),
    add_rows = if (include_add_rows) build_post_lasso_add_rows(available) else NULL,
    gof_omit = ".*",
    notes = table_notes
  )

  postprocess_post_lasso_table(
    path = output_file,
    term_labels = term_order,
    reference_labels = post_lasso_reference_labels(),
    model_labels = post_lasso_model_label(names(ame_list))
  )

  invisible(TRUE)
}

post_lasso_reference_labels <- function() {
  c(
    "Confiance dans l'État" = "Pas du tout confiance",
    "Niveau d'imposition" = "Juste",
    "Risque de conflit" = "Très élevé",
    "Genre" = "Femme",
    "Revenus" = "< 12k",
    "Âge détaillé" = "18 à 24 ans",
    "Tranche d'âge" = "18-24 ans",
    "Situation matrimoniale" = "Autre",
    "Diplôme" = "Bac",
    "Niveau d'étude" = "Bac",
    "PCS répondant" = "Agriculteur exploitant",
    "Revenus mensuels du foyer" = "12k+/mois",
    "Statut d'activité" = "Non réponse",
    "Actif" = "Non réponse",
    "Contributeur principal" = "Non concerné / seul",
    "Proximité partisane" = "Non réponse",
    "Région" = "Nord-Est",
    "Taille commune" = "100 000 habitants et plus"
  )
}

postprocess_post_lasso_table <- function(path,
                                         term_labels,
                                         reference_labels,
                                         model_labels) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\\\\centering", "\\\\centering\n\\\\scriptsize", lines, fixed = FALSE)
  model_columns <- length(model_labels)

  header_idx <- grep("TinyTableHeader", lines, fixed = TRUE)
  if (length(header_idx) != 1L) {
    stop("Could not locate the post-LASSO table header row for post-processing.", call. = FALSE)
  }

  lines[header_idx] <- "\\midrule %% TinyTableHeader"
  spanner_line <- paste0(
    "& Modalité & \\SetCell[c=", model_columns, "]{c} Refus de baisser la dépense de défense \\\\"
  )
  lines <- append(lines, spanner_line, after = header_idx - 1L)
  model_label_line <- paste0("& & ", paste(model_labels, collapse = " & "), " \\\\")
  lines <- append(lines, model_label_line, after = header_idx)

  first_term_idx <- which(vapply(
    lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))[1]
  observations_idx <- which(startsWith(lines, "Observations &"))
  bottomrule_idx <- which(startsWith(lines, "\\bottomrule"))
  body_end_idx <- if (length(observations_idx) == 1L) observations_idx - 1L else bottomrule_idx - 1L

  if (length(first_term_idx) != 1L || length(body_end_idx) != 1L || first_term_idx > body_end_idx) {
    stop("Could not locate the post-LASSO table body for post-processing.", call. = FALSE)
  }

  body_lines <- lines[first_term_idx:body_end_idx]
  block_starts <- which(vapply(
    body_lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))

  if (length(block_starts) == 0L) {
    stop("Could not identify term blocks in the post-LASSO table body.", call. = FALSE)
  }

  block_ends <- c(block_starts[-1L] - 1L, length(body_lines))
  blocks <- setNames(vector("list", length(term_labels)), term_labels)

  for (i in seq_along(block_starts)) {
    block <- body_lines[block_starts[i]:block_ends[i]]
    matched_term <- term_labels[vapply(
      term_labels,
      function(term_label) startsWith(block[1], paste0(term_label, " &")),
      logical(1)
    )]

    if (length(matched_term) == 1L) {
      blocks[[matched_term]] <- block
    }
  }

  reordered_lines <- character(0)
  reordered_start_idx <- integer(0)

  for (term_label in term_labels) {
    block <- blocks[[term_label]]
    if (length(block) == 0L) {
      next
    }

    reordered_start_idx <- c(reordered_start_idx, length(reordered_lines) + 1L)

    if (term_label %chin% names(reference_labels)) {
      reference_cells <- paste(rep("\\textit{Ref}", model_columns), collapse = " & ")
      original_row_prefix <- paste0(term_label, " &")
      block[1] <- paste0(
        term_label, " & ", reference_labels[[term_label]], " & ",
        reference_cells, " \\\\",
        "\n&", substring(block[1], nchar(original_row_prefix) + 1L)
      )
      block[1] <- sub("\n&  &", "\n& Non réponse &", block[1], fixed = TRUE)
    }

    reordered_lines <- c(reordered_lines, block)
  }

  for (i in seq_along(reordered_start_idx)) {
    row_idx <- reordered_start_idx[i]
    matched_term <- term_labels[vapply(
      term_labels,
      function(term_label) startsWith(reordered_lines[row_idx], paste0(term_label, " &")),
      logical(1)
    )]

    if (length(matched_term) != 1L) {
      next
    }

    term_prefix <- paste0(matched_term, " &")
    reordered_lines[row_idx] <- paste0(
      "\\textbf{", matched_term, "} &",
      substring(reordered_lines[row_idx], nchar(term_prefix) + 1L)
    )

    if (i > 1L) {
      reordered_lines[row_idx] <- paste0("\\midrule\n", reordered_lines[row_idx])
    }
  }

  lines <- c(
    lines[seq_len(first_term_idx - 1L)],
    reordered_lines,
    lines[(body_end_idx + 1L):length(lines)]
  )

  observations_idx <- which(startsWith(lines, "Observations &"))
  if (length(observations_idx) == 1L) {
    lines[observations_idx] <- paste0("\\midrule\\midrule\n", lines[observations_idx])
  }

  writeLines(lines, path, useBytes = TRUE)
}

write_post_lasso_tables <- function(post_lasso_results, table_specs) {
  available <- post_lasso_results[
    vapply(post_lasso_results, Negate(is.null), logical(1))
  ]
  ame_list <- lapply(available, function(result) clean_post_lasso_ame_labels(result$ame))
  all_terms <- unique(unlist(lapply(ame_list, function(ame) unique(ame$term))))
  covered_terms <- unique(unlist(lapply(table_specs, function(spec) spec$term_order)))
  missing_terms <- setdiff(all_terms, covered_terms)

  if (length(missing_terms) > 0) {
    stop(
      "The themed post-LASSO tables do not cover these AME terms: ",
      paste(missing_terms, collapse = ", "),
      call. = FALSE
    )
  }

  output_files <- character(length(table_specs))

  for (i in seq_along(table_specs)) {
    spec <- table_specs[[i]]
    output_file <- file.path(table_dir, spec$file_name)

    write_post_lasso_table(
      post_lasso_results = post_lasso_results,
      output_file = output_file,
      title = spec$title,
      keep_terms = spec$keep_terms,
      term_order = spec$term_order,
      include_add_rows = isTRUE(spec$include_add_rows)
    )

    output_files[i] <- output_file
    message("Table saved to: ", output_file)
  }

  invisible(output_files)
}


# Data preparation ----

survey2025 <- fread(file.path(data_final_dir, "final_survey2025.csv"))[treatment == 0]
survey2025 <- filter_valid_weights(survey2025)
survey2025 <- survey2025[!is.na(militaryexp_binary)]

if (nrow(survey2025) == 0) {
  stop("No control-group observations with non-missing militaryexp_binary.", call. = FALSE)
}

character_cols <- names(survey2025)[vapply(survey2025, is.character, logical(1))]
survey2025[, (character_cols) := lapply(.SD, empty_to_na), .SDcols = character_cols]

candidate_metadata <- build_candidate_metadata(survey2025)
candidate_vars <- candidate_metadata[keep == TRUE, variable]

missing_core_vars <- setdiff(core_vars, candidate_vars)
if (length(missing_core_vars) > 0) {
  stop(
    "Core variables missing from the LASSO candidate set: ",
    paste(missing_core_vars, collapse = ", "),
    call. = FALSE
  )
}

prepared <- prepare_lasso_data(survey2025, candidate_vars)
model_vars <- unique(prepared$variable_map$model_var)
design <- build_design_matrix(
  data = prepared$data,
  model_vars = model_vars,
  variable_map = prepared$variable_map
)

y <- prepared$data[[outcome_var]]
w <- prepared$data[[weight_var]]

if (!all(y %in% c(0, 1))) {
  stop("militaryexp_binary must be coded 0/1 for glmnet.", call. = FALSE)
}


# LASSO selection ----

lasso_penalized_all <- run_lasso_spec(
  design = design$matrix,
  y = y,
  weights = w,
  mapping = design$mapping,
  spec_name = "Penalize all",
  unpenalized_source_vars = character()
)

lasso_unpenalized_core <- run_lasso_spec(
  design = design$matrix,
  y = y,
  weights = w,
  mapping = design$mapping,
  spec_name = "Unpenalized core",
  unpenalized_source_vars = core_vars
)

selected_predictors <- rbindlist(
  list(lasso_penalized_all$selected, lasso_unpenalized_core$selected),
  use.names = TRUE,
  fill = TRUE
)

selected_variables <- selected_predictors[
  ,
  .(
    selected_matrix_columns = .N,
    max_abs_coefficient = max(abs(coefficient)),
    model_vars = paste(sort(unique(model_var)), collapse = "; ")
  ),
  by = .(model, lambda_choice, source_var)
][
  order(model, lambda_choice, -max_abs_coefficient, source_var)
]

selected_variables_file <- file.path(table_dir, "lasso_selected_variables.csv")
stale_selected_predictors_file <- file.path(table_dir, "lasso_selected_predictors.csv")

fwrite(selected_variables, selected_variables_file, na = "")
if (file.exists(stale_selected_predictors_file)) {
  invisible(file.remove(stale_selected_predictors_file))
}


# Post-LASSO weighted probit AMEs ----

penalized_all_vars <- selected_model_vars(selected_predictors, "Penalize all")
unpenalized_core_vars <- selected_model_vars(selected_predictors, "Unpenalized core")

post_lasso_results <- list(
  "Penalize all" = fit_post_lasso_ame(
    data = prepared$data,
    selected_vars = penalized_all_vars,
    model_label = "Penalize all"
  ),
  "Unpenalized core" = fit_post_lasso_ame(
    data = prepared$data,
    selected_vars = unpenalized_core_vars,
    model_label = "Unpenalized core"
  )
)

post_lasso_table_specs <- list(
  list(
    file_name = "lasso_postlasso_ame_01_attitudes.tex",
    title = "Post-LASSO : attitudes",
    term_order = c("Confiance dans l'État", "Niveau d'imposition", "Risque de conflit"),
    keep_terms = c("Confiance dans l'État", "Niveau d'imposition", "Risque de conflit"),
    include_add_rows = TRUE
  ),
  list(
    file_name = "lasso_postlasso_ame_02_demographie.tex",
    title = "Post-LASSO : démographie",
    term_order = c("Genre", "Age", "Âge détaillé", "Tranche d'âge", "Situation matrimoniale"),
    keep_terms = c("Genre", "Age", "Âge détaillé", "Tranche d'âge", "Situation matrimoniale"),
    include_add_rows = FALSE
  ),
  list(
    file_name = "lasso_postlasso_ame_03_revenus_activite.tex",
    title = "Post-LASSO : revenus et activité",
    term_order = c(
      "Revenus",
      "Revenus mensuels du foyer",
      "Statut d'activité",
      "Actif"
    ),
    keep_terms = c(
      "Revenus",
      "Revenus mensuels du foyer",
      "Statut d'activité",
      "Actif"
    ),
    include_add_rows = FALSE
  ),
  list(
    file_name = "lasso_postlasso_ame_04_diplome_pcs.tex",
    title = "Post-LASSO : diplôme, niveau d'étude et PCS",
    term_order = c("Diplôme", "Niveau d'étude", "PCS répondant"),
    keep_terms = c("Diplôme", "Niveau d'étude", "PCS répondant"),
    include_add_rows = FALSE
  ),
  list(
    file_name = "lasso_postlasso_ame_05_politique.tex",
    title = "Post-LASSO : politique",
    term_order = c("Proximité partisane"),
    keep_terms = c("Proximité partisane"),
    include_add_rows = FALSE
  ),
  list(
    file_name = "lasso_postlasso_ame_06_territoire.tex",
    title = "Post-LASSO : territoire",
    term_order = c("Région", "Taille commune", "Contributeur principal"),
    keep_terms = c("Région", "Taille commune", "Contributeur principal"),
    include_add_rows = FALSE
  )
)

write_post_lasso_tables(post_lasso_results, post_lasso_table_specs)

legacy_post_lasso_table_file <- file.path(table_dir, "lasso_postlasso_ame.tex")
if (file.exists(legacy_post_lasso_table_file)) {
  invisible(file.remove(legacy_post_lasso_table_file))
}

stale_post_lasso_table_files <- file.path(
  table_dir,
  c(
    "lasso_postlasso_ame_01_sociodemographie.tex",
    "lasso_postlasso_ame_02_territoire_socioeco.tex",
    "lasso_postlasso_ame_03_attitudes_politique.tex",
    "lasso_postlasso_ame_03_socioeco.tex",
    "lasso_postlasso_ame_04_territoire_politique.tex"
  )
)
stale_post_lasso_table_files <- stale_post_lasso_table_files[file.exists(stale_post_lasso_table_files)]
if (length(stale_post_lasso_table_files) > 0) {
  invisible(file.remove(stale_post_lasso_table_files))
}


# Console summary ----

cat("\nLASSO defense-consent screen\n")
cat("Control-group observations: ", nrow(prepared$data), "\n", sep = "")
cat("Candidate source variables: ", length(candidate_vars), "\n", sep = "")
cat("Design-matrix columns: ", ncol(design$matrix), "\n", sep = "")
cat(
  "Selected source variables, penalize all (lambda.1se): ",
  uniqueN(selected_predictors[model == "Penalize all" & lambda_choice == "lambda.1se", source_var]),
  "\n",
  sep = ""
)
cat(
  "Selected source variables, unpenalized core (lambda.1se): ",
  uniqueN(selected_predictors[model == "Unpenalized core" & lambda_choice == "lambda.1se", source_var]),
  "\n",
  sep = ""
)
cat("Selected variable file: ", selected_variables_file, "\n", sep = "")
cat("Post-LASSO AME tables:\n")
for (spec in post_lasso_table_specs) {
  cat("  - ", file.path(table_dir, spec$file_name), "\n", sep = "")
}
