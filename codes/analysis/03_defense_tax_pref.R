#!/usr/bin/env Rscript

# This script estimates a mixed binary probit model for q36 funding preferences.
# q36 is treated as a multiple-response question, not forced into a single
# nominal outcome.

# Setup ----

add_local_renv_library <- function() {
  r_minor <- sub("^(\\d+\\.\\d+).*", "\\1", as.character(getRversion()))
  candidates <- Sys.glob(file.path("renv", "library", "*", paste0("R-", r_minor), R.version$platform))
  candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
  candidates <- candidates[dir.exists(candidates)]

  if (length(candidates) > 0L) {
    .libPaths(unique(c(candidates, .libPaths())))
  }
}

add_local_renv_library()

required_packages <- c("data.table", "fixest", "lme4", "MNP", "mvProbit")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install them or update the project renv before running 03_defense_tax_pref.R.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(lme4)
  library(MNP)
  library(mvProbit)
})

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
table_latex_dir <- file.path(table_dir, "latex")
table_html_dir <- file.path(table_dir, "html")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_latex_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_html_dir, recursive = TRUE, showWarnings = FALSE)


# Labels and settings ----

q36_option_labels <- c(
  q36_q1 = "Baisse des autres dépenses publiques",
  q36_q2 = "Hausse des impôts ou cotisations sociales",
  q36_q3 = "Augmentation du déficit et de la dette",
  q36_q4 = "Augmentation du nombre d'heures ou de jours travaillés"
)

q36_option_short_labels <- c(
  q36_q1 = "Baisse dépenses",
  q36_q2 = "Hausse impôts",
  q36_q3 = "Déficit/dette",
  q36_q4 = "Temps travaillé"
)

q35_levels <- c(
  "Vous ne savez pas",
  "Très élevé",
  "Assez élevé",
  "Plutôt faible",
  "Très faible"
)

q30_levels <- c(
  "Pas du tout confiance",
  "Plutôt pas confiance",
  "Plutôt confiance",
  "Tout à fait confiance"
)

satisfaction_levels <- c(
  "Pas du tout satisfait(e)",
  "Plutôt pas satisfait(e)",
  "Plutôt satisfait(e)",
  "Très satisfait(e)"
)

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

income_label_map <- c(
  "Moins de 12 000€; (soit moins de 1 000€; par mois)" = "< 12k",
  "De 12 000€; à moins de 18 000€; (1 000€; à 1 500€; par mois)" = "12k--18k",
  "De 18 000€; à moins de 24 000€; (1 500€; à 2 000€; par mois)" = "18k--24k",
  "De 24 000€; à moins de 36 000€; (2 000€; à 3 000€; par mois)" = "24k--36k",
  "De 36 000€; à moins de 48 000€; (3 000€; à 4 000€; par mois)" = "36k--48k",
  "De 48 000€; à moins de 60 000€; (4 000€; à 5 000€; par mois)" = "48k--60k",
  "De 60 000€; à moins de 72 000€; (5 000€; à 6 000€; par mois)" = "60k--72k",
  "De 72 000€; à moins de 144 000€; (6 000€; à 12 000€; par mois)" = "72k--144k",
  "144 000€; et plus (12 000€; par mois et plus)" = "144k+",
  "Je ne souhaite pas répondre" = "NSP"
)

q36_focal_vars <- c(
  "q30_q2", "q35", "q19", "q5", "continuous_age_scaled",
  "continuous_age_scaled_sq", "acte_citoyen", "q28", "gender",
  "tranche_revenus"
)
q36_control_vars <- c("pcs", "matri", "foyer", "tailleagglo5", "diplome", "partisane", "sat")
q36_mnp_draws <- as.integer(Sys.getenv("Q36_MNP_DRAWS", unset = "600"))
q36_mnp_burnin <- as.integer(Sys.getenv("Q36_MNP_BURNIN", unset = "100"))
q36_mnp_seed <- as.integer(Sys.getenv("Q36_MNP_SEED", unset = "202505"))
q36_mnp_pred_draws <- as.integer(Sys.getenv("Q36_MNP_PRED_DRAWS", unset = "1"))
q36_mvprobit_n_ghk <- as.integer(Sys.getenv("Q36_MVPROBIT_NGHK", unset = "100"))
q36_mvprobit_iterlim <- as.integer(Sys.getenv("Q36_MVPROBIT_ITERLIM", unset = "80"))
q36_run_mvprobit <- tolower(Sys.getenv("Q36_RUN_MVPROBIT", unset = "false")) %chin% c("1", "true", "yes")


# Helper functions ----

empty_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

relevel_factor <- function(x, ref, var_name) {
  x <- factor(x)

  if (!ref %in% levels(x)) {
    stop("Reference level '", ref, "' not found in ", var_name, call. = FALSE)
  }

  relevel(x, ref = ref)
}

factor_with_expected_levels <- function(x, expected_levels, ref, var_name) {
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), expected_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in ", var_name, ": ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!ref %chin% x_chr) {
    stop("Reference level '", ref, "' not found in ", var_name, call. = FALSE)
  }

  factor(x_chr, levels = expected_levels)
}

relevel_income_factor <- function(x) {
  x_chr <- as.character(x)
  ref <- income_levels[1L]

  if (!ref %chin% x_chr) {
    stop("Reference level '", ref, "' not found in tranche_revenus", call. = FALSE)
  }

  present_levels <- income_levels[income_levels %chin% unique(x_chr)]
  factor(x_chr, levels = present_levels)
}

filter_valid_weights <- function(data) {
  if (!"weights" %chin% names(data)) {
    stop("Column 'weights' is missing from final_survey2025.csv.", call. = FALSE)
  }

  data[, weights := as.numeric(weights)]

  if (all(is.na(data$weights))) {
    stop("Column 'weights' is empty or non-numeric.", call. = FALSE)
  }

  data <- data[!is.na(weights) & weights > 0]

  if (nrow(data) == 0L) {
    stop("No observations with valid positive survey weights.", call. = FALSE)
  }

  data
}

add_scaled_age <- function(data) {
  age_mean <- weighted.mean(data$continuous_age, w = data$weights)
  age_sd <- sqrt(weighted.mean((data$continuous_age - age_mean)^2, w = data$weights))

  if (!is.finite(age_sd) || age_sd <= 0) {
    stop("continuous_age has no valid variation in the model sample.", call. = FALSE)
  }

  data[
    ,
    `:=`(
      continuous_age_scaled = (continuous_age - age_mean) / age_sd,
      continuous_age_scaled_sq = ((continuous_age - age_mean) / age_sd)^2
    )
  ]
  data
}

prepare_q36_long_sample <- function(path, option_labels) {
  q36_cols <- names(option_labels)
  covariates <- c(
    "respid", "weights", "treatment", "q35", "q30_q2", "q19", "q5",
    "pcs", "matri", "foyer", "continuous_age", "q23", "tailleagglo5",
    "tranche_revenus", "diplome", "partisane", "q28", "gender", "sat"
  )
  required_columns <- c(q36_cols, covariates)

  survey <- fread(path)[treatment == 0]
  missing_columns <- setdiff(required_columns, names(survey))

  if (length(missing_columns) > 0L) {
    stop(
      "Required columns missing from final_survey2025.csv: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  survey <- filter_valid_weights(survey)
  char_vars <- c(
    "q35", "q30_q2", "q19", "q5", "q23", "pcs", "matri", "foyer",
    "tailleagglo5", "tranche_revenus", "diplome", "partisane",
    "q28", "gender", "sat"
  )
  survey[, (char_vars) := lapply(.SD, empty_to_na), .SDcols = char_vars]
  survey <- survey[q5 %chin% c("Oui", "Non")]

  sample_vars <- c(
    q36_cols, "respid", "weights", "q35", "q30_q2", "q19", "q5",
    "pcs", "matri", "foyer", "continuous_age", "q23", "tailleagglo5",
    "tranche_revenus", "diplome", "partisane", "q28", "gender", "sat"
  )
  survey <- survey[complete.cases(survey[, ..sample_vars])]

  if (nrow(survey) == 0L) {
    stop("No complete-case control-group observations for the q36 GLMM sample.", call. = FALSE)
  }

  invalid_q36 <- q36_cols[
    vapply(
      q36_cols,
      function(col) any(!survey[[col]] %in% c(0L, 1L)),
      logical(1)
    )
  ]

  if (length(invalid_q36) > 0L) {
    stop("q36 columns must be coded 0/1: ", paste(invalid_q36, collapse = ", "), call. = FALSE)
  }

  survey[, q36_selection_count := rowSums(.SD == 1L, na.rm = TRUE), .SDcols = q36_cols]
  survey <- add_scaled_age(survey)
  survey[
    ,
    `:=`(
      respid = factor(respid),
      q30_q2 = factor_with_expected_levels(
        q30_q2,
        expected_levels = q30_levels,
        ref = q30_levels[1L],
        var_name = "q30_q2"
      ),
      q35 = factor_with_expected_levels(
        q35,
        expected_levels = q35_levels,
        ref = q35_levels[1L],
        var_name = "q35"
      ),
      q19 = relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19"),
      q5 = factor_with_expected_levels(q5, expected_levels = c("Non", "Oui"), ref = "Non", var_name = "q5"),
      q28 = factor_with_expected_levels(
        q28,
        expected_levels = satisfaction_levels,
        ref = satisfaction_levels[1L],
        var_name = "q28"
      ),
      gender = relevel_factor(gender, ref = "Femme", var_name = "gender"),
      acte_citoyen = relevel_factor(q23, ref = "Pas du tout d’accord", var_name = "acte_citoyen"),
      tranche_revenus = relevel_income_factor(tranche_revenus),
      sat = factor_with_expected_levels(
        sat,
        expected_levels = satisfaction_levels,
        ref = satisfaction_levels[1L],
        var_name = "sat"
      ),
      pcs = relevel_factor(pcs, ref = "Retraité", var_name = "pcs"),
      matri = factor(matri),
      foyer = factor(foyer),
      tailleagglo5 = factor(tailleagglo5),
      diplome = factor(diplome),
      partisane = relevel_factor(partisane, ref = "Aucun", var_name = "partisane")
    )
  ]

  q36_long <- melt(
    survey,
    id.vars = setdiff(names(survey), q36_cols),
    measure.vars = q36_cols,
    variable.name = "funding_option_raw",
    value.name = "selected"
  )
  q36_long[
    ,
    `:=`(
      selected = as.integer(selected),
      funding_option_raw = as.character(funding_option_raw),
      funding_option = factor(
        option_labels[as.character(funding_option_raw)],
        levels = unname(option_labels)
      )
    )
  ]

  q36_long
}

build_term_specs <- function(data) {
  income_ref <- levels(data$tranche_revenus)[1L]
  income_contrasts <- setdiff(levels(data$tranche_revenus), income_ref)

  list(
    list(
      type = "factor", var = "q30_q2", term = "Confiance dans l'État",
      ref = q30_levels[1L], ref_label = q30_levels[1L],
      contrasts = q30_levels[-1L], contrast_labels = q30_levels[-1L]
    ),
    list(
      type = "factor", var = "q35", term = "Risque de conflit",
      ref = q35_levels[1L], ref_label = "NSP",
      contrasts = q35_levels[-1L], contrast_labels = q35_levels[-1L]
    ),
    list(
      type = "factor", var = "q19", term = "Niveau d'imposition",
      ref = "Ni trop, ni pas assez élevés", ref_label = "Juste",
      contrasts = c("Trop élevés", "Pas assez élevés"),
      contrast_labels = c("Trop élevés", "Pas assez")
    ),
    list(
      type = "factor", var = "q5", term = "Impôt sur le revenu",
      ref = "Non", ref_label = "Non",
      contrasts = "Oui", contrast_labels = "Oui"
    ),
    list(
      type = "factor", var = "acte_citoyen", term = "Acte citoyen",
      ref = "Pas du tout d’accord", ref_label = "Pas du tout d'accord",
      contrasts = c("Plutôt pas d’accord", "Plutôt d’accord", "Tout à fait d’accord"),
      contrast_labels = c("Plutôt pas d'accord", "Plutôt d'accord", "Tout à fait d'accord")
    ),
    list(
      type = "factor", var = "q28", term = "Satisfaction argent public",
      ref = satisfaction_levels[1L], ref_label = satisfaction_levels[1L],
      contrasts = satisfaction_levels[-1L], contrast_labels = satisfaction_levels[-1L]
    ),
    list(
      type = "factor", var = "gender", term = "Genre",
      ref = "Femme", ref_label = "Femme",
      contrasts = "Homme", contrast_labels = "Homme"
    ),
    list(
      type = "continuous", var = "continuous_age_scaled", term = "Age",
      contrast_labels = "Par écart-type"
    ),
    list(
      type = "continuous", var = "continuous_age_scaled_sq",
      term = "Age\\textsuperscript{2}", contrast_labels = "Par unité standardisée"
    ),
    list(
      type = "factor", var = "tranche_revenus", term = "Revenus",
      ref = income_ref, ref_label = income_label_map[[income_ref]],
      contrasts = income_contrasts,
      contrast_labels = unname(income_label_map[income_contrasts])
    )
  )
}

fit_q36_option_model <- function(data) {
  focal_rhs <- paste(q36_focal_vars, collapse = " + ")
  control_rhs <- paste(q36_control_vars, collapse = " + ")

  glmer(
    as.formula(
      paste(
        "selected ~ funding_option * (", focal_rhs, ") +",
        control_rhs,
        "+ (1 | respid)"
      )
    ),
    data = data,
    weights = weights,
    family = binomial(link = "probit"),
    nAGQ = 0,
    control = glmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 100000)
    )
  )
}

fit_q36_clustered_binary_model <- function(data) {
  focal_rhs <- paste(q36_focal_vars, collapse = " + ")
  control_rhs <- paste(q36_control_vars, collapse = " + ")

  feglm(
    as.formula(
      paste(
        "selected ~ funding_option * (", focal_rhs, ") +",
        control_rhs
      )
    ),
    data = data,
    weights = ~ weights,
    family = binomial(link = "probit"),
    vcov = ~ respid
  )
}

fit_q36_mnp_model <- function(data) {
  model_formula <- reformulate(c(q36_focal_vars, q36_control_vars), response = "q36_choice")
  retry_seeds <- q36_mnp_seed + 0:4
  last_error <- NULL

  for (seed in retry_seeds) {
    set.seed(seed)
    model <- tryCatch(
      MNP::mnp(
        model_formula,
        data = as.data.frame(data),
        base = names(q36_option_labels)[1L],
        n.draws = q36_mnp_draws,
        burnin = q36_mnp_burnin,
        thin = 0,
        trace = FALSE,
        verbose = FALSE
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(model)) {
      model$call$formula <- model_formula
      model$q36_seed <- seed
      return(model)
    }

    warning(
      "MNP sampler failed with seed ",
      seed,
      "; retrying with the next deterministic seed.",
      call. = FALSE
    )
  }

  stop(
    "MNP sampler failed for all deterministic retry seeds. Last error: ",
    conditionMessage(last_error),
    call. = FALSE
  )
}

fit_q36_mvprobit_model <- function(data) {
  model_formula <- as.formula(
    paste(
      "cbind(",
      paste(names(q36_option_labels), collapse = ", "),
      ") ~ ",
      paste(c(q36_focal_vars, q36_control_vars), collapse = " + ")
    )
  )

  model <- mvProbit::mvProbit(
    model_formula,
    data = as.data.frame(data),
    algorithm = "GHK",
    nGHK = q36_mvprobit_n_ghk,
    random.seed = 202505,
    printLevel = 0,
    iterlim = q36_mvprobit_iterlim
  )
  model$call$formula <- model_formula
  model$q36_formula <- model_formula
  model
}

fixed_effect_matrix <- function(model, newdata) {
  fixed_terms <- delete.response(terms(nobars(formula(model))))
  x_mat <- model.matrix(
    fixed_terms,
    data = newdata,
    contrasts.arg = attr(getME(model, "X"), "contrasts")
  )
  beta_names <- names(fixef(model))
  missing_cols <- setdiff(beta_names, colnames(x_mat))

  if (length(missing_cols) > 0L) {
    x_mat <- cbind(
      x_mat,
      matrix(0, nrow = nrow(x_mat), ncol = length(missing_cols), dimnames = list(NULL, missing_cols))
    )
  }

  x_mat[, beta_names, drop = FALSE]
}

predict_fixed <- function(model, newdata) {
  x_mat <- fixed_effect_matrix(model, newdata)
  eta <- as.vector(x_mat %*% fixef(model))

  list(
    x = x_mat,
    eta = eta,
    predicted = pnorm(eta)
  )
}

fixed_effect_matrix_feglm <- function(model, newdata) {
  fixed_terms <- delete.response(terms(formula(model)))
  x_mat <- model.matrix(fixed_terms, data = newdata)
  beta_names <- names(coef(model))
  missing_cols <- setdiff(beta_names, colnames(x_mat))

  if (length(missing_cols) > 0L) {
    x_mat <- cbind(
      x_mat,
      matrix(0, nrow = nrow(x_mat), ncol = length(missing_cols), dimnames = list(NULL, missing_cols))
    )
  }

  x_mat[, beta_names, drop = FALSE]
}

predict_feglm_fixed <- function(model, newdata) {
  x_mat <- fixed_effect_matrix_feglm(model, newdata)
  eta <- as.vector(x_mat %*% coef(model))

  list(
    x = x_mat,
    eta = eta,
    predicted = pnorm(eta)
  )
}

format_cell <- function(estimate, std_error, p_value, html = FALSE) {
  if (!is.finite(estimate)) {
    return("")
  }

  if (abs(estimate) < 0.0005) {
    estimate <- 0
  }
  if (!is.finite(std_error)) {
    if (html) {
      return(sprintf("%.3f", estimate))
    }
    return(sprintf("\\num{%.3f}", estimate))
  }
  if (abs(std_error) < 0.0005) {
    std_error <- 0
  }

  stars <- if (is.na(p_value)) {
    ""
  } else if (p_value < 0.001) {
    "***"
  } else if (p_value < 0.01) {
    "**"
  } else if (p_value < 0.05) {
    "*"
  } else if (p_value < 0.1) {
    "+"
  } else {
    ""
  }

  if (html) {
    return(sprintf("%.3f%s (%.3f)", estimate, stars, std_error))
  }

  sprintf("\\num{%.3f}%s (\\num{%.3f})", estimate, stars, std_error)
}

compute_factor_ame <- function(model, data, var, ref, level) {
  option_data <- copy(data)
  lo <- copy(option_data)
  hi <- copy(option_data)
  lo[, (var) := factor(ref, levels = levels(option_data[[var]]))]
  hi[, (var) := factor(level, levels = levels(option_data[[var]]))]

  pred_lo <- predict_fixed(model, lo)
  pred_hi <- predict_fixed(model, hi)
  weights_norm <- option_data$weights / sum(option_data$weights)
  estimate <- sum(weights_norm * (pred_hi$predicted - pred_lo$predicted))
  gradient <- colSums(weights_norm * (dnorm(pred_hi$eta) * pred_hi$x - dnorm(pred_lo$eta) * pred_lo$x))

  list(estimate = estimate, gradient = gradient)
}

compute_continuous_ame <- function(model, data, var, eps = 1e-5) {
  option_data <- copy(data)
  lo <- copy(option_data)
  hi <- copy(option_data)
  hi[, (var) := get(var) + eps]

  pred_lo <- predict_fixed(model, lo)
  pred_hi <- predict_fixed(model, hi)
  weights_norm <- option_data$weights / sum(option_data$weights)
  estimate <- sum(weights_norm * (pred_hi$predicted - pred_lo$predicted)) / eps
  gradient <- colSums(
    weights_norm * (dnorm(pred_hi$eta) * pred_hi$x - dnorm(pred_lo$eta) * pred_lo$x)
  ) / eps

  list(estimate = estimate, gradient = gradient)
}

add_inference <- function(estimate, gradient, vcov_mat) {
  std_error <- sqrt(as.numeric(t(gradient) %*% vcov_mat %*% gradient))
  z_value <- estimate / std_error
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)

  list(
    estimate = estimate,
    std_error = std_error,
    p_value = p_value
  )
}

compute_option_ames <- function(model, data, term_specs) {
  vcov_mat <- as.matrix(vcov(model))
  beta_names <- names(fixef(model))
  vcov_mat <- vcov_mat[beta_names, beta_names, drop = FALSE]
  option_levels <- levels(data$funding_option)

  results <- list()
  row_id <- 0L

  for (option_label in option_levels) {
    option_data <- data[funding_option == option_label]

    for (spec in term_specs) {
      if (identical(spec$type, "factor")) {
        for (i in seq_along(spec$contrasts)) {
          ame <- compute_factor_ame(
            model = model,
            data = option_data,
            var = spec$var,
            ref = spec$ref,
            level = spec$contrasts[[i]]
          )
          inference <- add_inference(ame$estimate, ame$gradient, vcov_mat)
          row_id <- row_id + 1L
          results[[row_id]] <- data.table(
            term = spec$term,
            contrast = spec$contrast_labels[[i]],
            option = option_label,
            estimate = inference$estimate,
            std_error = inference$std_error,
            p_value = inference$p_value
          )
        }
      } else {
        ame <- compute_continuous_ame(model = model, data = option_data, var = spec$var)
        inference <- add_inference(ame$estimate, ame$gradient, vcov_mat)
        row_id <- row_id + 1L
        results[[row_id]] <- data.table(
          term = spec$term,
          contrast = spec$contrast_labels,
          option = option_label,
          estimate = inference$estimate,
          std_error = inference$std_error,
          p_value = inference$p_value
        )
      }
    }
  }

  rbindlist(results)
}

compute_factor_ame_feglm <- function(model, data, var, ref, level) {
  option_data <- copy(data)
  lo <- copy(option_data)
  hi <- copy(option_data)
  lo[, (var) := factor(ref, levels = levels(option_data[[var]]))]
  hi[, (var) := factor(level, levels = levels(option_data[[var]]))]

  pred_lo <- predict_feglm_fixed(model, lo)
  pred_hi <- predict_feglm_fixed(model, hi)
  weights_norm <- option_data$weights / sum(option_data$weights)
  estimate <- sum(weights_norm * (pred_hi$predicted - pred_lo$predicted))
  gradient <- colSums(weights_norm * (dnorm(pred_hi$eta) * pred_hi$x - dnorm(pred_lo$eta) * pred_lo$x))

  list(estimate = estimate, gradient = gradient)
}

compute_continuous_ame_feglm <- function(model, data, var, eps = 1e-5) {
  option_data <- copy(data)
  lo <- copy(option_data)
  hi <- copy(option_data)
  hi[, (var) := get(var) + eps]

  pred_lo <- predict_feglm_fixed(model, lo)
  pred_hi <- predict_feglm_fixed(model, hi)
  weights_norm <- option_data$weights / sum(option_data$weights)
  estimate <- sum(weights_norm * (pred_hi$predicted - pred_lo$predicted)) / eps
  gradient <- colSums(
    weights_norm * (dnorm(pred_hi$eta) * pred_hi$x - dnorm(pred_lo$eta) * pred_lo$x)
  ) / eps

  list(estimate = estimate, gradient = gradient)
}

compute_option_ames_feglm <- function(model, data, term_specs) {
  vcov_mat <- as.matrix(vcov(model))
  beta_names <- names(coef(model))
  vcov_mat <- vcov_mat[beta_names, beta_names, drop = FALSE]
  option_levels <- levels(data$funding_option)

  results <- list()
  row_id <- 0L

  for (option_label in option_levels) {
    option_data <- data[funding_option == option_label]

    for (spec in term_specs) {
      if (identical(spec$type, "factor")) {
        for (i in seq_along(spec$contrasts)) {
          ame <- compute_factor_ame_feglm(
            model = model,
            data = option_data,
            var = spec$var,
            ref = spec$ref,
            level = spec$contrasts[[i]]
          )
          inference <- add_inference(ame$estimate, ame$gradient, vcov_mat)
          row_id <- row_id + 1L
          results[[row_id]] <- data.table(
            term = spec$term,
            contrast = spec$contrast_labels[[i]],
            option = option_label,
            estimate = inference$estimate,
            std_error = inference$std_error,
            p_value = inference$p_value
          )
        }
      } else {
        ame <- compute_continuous_ame_feglm(model = model, data = option_data, var = spec$var)
        inference <- add_inference(ame$estimate, ame$gradient, vcov_mat)
        row_id <- row_id + 1L
        results[[row_id]] <- data.table(
          term = spec$term,
          contrast = spec$contrast_labels,
          option = option_label,
          estimate = inference$estimate,
          std_error = inference$std_error,
          p_value = inference$p_value
        )
      }
    }
  }

  rbindlist(results)
}

prepare_q36_respondent_sample <- function(q36_long) {
  q36_cols <- names(q36_option_labels)
  respondent_vars <- setdiff(names(q36_long), c("funding_option_raw", "funding_option", "selected"))
  respondents <- unique(q36_long[, ..respondent_vars], by = "respid")
  q36_wide <- dcast(
    q36_long[, .(respid, funding_option_raw, selected)],
    respid ~ funding_option_raw,
    value.var = "selected"
  )
  respondents <- merge(respondents, q36_wide, by = "respid")

  missing_q36_cols <- setdiff(q36_cols, names(respondents))
  if (length(missing_q36_cols) > 0L) {
    stop("Missing q36 option columns in respondent-level sample: ", paste(missing_q36_cols, collapse = ", "), call. = FALSE)
  }

  respondents
}

prepare_q36_single_choice_sample <- function(q36_respondents) {
  q36_cols <- names(q36_option_labels)
  single <- copy(q36_respondents)[q36_selection_count == 1L]

  if (nrow(single) == 0L) {
    stop("No respondents selected exactly one q36 option.", call. = FALSE)
  }

  single[
    ,
    q36_choice := factor(
      q36_cols[max.col(as.matrix(.SD == 1L))],
      levels = q36_cols
    ),
    .SDcols = q36_cols
  ]

  single
}

mnp_predict_prob <- function(model, newdata) {
  pred <- predict(
    model,
    newdata = as.data.frame(newdata),
    newdraw = model$param,
    n.draws = q36_mnp_pred_draws,
    type = "prob",
    verbose = FALSE
  )$p

  if (length(dim(pred)) == 2L) {
    pred <- array(pred, dim = c(dim(pred), 1L), dimnames = c(dimnames(pred), list("1")))
  }

  pred
}

weighted_option_draws <- function(prob_array, weights) {
  weights_norm <- weights / sum(weights)
  apply(
    prob_array,
    3L,
    function(prob_mat) colSums(weights_norm * prob_mat)
  )
}

compute_factor_ame_mnp <- function(model, data, var, ref, level) {
  lo <- copy(data)
  hi <- copy(data)
  lo[, (var) := factor(ref, levels = levels(data[[var]]))]
  hi[, (var) := factor(level, levels = levels(data[[var]]))]

  lo_draws <- weighted_option_draws(mnp_predict_prob(model, lo), lo$weights)
  hi_draws <- weighted_option_draws(mnp_predict_prob(model, hi), hi$weights)
  hi_draws - lo_draws
}

compute_continuous_ame_mnp <- function(model, data, var, eps = 1) {
  lo <- copy(data)
  hi <- copy(data)
  hi[, (var) := get(var) + eps]

  lo_draws <- weighted_option_draws(mnp_predict_prob(model, lo), lo$weights)
  hi_draws <- weighted_option_draws(mnp_predict_prob(model, hi), hi$weights)
  (hi_draws - lo_draws) / eps
}

compute_option_ames_mnp <- function(model, data, term_specs) {
  results <- list()
  row_id <- 0L
  option_raw <- rownames(weighted_option_draws(mnp_predict_prob(model, data[1L]), data[1L, weights]))

  for (spec in term_specs) {
    if (identical(spec$type, "factor")) {
      for (i in seq_along(spec$contrasts)) {
        draws <- compute_factor_ame_mnp(
          model = model,
          data = data,
          var = spec$var,
          ref = spec$ref,
          level = spec$contrasts[[i]]
        )
        for (option_idx in seq_along(option_raw)) {
          option_draws <- draws[option_idx, ]
          std_error <- sd(option_draws)
          p_value <- if (is.finite(std_error) && std_error > 0) {
            2 * pnorm(abs(mean(option_draws) / std_error), lower.tail = FALSE)
          } else {
            NA_real_
          }
          row_id <- row_id + 1L
          results[[row_id]] <- data.table(
            term = spec$term,
            contrast = spec$contrast_labels[[i]],
            option = unname(q36_option_labels[[option_raw[[option_idx]]]]),
            estimate = mean(option_draws),
            std_error = std_error,
            p_value = p_value
          )
        }
      }
    } else {
      draws <- compute_continuous_ame_mnp(model = model, data = data, var = spec$var)
      for (option_idx in seq_along(option_raw)) {
        option_draws <- draws[option_idx, ]
        std_error <- sd(option_draws)
        p_value <- if (is.finite(std_error) && std_error > 0) {
          2 * pnorm(abs(mean(option_draws) / std_error), lower.tail = FALSE)
        } else {
          NA_real_
        }
        row_id <- row_id + 1L
        results[[row_id]] <- data.table(
          term = spec$term,
          contrast = spec$contrast_labels,
          option = unname(q36_option_labels[[option_raw[[option_idx]]]]),
          estimate = mean(option_draws),
          std_error = std_error,
          p_value = p_value
        )
      }
    }
  }

  rbindlist(results)
}

extract_mvprobit_sigma <- function(model) {
  n_dep <- model$nDep
  sigma <- diag(n_dep)
  coef_vec <- coef(model)
  corr_terms <- coef_vec[grep("^R_[0-9]+_[0-9]+$", names(coef_vec))]

  for (term_name in names(corr_terms)) {
    idx <- as.integer(strsplit(sub("^R_", "", term_name), "_", fixed = TRUE)[[1L]])
    sigma[idx[1L], idx[2L]] <- corr_terms[[term_name]]
    sigma[idx[2L], idx[1L]] <- corr_terms[[term_name]]
  }

  dimnames(sigma) <- list(unname(q36_option_short_labels), unname(q36_option_short_labels))
  sigma
}

compute_mvprobit_ames <- function(model, data, term_specs) {
  n_dep <- model$nDep
  n_reg <- model$nReg
  model_formula <- if (!is.null(model$q36_formula)) {
    model$q36_formula
  } else {
    eval(model$call$formula)
  }
  coef_vec <- coef(model)
  beta <- matrix(coef_vec[seq_len(n_dep * n_reg)], nrow = n_reg, ncol = n_dep)
  x_mat <- model.matrix(delete.response(terms(model_formula)), data = data)
  x_mat <- x_mat[, seq_len(n_reg), drop = FALSE]
  option_levels <- unname(q36_option_labels)

  predict_probs <- function(newdata) {
    new_x <- model.matrix(delete.response(terms(model_formula)), data = newdata)
    new_x <- new_x[, seq_len(n_reg), drop = FALSE]
    pnorm(new_x %*% beta)
  }

  results <- list()
  row_id <- 0L

  for (spec in term_specs) {
    if (identical(spec$type, "factor")) {
      for (i in seq_along(spec$contrasts)) {
        lo <- copy(data)
        hi <- copy(data)
        lo[, (spec$var) := factor(spec$ref, levels = levels(data[[spec$var]]))]
        hi[, (spec$var) := factor(spec$contrasts[[i]], levels = levels(data[[spec$var]]))]
        diff_mat <- predict_probs(hi) - predict_probs(lo)
        estimates <- colSums((data$weights / sum(data$weights)) * diff_mat)

        for (option_idx in seq_along(option_levels)) {
          row_id <- row_id + 1L
          results[[row_id]] <- data.table(
            term = spec$term,
            contrast = spec$contrast_labels[[i]],
            option = option_levels[[option_idx]],
            estimate = estimates[[option_idx]],
            std_error = NA_real_,
            p_value = NA_real_
          )
        }
      }
    } else {
      lo <- copy(data)
      hi <- copy(data)
      hi[, (spec$var) := get(spec$var) + 1e-5]
      diff_mat <- (predict_probs(hi) - predict_probs(lo)) / 1e-5
      estimates <- colSums((data$weights / sum(data$weights)) * diff_mat)

      for (option_idx in seq_along(option_levels)) {
        row_id <- row_id + 1L
        results[[row_id]] <- data.table(
          term = spec$term,
          contrast = spec$contrast_labels,
          option = option_levels[[option_idx]],
          estimate = estimates[[option_idx]],
          std_error = NA_real_,
          p_value = NA_real_
        )
      }
    }
  }

  results <- rbindlist(results)
  attr(results, "sigma") <- extract_mvprobit_sigma(model)
  results
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

write_option_ame_table <- function(path_tex,
                                   path_html,
                                   ames,
                                   term_specs,
                                   option_labels,
                                   nobs_model,
                                   respondents_model,
                                   selection_count_table,
                                   singular_fit) {
  option_levels <- unname(q36_option_labels)
  option_headers <- unname(option_labels[names(q36_option_labels)])

  lookup_cell <- function(term_label, contrast_label, option_label, html = FALSE) {
    row <- ames[term == term_label & contrast == contrast_label & option == option_label]
    if (nrow(row) == 0L) {
      return("")
    }

    format_cell(row$estimate[[1L]], row$std_error[[1L]], row$p_value[[1L]], html = html)
  }

  add_row <- function(term, contrast, cells) {
    paste(c(term, contrast, cells), collapse = " & ")
  }

  body <- character()
  html_rows <- data.table(
    term = character(),
    contrast = character(),
    q36_q1 = character(),
    q36_q2 = character(),
    q36_q3 = character(),
    q36_q4 = character(),
    section_start = logical()
  )

  add_html_row <- function(term, contrast, cells, section_start = FALSE) {
    html_rows <<- rbind(
      html_rows,
      data.table(
        term = term,
        contrast = contrast,
        q36_q1 = cells[[1L]],
        q36_q2 = cells[[2L]],
        q36_q3 = cells[[3L]],
        q36_q4 = cells[[4L]],
        section_start = section_start
      )
    )
  }

  for (spec_idx in seq_along(term_specs)) {
    spec <- term_specs[[spec_idx]]
    if (spec_idx > 1L) {
      body <- c(body, "\\midrule")
    }

    if (identical(spec$type, "factor")) {
      ref_cells <- rep("\\textit{Ref}", length(option_levels))
      body <- c(body, add_row(paste0("\\textbf{", spec$term, "}"), spec$ref_label, ref_cells))
      add_html_row(spec$term, spec$ref_label, rep("Ref", length(option_levels)), section_start = spec_idx > 1L)

      for (contrast_label in spec$contrast_labels) {
        cells <- vapply(
          option_levels,
          lookup_cell,
          character(1),
          term_label = spec$term,
          contrast_label = contrast_label
        )
        html_cells <- vapply(
          option_levels,
          lookup_cell,
          character(1),
          term_label = spec$term,
          contrast_label = contrast_label,
          html = TRUE
        )
        body <- c(body, add_row("", contrast_label, cells))
        add_html_row("", contrast_label, html_cells)
      }
    } else {
      cells <- vapply(
        option_levels,
        lookup_cell,
        character(1),
        term_label = spec$term,
        contrast_label = spec$contrast_labels
      )
      html_cells <- vapply(
        option_levels,
        lookup_cell,
        character(1),
        term_label = spec$term,
        contrast_label = spec$contrast_labels,
        html = TRUE
      )
      body <- c(body, add_row(paste0("\\textbf{", spec$term, "}"), spec$contrast_labels, cells))
      add_html_row(spec$term, spec$contrast_labels, html_cells, section_start = spec_idx > 1L)
    }
  }

  multiple_selection_n <- selection_count_table[q36_selection_count > 1L, sum(N)]
  zero_selection_n <- selection_count_table[q36_selection_count == 0L, sum(N)]
  footer_rows <- c(
    "\\midrule\\midrule",
    add_row("Observations", "répondant-option", rep(as.character(nobs_model), 4L)),
    add_row("Répondants", "", rep(as.character(respondents_model), 4L)),
    add_row("Répondants multi-réponses", "", rep(as.character(multiple_selection_n), 4L)),
    add_row("Répondants sans q36", "", rep(as.character(zero_selection_n), 4L)),
    add_row("Contrôles additionnels", "", rep("Oui", 4L)),
    add_row("Intercept aléatoire", "", rep("Répondant", 4L)),
    add_row("Ajustement singulier", "", rep(ifelse(singular_fit, "Oui", "Non"), 4L))
  )
  body <- c(body, footer_rows)
  body <- paste0(body, " \\\\")
  body[body == "\\midrule \\\\"] <- "\\midrule"
  body[body == "\\midrule\\midrule \\\\"] <- "\\midrule\\midrule"

  note_text <- paste(
    "\\textbf{Note:} Modèle probit mixte estimé sur un format long répondant-option des quatre choix de financement q36.",
    "La variable dépendante vaut 1 si l'option q36 est cochée ; les réponses multiples sont conservées.",
    "Chaque cellule présente un effet marginal moyen propre à l'option, calculé avec les effets aléatoires fixés à zéro.",
    "Le modèle interagit l'option q36 avec les variables affichées et inclut aussi la catégorie socio-professionnelle,",
    "la situation matrimoniale, le foyer, la taille d'agglomération, le diplôme, la proximité partisane et la satisfaction générale comme contrôles.",
    "Les poids sont utilisés comme poids de vraisemblance dans lme4, non comme correction complète de plan de sondage.",
    "L'estimation utilise l'approximation rapide nAGQ = 0 de glmer.",
    "Age et age au carré sont standardisés.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )

  tex_lines <- c(
    "\\begin{table*}[!t]",
    "\\centering",
    "\\scriptsize \\selectfont",
    "\\setlength{\\tabcolsep}{2pt}",
    "",
    "\\begin{talltblr}[",
    "caption={Déterminants des préférences de financement de la défense},",
    "label={tab:defense_tax_pref_q36_glmm},",
    paste0("note{}={", note_text, "},"),
    "]{",
    "width=0.99\\textwidth,",
    "\\rowsep = 0pt,",
    "colspec={",
    "Q[l,wd=0.17\\textwidth]",
    "Q[l,wd=0.18\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "},",
    "column{3,4,5,6}={halign=c},",
    "column{1,2}={halign=l},",
    "row{1}={font=\\bfseries},",
    "}",
    "\\toprule",
    paste0("& & ", paste(option_headers, collapse = " & "), " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{talltblr}",
    "\\end{table*}"
  )
  writeLines(tex_lines, path_tex, useBytes = TRUE)
  message("Table saved to: ", path_tex)

  add_html_row("Observations", "répondant-option", rep(as.character(nobs_model), 4L), section_start = TRUE)
  add_html_row("Répondants", "", rep(as.character(respondents_model), 4L))
  add_html_row("Répondants multi-réponses", "", rep(as.character(multiple_selection_n), 4L))
  add_html_row("Répondants sans q36", "", rep(as.character(zero_selection_n), 4L))
  add_html_row("Contrôles additionnels", "", rep("Oui", 4L))
  add_html_row("Intercept aléatoire", "", rep("Répondant", 4L))
  add_html_row("Ajustement singulier", "", rep(ifelse(singular_fit, "Oui", "Non"), 4L))

  render_html_row <- function(i) {
    row <- html_rows[i]
    cls <- if (row$section_start) " class=\"section\"" else ""
    term_label <- html_escape(row$term)
    term_label <- gsub("Age\\\\textsuperscript\\{2\\}", "Age<sup>2</sup>", term_label)
    term <- if (nzchar(row$term)) paste0("<strong>", term_label, "</strong>") else ""
    paste0(
      "<tr", cls, ">",
      "<td>", term, "</td>",
      "<td>", html_escape(row$contrast), "</td>",
      "<td>", html_escape(row$q36_q1), "</td>",
      "<td>", html_escape(row$q36_q2), "</td>",
      "<td>", html_escape(row$q36_q3), "</td>",
      "<td>", html_escape(row$q36_q4), "</td>",
      "</tr>"
    )
  }

  html_note <- paste(
    "Note: Modèle probit mixte estimé sur un format long répondant-option des quatre choix de financement q36.",
    "La variable dépendante vaut 1 si l'option q36 est cochée ; les réponses multiples sont conservées.",
    "Chaque cellule présente un effet marginal moyen propre à l'option, calculé avec les effets aléatoires fixés à zéro.",
    "Le modèle interagit l'option q36 avec les variables affichées et inclut aussi la catégorie socio-professionnelle,",
    "la situation matrimoniale, le foyer, la taille d'agglomération, le diplôme, la proximité partisane et la satisfaction générale comme contrôles.",
    "Les poids sont utilisés comme poids de vraisemblance dans lme4, non comme correction complète de plan de sondage.",
    "L'estimation utilise l'approximation rapide nAGQ = 0 de glmer.",
    "Age et age au carré sont standardisés.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
  html_lines <- c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<title>Déterminants des préférences de financement de la défense</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:24px;color:#111827;background:#fff;}",
    "table{border-collapse:collapse;width:100%;font-size:13px;}",
    "caption{caption-side:top;text-align:left;font-weight:700;margin-bottom:8px;}",
    "th,td{border-bottom:1px solid #e5e7eb;padding:5px 7px;text-align:left;vertical-align:top;}",
    "th:nth-child(n+3),td:nth-child(n+3){text-align:center;}",
    "tr.section td{border-top:2px solid #9ca3af;}",
    "tfoot td{border-top:2px solid #111827;font-size:12px;color:#374151;text-align:left;}",
    "</style>",
    "</head>",
    "<body>",
    "<table>",
    "<caption>Déterminants des préférences de financement de la défense</caption>",
    "<thead>",
    paste0(
      "<tr><th>Variable</th><th>Modalité</th>",
      paste0("<th>", html_escape(option_headers), "</th>", collapse = ""),
      "</tr>"
    ),
    "</thead>",
    "<tbody>",
    vapply(seq_len(nrow(html_rows)), render_html_row, character(1)),
    "</tbody>",
    "<tfoot>",
    paste0("<tr><td colspan=\"6\">", html_escape(html_note), "</td></tr>"),
    "</tfoot>",
    "</table>",
    "</body>",
    "</html>"
  )
  writeLines(html_lines, path_html, useBytes = TRUE)
  message("Table saved to: ", path_html)
}

write_companion_option_ame_table <- function(path_tex,
                                             path_html,
                                             ames,
                                             term_specs,
                                             option_labels,
                                             footer_rows,
                                             caption,
                                             label,
                                             note_text,
                                             html_note = note_text) {
  option_levels <- unname(q36_option_labels)
  option_headers <- unname(option_labels[names(q36_option_labels)])

  lookup_cell <- function(term_label, contrast_label, option_label, html = FALSE) {
    row <- ames[term == term_label & contrast == contrast_label & option == option_label]
    if (nrow(row) == 0L) {
      return("")
    }

    format_cell(row$estimate[[1L]], row$std_error[[1L]], row$p_value[[1L]], html = html)
  }

  add_row <- function(term, contrast, cells) {
    paste(c(term, contrast, cells), collapse = " & ")
  }

  body <- character()
  html_rows <- data.table(
    term = character(),
    contrast = character(),
    q36_q1 = character(),
    q36_q2 = character(),
    q36_q3 = character(),
    q36_q4 = character(),
    section_start = logical()
  )

  add_html_row <- function(term, contrast, cells, section_start = FALSE) {
    html_rows <<- rbind(
      html_rows,
      data.table(
        term = term,
        contrast = contrast,
        q36_q1 = cells[[1L]],
        q36_q2 = cells[[2L]],
        q36_q3 = cells[[3L]],
        q36_q4 = cells[[4L]],
        section_start = section_start
      )
    )
  }

  for (spec_idx in seq_along(term_specs)) {
    spec <- term_specs[[spec_idx]]
    if (spec_idx > 1L) {
      body <- c(body, "\\midrule")
    }

    if (identical(spec$type, "factor")) {
      ref_cells <- rep("\\textit{Ref}", length(option_levels))
      body <- c(body, add_row(paste0("\\textbf{", spec$term, "}"), spec$ref_label, ref_cells))
      add_html_row(spec$term, spec$ref_label, rep("Ref", length(option_levels)), section_start = spec_idx > 1L)

      for (contrast_label in spec$contrast_labels) {
        cells <- vapply(
          option_levels,
          lookup_cell,
          character(1),
          term_label = spec$term,
          contrast_label = contrast_label
        )
        html_cells <- vapply(
          option_levels,
          lookup_cell,
          character(1),
          term_label = spec$term,
          contrast_label = contrast_label,
          html = TRUE
        )
        body <- c(body, add_row("", contrast_label, cells))
        add_html_row("", contrast_label, html_cells)
      }
    } else {
      cells <- vapply(
        option_levels,
        lookup_cell,
        character(1),
        term_label = spec$term,
        contrast_label = spec$contrast_labels
      )
      html_cells <- vapply(
        option_levels,
        lookup_cell,
        character(1),
        term_label = spec$term,
        contrast_label = spec$contrast_labels,
        html = TRUE
      )
      body <- c(body, add_row(paste0("\\textbf{", spec$term, "}"), spec$contrast_labels, cells))
      add_html_row(spec$term, spec$contrast_labels, html_cells, section_start = spec_idx > 1L)
    }
  }

  if (length(footer_rows) > 0L) {
    body <- c(body, "\\midrule\\midrule")
    for (footer_idx in seq_along(footer_rows)) {
      row <- footer_rows[[footer_idx]]
      html_cells <- if (!is.null(row$html_cells)) row$html_cells else row$cells
      body <- c(body, add_row(row$term, row$contrast, row$cells))
      add_html_row(row$term, row$contrast, html_cells, section_start = footer_idx == 1L)
    }
  }

  body <- paste0(body, " \\\\")
  body[body == "\\midrule \\\\"] <- "\\midrule"
  body[body == "\\midrule\\midrule \\\\"] <- "\\midrule\\midrule"

  tex_lines <- c(
    "\\begin{table*}[!t]",
    "\\centering",
    "\\scriptsize \\selectfont",
    "\\setlength{\\tabcolsep}{2pt}",
    "",
    "\\begin{talltblr}[",
    paste0("caption={", caption, "},"),
    paste0("label={", label, "},"),
    paste0("note{}={", note_text, "},"),
    "]{",
    "width=0.99\\textwidth,",
    "\\rowsep = 0pt,",
    "colspec={",
    "Q[l,wd=0.17\\textwidth]",
    "Q[l,wd=0.18\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "},",
    "column{3,4,5,6}={halign=c},",
    "column{1,2}={halign=l},",
    "row{1}={font=\\bfseries},",
    "}",
    "\\toprule",
    paste0("& & ", paste(option_headers, collapse = " & "), " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{talltblr}",
    "\\end{table*}"
  )
  writeLines(tex_lines, path_tex, useBytes = TRUE)
  message("Table saved to: ", path_tex)

  render_html_row <- function(i) {
    row <- html_rows[i]
    cls <- if (row$section_start) " class=\"section\"" else ""
    term_label <- html_escape(row$term)
    term_label <- gsub("Age\\\\textsuperscript\\{2\\}", "Age<sup>2</sup>", term_label)
    term <- if (nzchar(row$term)) paste0("<strong>", term_label, "</strong>") else ""
    paste0(
      "<tr", cls, ">",
      "<td>", term, "</td>",
      "<td>", html_escape(row$contrast), "</td>",
      "<td>", html_escape(row$q36_q1), "</td>",
      "<td>", html_escape(row$q36_q2), "</td>",
      "<td>", html_escape(row$q36_q3), "</td>",
      "<td>", html_escape(row$q36_q4), "</td>",
      "</tr>"
    )
  }

  html_lines <- c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    paste0("<title>", html_escape(caption), "</title>"),
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:24px;color:#111827;background:#fff;}",
    "table{border-collapse:collapse;width:100%;font-size:13px;}",
    "caption{caption-side:top;text-align:left;font-weight:700;margin-bottom:8px;}",
    "th,td{border-bottom:1px solid #e5e7eb;padding:5px 7px;text-align:left;vertical-align:top;}",
    "th:nth-child(n+3),td:nth-child(n+3){text-align:center;}",
    "tr.section td{border-top:2px solid #9ca3af;}",
    "tfoot td{border-top:2px solid #111827;font-size:12px;color:#374151;text-align:left;}",
    "</style>",
    "</head>",
    "<body>",
    "<table>",
    paste0("<caption>", html_escape(caption), "</caption>"),
    "<thead>",
    paste0(
      "<tr><th>Variable</th><th>Modalité</th>",
      paste0("<th>", html_escape(option_headers), "</th>", collapse = ""),
      "</tr>"
    ),
    "</thead>",
    "<tbody>",
    vapply(seq_len(nrow(html_rows)), render_html_row, character(1)),
    "</tbody>",
    "<tfoot>",
    paste0("<tr><td colspan=\"6\">", html_escape(html_note), "</td></tr>"),
    "</tfoot>",
    "</table>",
    "</body>",
    "</html>"
  )
  writeLines(html_lines, path_html, useBytes = TRUE)
  message("Table saved to: ", path_html)
}


# Data preparation ----

q36_long <- prepare_q36_long_sample(
  path = file.path(data_final_dir, "final_survey2025.csv"),
  option_labels = q36_option_labels
)

selection_counts <- q36_long[
  ,
  .(
    selected = sum(selected),
    rows = .N,
    weighted_share = weighted.mean(selected, w = weights)
  ),
  by = .(funding_option_raw, funding_option)
][order(funding_option_raw)]

respondent_selection_counts <- unique(q36_long[, .(respid, q36_selection_count)])[
  ,
  .N,
  by = q36_selection_count
][order(q36_selection_count)]
q36_respondents <- prepare_q36_respondent_sample(q36_long)
q36_single_choice <- prepare_q36_single_choice_sample(q36_respondents)

message("Model sample respondents: ", uniqueN(q36_long$respid))
message("Model sample respondent-option rows: ", nrow(q36_long))
message("Single-answer respondents: ", nrow(q36_single_choice))
message("Q36 selections per respondent:")
print(respondent_selection_counts)
message("Weighted q36 selection rates:")
print(selection_counts)


# Model and AMEs ----

# selected_ij = 1[q36 option j selected by respondent i]
# Pr(selected_ij = 1) = Phi(alpha_j + X_i beta_j + Z_i gamma + u_i)
# where u_i is a respondent random intercept. X_i variables are interacted
# with the q36 option so the displayed AMEs are option-specific.
q36_model <- fit_q36_option_model(q36_long)
term_specs <- build_term_specs(q36_long)
option_ames <- compute_option_ames(q36_model, q36_long, term_specs)
singular_fit <- isSingular(q36_model)

if (singular_fit) {
  message("Singular GLMM fit detected. Interpret the respondent random-effect variance cautiously.")
}

# Among respondents who selected exactly one q36 option, this model treats q36
# as a mutually exclusive discrete choice. AMEs across the four columns shift
# probability mass and therefore sum approximately to zero for each contrast.
q36_mnp_model <- fit_q36_mnp_model(q36_single_choice)
mnp_ames <- compute_option_ames_mnp(q36_mnp_model, q36_single_choice, term_specs)

# This model keeps the multiple-response coding but replaces the random
# intercept with respondent-clustered inference in a stacked binary probit.
q36_clustered_binary_model <- fit_q36_clustered_binary_model(q36_long)
clustered_binary_ames <- compute_option_ames_feglm(q36_clustered_binary_model, q36_long, term_specs)

mvprobit_ames <- NULL
if (q36_run_mvprobit) {
  message("Running mvProbit. This model can be slow on the full q36 design.")
  q36_mvprobit_model <- fit_q36_mvprobit_model(q36_respondents)
  mvprobit_ames <- compute_mvprobit_ames(q36_mvprobit_model, q36_respondents, term_specs)
} else {
  message("Skipping mvProbit. Set Q36_RUN_MVPROBIT=true to estimate the multivariate probit companion model.")
}


# Export table ----

write_option_ame_table(
  path_tex = file.path(table_latex_dir, "defense_tax_pref_ame_q36_glmm.tex"),
  path_html = file.path(table_html_dir, "defense_tax_pref_ame_q36_glmm.html"),
  ames = option_ames,
  term_specs = term_specs,
  option_labels = q36_option_short_labels,
  nobs_model = nobs(q36_model),
  respondents_model = uniqueN(q36_long$respid),
  selection_count_table = respondent_selection_counts,
  singular_fit = singular_fit
)

write_companion_option_ame_table(
  path_tex = file.path(table_latex_dir, "defense_tax_pref_mprobit_q36_single.tex"),
  path_html = file.path(table_html_dir, "defense_tax_pref_mprobit_q36_single.html"),
  ames = mnp_ames,
  term_specs = term_specs,
  option_labels = q36_option_short_labels,
  footer_rows = list(
    list(term = "Observations", contrast = "répondants", cells = rep(as.character(nrow(q36_single_choice)), 4L)),
    list(term = "Échantillon", contrast = "", cells = rep("Une seule réponse q36", 4L)),
    list(term = "Contrôles additionnels", contrast = "", cells = rep("Oui", 4L)),
    list(term = "Tirages MCMC", contrast = "", cells = rep(as.character(q36_mnp_draws - q36_mnp_burnin), 4L)),
    list(term = "Tirages prédiction", contrast = "", cells = rep(as.character(q36_mnp_pred_draws), 4L)),
    list(term = "Graine MCMC", contrast = "", cells = rep(as.character(q36_mnp_model$q36_seed), 4L))
  ),
  caption = "Préférences de financement q36 parmi les répondants à choix unique",
  label = "tab:defense_tax_pref_q36_mnp_single",
  note_text = paste(
    "\\textbf{Note:} Modèle probit multinomial bayésien estimé avec MNP sur les répondants qui ont coché exactement une option q36.",
    "Chaque cellule présente un effet marginal moyen sur la probabilité que l'option soit le choix unique.",
    "Les quatre colonnes forment une distribution de probabilité et les effets marginaux d'un contraste somment approximativement à zéro.",
    "Le modèle inclut les mêmes variables affichées et contrôles additionnels que la table q36 principale.",
    "MNP ne prend pas de poids de vraisemblance ; les poids de sondage sont utilisés pour moyenner les effets marginaux prédits.",
    "Pour les variables continues, les cellules correspondent à une hausse d'une unité de variable standardisée.",
    if (q36_mnp_pred_draws > 1) {
      "Seuil de confiance indicatif calculé à partir de la dispersion postérieure prédite : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
    } else {
      "Les écarts-types ne sont pas reportés par défaut ; définir Q36_MNP_PRED_DRAWS > 1 pour résumer la dispersion postérieure prédite."
    }
  ),
  html_note = paste(
    "Note: Modèle probit multinomial bayésien estimé avec MNP sur les répondants qui ont coché exactement une option q36.",
    "Chaque cellule présente un effet marginal moyen sur la probabilité que l'option soit le choix unique.",
    "Les quatre colonnes forment une distribution de probabilité et les effets marginaux d'un contraste somment approximativement à zéro.",
    "Le modèle inclut les mêmes variables affichées et contrôles additionnels que la table q36 principale.",
    "MNP ne prend pas de poids de vraisemblance ; les poids de sondage sont utilisés pour moyenner les effets marginaux prédits.",
    "Pour les variables continues, les cellules correspondent à une hausse d'une unité de variable standardisée.",
    if (q36_mnp_pred_draws > 1) {
      "Seuil de confiance indicatif calculé à partir de la dispersion postérieure prédite : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
    } else {
      "Les écarts-types ne sont pas reportés par défaut ; définir Q36_MNP_PRED_DRAWS > 1 pour résumer la dispersion postérieure prédite."
    }
  )
)

write_companion_option_ame_table(
  path_tex = file.path(table_latex_dir, "defense_tax_pref_binary_q36_clustered.tex"),
  path_html = file.path(table_html_dir, "defense_tax_pref_binary_q36_clustered.html"),
  ames = clustered_binary_ames,
  term_specs = term_specs,
  option_labels = q36_option_short_labels,
  footer_rows = list(
    list(term = "Observations", contrast = "répondant-option", cells = rep(as.character(nobs(q36_clustered_binary_model)), 4L)),
    list(term = "Répondants", contrast = "", cells = rep(as.character(uniqueN(q36_long$respid)), 4L)),
    list(term = "Répondants multi-réponses", contrast = "", cells = rep(as.character(respondent_selection_counts[q36_selection_count > 1L, sum(N)]), 4L)),
    list(term = "Répondants sans q36", contrast = "", cells = rep(as.character(respondent_selection_counts[q36_selection_count == 0L, sum(N)]), 4L)),
    list(term = "Contrôles additionnels", contrast = "", cells = rep("Oui", 4L)),
    list(term = "Écarts-types", contrast = "", cells = rep("Cluster répondant", 4L))
  ),
  caption = "Préférences de financement q36, probits binaires clusterisés",
  label = "tab:defense_tax_pref_q36_binary_clustered",
  note_text = paste(
    "\\textbf{Note:} Modèle probit binaire empilé au niveau répondant-option.",
    "La variable dépendante vaut 1 si l'option q36 est cochée ; les réponses multiples sont conservées.",
    "Le modèle interagit l'option q36 avec les variables affichées et inclut les mêmes contrôles additionnels que la table q36 principale.",
    "Les poids sont utilisés comme poids de vraisemblance et les écarts-types sont clusterisés au niveau du répondant.",
    "Age et age au carré sont standardisés.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  ),
  html_note = paste(
    "Note: Modèle probit binaire empilé au niveau répondant-option.",
    "La variable dépendante vaut 1 si l'option q36 est cochée ; les réponses multiples sont conservées.",
    "Le modèle interagit l'option q36 avec les variables affichées et inclut les mêmes contrôles additionnels que la table q36 principale.",
    "Les poids sont utilisés comme poids de vraisemblance et les écarts-types sont clusterisés au niveau du répondant.",
    "Age et age au carré sont standardisés.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

if (!is.null(mvprobit_ames)) {
  sigma <- attr(mvprobit_ames, "sigma")
  corr_pairs <- combn(seq_len(nrow(sigma)), 2L, simplify = FALSE)
  corr_rows <- lapply(
    seq_along(corr_pairs),
    function(i) {
      pair <- corr_pairs[[i]]
      label <- paste(rownames(sigma)[pair], collapse = " - ")
      value <- sprintf("%.3f", sigma[pair[1L], pair[2L]])
      list(
        term = if (i == 1L) "Corrélations latentes" else "",
        contrast = label,
        cells = c(value, "", "", "")
      )
    }
  )

  write_companion_option_ame_table(
    path_tex = file.path(table_latex_dir, "defense_tax_pref_mvprobit_q36.tex"),
    path_html = file.path(table_html_dir, "defense_tax_pref_mvprobit_q36.html"),
    ames = mvprobit_ames,
    term_specs = term_specs,
    option_labels = q36_option_short_labels,
    footer_rows = c(
      list(
        list(term = "Observations", contrast = "répondants", cells = rep(as.character(nrow(q36_respondents)), 4L)),
        list(term = "Contrôles additionnels", contrast = "", cells = rep("Oui", 4L)),
        list(term = "Simulations GHK", contrast = "", cells = rep(as.character(q36_mvprobit_n_ghk), 4L)),
        list(term = "Itérations max", contrast = "", cells = rep(as.character(q36_mvprobit_iterlim), 4L)),
        list(term = "Itérations utilisées", contrast = "", cells = rep(as.character(q36_mvprobit_model$iterations), 4L)),
        list(
          term = "Code convergence",
          contrast = q36_mvprobit_model$message,
          cells = rep(as.character(q36_mvprobit_model$code), 4L)
        )
      ),
      corr_rows
    ),
    caption = "Préférences de financement q36, probit multivarié",
    label = "tab:defense_tax_pref_q36_mvprobit",
    note_text = paste(
      "\\textbf{Note:} Modèle probit multivarié estimé au niveau répondant avec les quatre options q36 comme variables dépendantes binaires conjointes.",
      "Chaque équation inclut les mêmes variables affichées et contrôles additionnels que la table q36 principale.",
      "Les effets marginaux affichés portent sur les probabilités marginales de sélection de chaque option.",
      "La matrice de corrélations latentes résume la dépendance résiduelle entre options.",
      "mvProbit ne prend pas de poids de vraisemblance ; les poids de sondage sont utilisés pour moyenner les effets marginaux prédits.",
      "Les écarts-types des effets marginaux ne sont pas reportés dans cette table exploratoire.",
      "Age et age au carré sont standardisés."
    ),
    html_note = paste(
      "Note: Modèle probit multivarié estimé au niveau répondant avec les quatre options q36 comme variables dépendantes binaires conjointes.",
      "Chaque équation inclut les mêmes variables affichées et contrôles additionnels que la table q36 principale.",
      "Les effets marginaux affichés portent sur les probabilités marginales de sélection de chaque option.",
      "La matrice de corrélations latentes résume la dépendance résiduelle entre options.",
      "mvProbit ne prend pas de poids de vraisemblance ; les poids de sondage sont utilisés pour moyenner les effets marginaux prédits.",
      "Les écarts-types des effets marginaux ne sont pas reportés dans cette table exploratoire.",
      "Age et age au carré sont standardisés."
    )
  )
}
