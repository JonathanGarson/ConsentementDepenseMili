# This code mirrors the defense-spending analysis for other expenditure outcomes.

library(data.table)
library(fixest)
library(modelsummary)
library(marginaleffects)

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

empty_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x[trimws(x) == ""] <- NA_character_
  x
}

relevel_factor <- function(x, ref, var_name) {
  x <- factor(x)

  if (!ref %in% levels(x)) {
    stop("Reference level '", ref, "' not found in ", var_name, call. = FALSE)
  }
  relevel(x, ref = ref)
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

  if (nrow(data) == 0) {
    stop("No observations with valid positive survey weights.", call. = FALSE)
  }

  data
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
  ref <- income_levels[1]

  if (!ref %in% x_chr) {
    stop("Reference level '", ref, "' not found in tranche_revenus", call. = FALSE)
  }

  present_levels <- income_levels[income_levels %chin% unique(x_chr)]
  factor(x_chr, levels = present_levels)
}

clean_ame_labels <- function(ame) {
  ame$term[ame$term == "q19"] <- "Niveau d'imposition"
  ame$term[ame$term == "q30_q2"] <- "Confiance dans l'État"
  ame$term[ame$term == "q35"] <- "Risque de conflit"

  ame$contrast[ame$contrast == "Ni trop, ni pas assez élevés - Trop élevés"] <- "Ni trop ni peu - Trop élevés"
  ame$contrast[ame$contrast == "Pas assez élevés - Trop élevés"] <- "Pas assez - Trop élevés"
  ame$contrast[ame$contrast == "Plutôt confiance - Pas du tout confiance"] <- "Plutôt conf. - Pas du tout"
  ame$contrast[ame$contrast == "Plutôt pas confiance - Pas du tout confiance"] <- "Plutôt pas conf. - Pas du tout"
  ame$contrast[ame$contrast == "Tout à fait confiance - Pas du tout confiance"] <- "Tout à fait conf. - Pas du tout"
  ame$contrast[ame$contrast == "Vous ne savez pas - Très élevé"] <- "NSP - Très élevé"

  reference_suffixes <- c(
    " - Pas du tout",
    " - Trop élevés",
    " - Très élevé"
  )
  for (suffix in reference_suffixes) {
    suffix_idx <- !is.na(ame$contrast) & endsWith(ame$contrast, suffix)
    ame$contrast[suffix_idx] <- substring(
      ame$contrast[suffix_idx],
      1L,
      nchar(ame$contrast[suffix_idx]) - nchar(suffix)
    )
  }

  ame
}

order_otherexp_table_rows <- function(ame) {
  term_order <- c(
    "Confiance dans l'État",
    "Niveau d'imposition",
    "Risque de conflit"
  )
  contrast_order <- c(
    "Plutôt conf.",
    "Plutôt pas conf.",
    "Tout à fait conf.",
    "Ni trop ni peu",
    "Pas assez",
    "Assez élevé",
    "Plutôt faible",
    "Très faible",
    "NSP"
  )

  ordering_frame <- data.table(
    term_rank = fifelse(is.na(match(ame$term, term_order)), length(term_order) + 1L, match(ame$term, term_order)),
    contrast_rank = fifelse(is.na(match(ame$contrast, contrast_order)), length(contrast_order) + 1L, match(ame$contrast, contrast_order))
  )
  order_idx <- do.call(order, ordering_frame)
  ame[order_idx, ]
}

add_q35_binary <- function(data) {
  data[, q35_binary := fcase(
    q35 %chin% c("Très élevé", "Assez élevé"), "Risque élevé",
    q35 %chin% c("Plutôt faible", "Très faible"), "Risque faible",
    default = NA_character_
    )
  ]
  data[!is.na(q35_binary)]
}

otherexp_control_vars <- c(
  "pcs", "matri", "foyer", "continuous_age", "tailleagglo5",
  "tranche_revenus", "diplome", "partisane", "gender"
)

fit_otherexp_model <- function(data,
                               outcome,
                               partisane_ref = "Aucun",
                               q30_ref = "Pas du tout confiance",
                               q35_ref = "Très élevé",
                               q19_ref = "Trop élevés",
                               pcs_ref = "Retraité",
                               gender_ref = "Femme",
                               include_controls = FALSE,
                               use_q35_binary = FALSE) {
  data <- copy(data)
  if (use_q35_binary) {
    required_vars <- c(outcome, "q30_q2", "q35", "weights")
    data <- data[complete.cases(data[, ..required_vars])]
    data <- add_q35_binary(data)
    data[, q30_q2 := relevel_factor(q30_q2, ref = q30_ref, var_name = "q30_q2")]
    data[, q35_binary := relevel_factor(q35_binary, ref = "Risque faible", var_name = "q35_binary")]
    model_formula <- as.formula(sprintf("%s ~ q30_q2 + q35_binary", outcome))
  } else {
    required_vars <- c(outcome, "partisane", "q19", "q30_q2", "q35", "weights")
    if (include_controls) {
      required_vars <- c(required_vars, setdiff(otherexp_control_vars, "partisane"))
    }
    data <- data[complete.cases(data[, ..required_vars])]
    data[, partisane := relevel_factor(partisane, ref = partisane_ref, var_name = "partisane")]
    data[, q30_q2 := relevel_factor(q30_q2, ref = q30_ref, var_name = "q30_q2")]
    data[, q35 := relevel_factor(q35, ref = q35_ref, var_name = "q35")]
    data[, q19 := relevel_factor(q19, ref = q19_ref, var_name = "q19")]

    if (include_controls) {
      pcs_reference <- pcs_ref
      data[, gender := relevel_factor(gender, ref = gender_ref, var_name = "gender")]
      data[
        ,
        `:=`(
          pcs = relevel_factor(pcs, ref = pcs_reference, var_name = "pcs"),
          matri = factor(matri),
          foyer = factor(foyer),
          tailleagglo5 = factor(tailleagglo5),
          tranche_revenus = relevel_income_factor(tranche_revenus),
          diplome = factor(diplome)
        )
      ]

      model_formula <- as.formula(
        sprintf(
          "%s ~ q30_q2 + q35 + q19 + pcs + matri + foyer + continuous_age + tailleagglo5 + tranche_revenus + diplome + partisane + gender",
          outcome
        )
      )
    } else {
      model_formula <- as.formula(sprintf("%s ~ q30_q2 + q35 + q19 + partisane", outcome))
    }
  }

  model <- feglm(
    model_formula,
    family = binomial(link = "probit"),
    vcov = "hetero",
    weights = ~ weights,
    data = data
  )

  list(
    ame = avg_slopes(
      model,
      newdata = data,
      wts = "weights"
    ),
    nobs = nobs(model)
  )
}

postprocess_otherexp_table <- function(path, spanner_label, outcome_headers) {
  term_labels <- c(
    "Confiance dans l'État",
    "Niveau d'imposition",
    "Risque de conflit"
  )
  reference_labels <- c(
    "Confiance dans l'État" = "Pas du tout confiance",
    "Niveau d'imposition" = "Trop élevés",
    "Risque de conflit" = "Très élevé"
  )
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\\\\centering", "\\\\centering\n\\\\scriptsize", lines, fixed = FALSE)

  header_idx <- grep("TinyTableHeader", lines, fixed = TRUE)
  if (length(header_idx) != 1L) {
    stop("Could not locate the non-defense table header row for post-processing.", call. = FALSE)
  }

  column_line <- paste0("& & ", paste(outcome_headers, collapse = " & "), " \\\\")
  lines[header_idx] <- paste0(column_line, "\n\\midrule %% TinyTableHeader")
  spanner_line <- paste0("& & \\SetCell[c=", length(outcome_headers), "]{c} ", spanner_label, " \\\\")
  lines <- append(lines, spanner_line, after = header_idx - 1L)

  first_term_idx <- which(vapply(
    lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))[1]
  observations_idx <- which(startsWith(lines, "Observations &"))
  bottomrule_idx <- grep("^\\\\bottomrule", lines)

  if (length(first_term_idx) != 1L || length(bottomrule_idx) != 1L || first_term_idx >= bottomrule_idx) {
    stop("Could not locate the non-defense table body for post-processing.", call. = FALSE)
  }

  if (length(observations_idx) == 1L && first_term_idx < observations_idx) {
    body_lines <- lines[first_term_idx:(observations_idx - 1L)]
    summary_lines <- lines[observations_idx:(bottomrule_idx - 1L)]
  } else {
    body_lines <- lines[first_term_idx:(bottomrule_idx - 1L)]
    summary_lines <- character(0)
  }

  block_starts <- which(vapply(
    body_lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))

  if (length(block_starts) == 0L) {
    stop("Could not identify term blocks in the non-defense table body.", call. = FALSE)
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
    if (term_label %in% names(reference_labels)) {
      model_columns <- length(outcome_headers)
      reference_cells <- paste(rep("\\textit{Ref}", model_columns), collapse = " & ")
      original_row_prefix <- paste0(term_label, " &")
      block[1] <- paste0(
        term_label, " & ", reference_labels[[term_label]], " & ",
        reference_cells, " \\\\",
        "\n&", substring(block[1], nchar(original_row_prefix) + 1L)
      )
    }
    reordered_lines <- c(reordered_lines, block)
  }

  if (length(reordered_lines) == 0L) {
    stop("No term blocks available after reordering the non-defense table.", call. = FALSE)
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

    reordered_lines[row_idx] <- sub(
      paste0("^", matched_term, " &"),
      paste0("\\\\textbf{", matched_term, "} &"),
      reordered_lines[row_idx]
    )

    if (i > 1L) {
      reordered_lines[row_idx] <- paste0("\\midrule\n", reordered_lines[row_idx])
    }
  }

  lines <- c(
    lines[seq_len(first_term_idx - 1L)],
    reordered_lines,
    if (length(summary_lines) > 0L) {
      c(paste0("\\midrule\\midrule\n", summary_lines[1]), summary_lines[-1L])
    },
    lines[bottomrule_idx:length(lines)]
  )

  writeLines(lines, path, useBytes = TRUE)
}

write_ame_table <- function(models, file_name, notes, coef_omit = NULL, title = NULL, add_rows = NULL) {
  output_file <- file.path(table_dir, file_name)

  modelsummary(
    models,
    title = title,
    escape = FALSE,
    shape = term + contrast ~ model,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    fmt = 3,
    gof_omit = ".*",
    coef_omit = coef_omit,
    add_rows = add_rows,
    notes = notes,
    output = output_file
  )

  message("Table saved to: ", output_file)
}

outcome_labels <- c(
  healthexp_binary = "Santé",
  pensionexp_binary = "Retraites",
  povertyexp_binary = "Pauvreté"
)

otherexp_char_vars <- c(
  "partisane", "q19", "q30_q2", "q35", "pcs", "matri", "foyer",
  "tailleagglo5", "tranche_revenus", "diplome", "gender"
)

required_columns <- c(
  names(outcome_labels), "q19", "q30_q2", "q35", "treatment", "weights",
  otherexp_control_vars
)

consent_other <- fread(file.path(data_final_dir, "final_survey2025.csv"))[treatment == 0]
missing_columns <- setdiff(required_columns, names(consent_other))
if (length(missing_columns) > 0) {
  stop(
    "Required columns missing from final_survey2025.csv: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}
consent_other <- filter_valid_weights(consent_other)

consent_other[
  ,
  (otherexp_char_vars) := lapply(.SD, empty_to_na),
  .SDcols = otherexp_char_vars
]

controlled_model_outputs <- setNames(
  lapply(
    names(outcome_labels),
    function(outcome) {
      fit_otherexp_model(
      data = consent_other,
      outcome = outcome,
      partisane_ref = "Aucun",
        q35_ref = "Très élevé",
        q19_ref = "Trop élevés",
        include_controls = TRUE
      )
    }
  ),
  outcome_labels
)
controlled_models <- lapply(
  controlled_model_outputs,
  function(model_output) order_otherexp_table_rows(clean_ame_labels(model_output$ame))
)

otherexp_add_rows <- data.frame(
  term = c("Observations", "Contrôle"),
  contrast = c("", ""),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

for (outcome_label in names(controlled_model_outputs)) {
  otherexp_add_rows[[outcome_label]] <- c(
    as.character(controlled_model_outputs[[outcome_label]]$nobs),
    "Oui"
  )
}

write_ame_table(
  models = controlled_models,
  file_name = "otherexp_ame_baseline.tex",
  title = "Refus de baisser les autres dépenses de l'État",
  coef_omit = "pcs|matri|foyer|tailleagglo5|diplome|partisane|gender|continuous_age|tranche_revenus",
  add_rows = otherexp_add_rows,
  notes = c(
    "Liste des variables de contrôle : catégorie socio-professionnelle, situation matrimoniale, foyer, âge, taille d'agglomération, revenus, diplôme, proximité partisane et genre.",
    "+ p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés à partir de modèles probit pondérés par les poids de sondage. Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses."
  )
)
postprocess_otherexp_table(
  path = file.path(table_dir, "otherexp_ame_baseline.tex"),
  spanner_label = "Refus de baisser la dépense considérée",
  outcome_headers = unname(outcome_labels)
)

message("Table saved to: ", file.path(table_dir, "otherexp_ame_baseline.tex"))
