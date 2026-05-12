# This script estimates the main defense-spending consent models and
# exports LaTeX and HTML tables.

# Setup ----
library(data.table)
library(fixest)
library(marginaleffects)
library(modelsummary)

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
table_latex_dir <- file.path(table_dir, "latex")
table_html_dir <- file.path(table_dir, "html")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_latex_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_html_dir, recursive = TRUE, showWarnings = FALSE)


# Helper functions ----

# Convert blank strings to missing values before building model samples.
empty_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x[trimws(x) == ""] <- NA_character_
  x
}

# Relevel categorical variables and fail loudly if the requested reference is absent.
relevel_factor <- function(x, ref, var_name) {
  x <- factor(x)

  if (!ref %in% levels(x)) {
    stop("Reference level '", ref, "' not found in ", var_name, call. = FALSE)
  }

  relevel(x, ref = ref)
}

relevel_q30_factor <- function(x) {
  q30_levels <- c(
    "Pas du tout confiance",
    "Plutôt pas confiance",
    "Plutôt confiance",
    "Tout à fait confiance"
  )
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), q30_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in q30_q2: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!q30_levels[1] %chin% x_chr) {
    stop("Reference level '", q30_levels[1], "' not found in q30_q2", call. = FALSE)
  }

  factor(x_chr, levels = q30_levels)
}

relevel_q5_factor <- function(x) {
  q5_levels <- c("Non", "Oui")
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), q5_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in q5 after yes/no filtering: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!q5_levels[1] %chin% x_chr) {
    stop("Reference level '", q5_levels[1], "' not found in q5", call. = FALSE)
  }

  factor(x_chr, levels = q5_levels)
}

relevel_q35_nsp_factor <- function(x) {
  q35_levels <- c(
    "Vous ne savez pas",
    "Très élevé",
    "Assez élevé",
    "Plutôt faible",
    "Très faible"
  )
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), q35_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in q35: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!q35_levels[1] %chin% x_chr) {
    stop("Reference level '", q35_levels[1], "' not found in q35", call. = FALSE)
  }

  factor(x_chr, levels = q35_levels)
}

relevel_q35_tres_faible_factor <- function(x) {
  q35_levels <- c(
    "Très faible",
    "Plutôt faible",
    "Assez élevé",
    "Très élevé",
    "Vous ne savez pas"
  )
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), q35_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in q35: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!q35_levels[1] %chin% x_chr) {
    stop("Reference level '", q35_levels[1], "' not found in q35", call. = FALSE)
  }

  factor(x_chr, levels = q35_levels)
}

relevel_satisfaction_factor <- function(x, var_name) {
  satisfaction_levels <- c(
    "Pas du tout satisfait(e)",
    "Plutôt pas satisfait(e)",
    "Plutôt satisfait(e)",
    "Très satisfait(e)"
  )
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), satisfaction_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in ", var_name, ": ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!satisfaction_levels[1] %chin% x_chr) {
    stop(
      "Reference level '", satisfaction_levels[1], "' not found in ", var_name,
      call. = FALSE
    )
  }

  factor(x_chr, levels = satisfaction_levels)
}

relevel_sat_factor <- function(x) {
  relevel_satisfaction_factor(x, var_name = "sat")
}

relevel_q28_factor <- function(x) {
  relevel_satisfaction_factor(x, var_name = "q28")
}

relevel_q35_nsp_collapsed_factor <- function(x) {
  q35_levels <- c("Risque faible", "Risque élevé", "NSP")
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), q35_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in q35_nsp_collapsed: ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!q35_levels[1] %chin% x_chr) {
    stop("Reference level '", q35_levels[1], "' not found in q35_nsp_collapsed", call. = FALSE)
  }

  factor(x_chr, levels = q35_levels)
}

# Reorder income brackets from lowest to highest and keep the smallest bracket as reference.
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

relevel_age2_factor <- function(x) {
  age_levels <- c(
    "18 à 24 ans",
    "25 à 29 ans",
    "30 à 34 ans",
    "35 à 39 ans",
    "40 à 44 ans",
    "45 à 49 ans",
    "50 à 54 ans",
    "55 à 59 ans",
    "60 à 64 ans",
    "65 à 69 ans",
    "70 ans et plus"
  )

  x_chr <- as.character(x)
  ref <- age_levels[1]

  if (!ref %in% x_chr) {
    stop("Reference level '", ref, "' not found in age2", call. = FALSE)
  }

  present_levels <- age_levels[age_levels %chin% unique(x_chr)]
  factor(x_chr, levels = present_levels)
}

# Survey-weighted probits require a numeric, strictly positive weights column.
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

# Shorten labels so the AME tables are easier to read.
clean_ame_labels <- function(ame) {
  revenue_label_map <- c(
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

  ame$term[ame$term == "q19"] <- "Niveau d'imposition"
  ame$term[ame$term == "q30_q2"] <- "Confiance dans l'État"
  ame$term[ame$term == "q35"] <- "Risque de conflit"
  ame$term[ame$term == "q35_nsp_collapsed"] <- "Risque de conflit"
  ame$term[ame$term == "q5"] <- "Impôt sur le revenu"
  ame$term[ame$term == "q28"] <- "Satisfaction Utilisation Argent Public"
  ame$term[ame$term == "gender"] <- "Genre"
  ame$term[ame$term == "continuous_age"] <- "Age"
  ame$term[ame$term == "continuous_age_sq"] <- "Age\\textsuperscript{2}"
  ame$term[ame$term == "continuous_age_norm"] <- "Age"
  ame$term[ame$term == "continuous_age_norm_sq"] <- "Age\\textsuperscript{2}"
  ame$term[ame$term == "age2"] <- "age_2"
  ame$term[ame$term == "acte_citoyen"] <- "Acte citoyen"
  ame$term[ame$term == "tranche_revenus"] <- "Revenus"

  ame$contrast[ame$contrast == "Pas assez élevés - Ni trop, ni pas assez élevés"] <- "Pas assez - Juste"
  ame$contrast[ame$contrast == "Trop élevés - Ni trop, ni pas assez élevés"] <- "Trop élevés - Juste"
  ame$contrast[ame$contrast == "Plutôt confiance - Pas du tout confiance"] <- "Plutôt conf. - Pas du tout"
  ame$contrast[ame$contrast == "Plutôt pas confiance - Pas du tout confiance"] <- "Plutôt pas conf. - Pas du tout"
  ame$contrast[ame$contrast == "Tout à fait confiance - Pas du tout confiance"] <- "Tout à fait conf. - Pas du tout"
  ame$contrast <- gsub("Vous ne savez pas", "NSP", ame$contrast, fixed = TRUE)
  ame$contrast[ame$term %in% c("Age", "Age\\textsuperscript{2}")] <- ""

  for (long_label in names(revenue_label_map)) {
    ame$contrast <- gsub(
      long_label,
      revenue_label_map[[long_label]],
      ame$contrast,
      fixed = TRUE
    )
  }

  reference_suffixes <- c(
    " - Pas du tout d’accord",
    " - Pas du tout",
    " - Juste",
    " - Trop élevés",
    " - Très élevé",
    " - Très faible",
    " - NSP",
    " - Risque élevé",
    " - Risque faible",
    " - Non",
    " - Pas du tout satisfait(e)",
    " - 18 à 24 ans",
    " - Femme",
    " - < 12k"
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

order_main_table_rows <- function(ame) {
  term_order <- c(
    "Acte citoyen",
    "Niveau d'imposition",
    "Impôt sur le revenu",
    "Risque de conflit",
    "Satisfaction Utilisation Argent Public",
    "Genre",
    "age_2",
    "Age",
    "Age\\textsuperscript{2}",
    "Revenus"
  )
  contrast_order <- list(
    "Acte citoyen" = c("Plutôt pas d’accord", "Plutôt d’accord", "Tout à fait d’accord"),
    "Niveau d'imposition" = c("Trop élevés", "Pas assez"),
    "Impôt sur le revenu" = "Oui",
    "Risque de conflit" = c("Risque élevé", "NSP", "Plutôt faible", "Assez élevé", "Très élevé", "Risque faible", "Très faible"),
    "Satisfaction Utilisation Argent Public" = c("Plutôt pas satisfait(e)", "Plutôt satisfait(e)", "Très satisfait(e)"),
    "Genre" = "Homme",
    "age_2" = c(
      "25 à 29 ans",
      "30 à 34 ans",
      "35 à 39 ans",
      "40 à 44 ans",
      "45 à 49 ans",
      "50 à 54 ans",
      "55 à 59 ans",
      "60 à 64 ans",
      "65 à 69 ans",
      "70 ans et plus"
    ),
    "Age" = "",
    "Age\\textsuperscript{2}" = "",
    "Revenus" = c(
      "12k--18k",
      "18k--24k",
      "24k--36k",
      "36k--48k",
      "48k--60k",
      "60k--72k",
      "72k--144k",
      "144k+",
      "NSP"
    )
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

order_console_ame_rows <- function(ame) {
  term_order <- c("partisane", "q19", "q30_q2", "q35", "q35_binary", "sat")
  contrast_order <- list(
    q30_q2 = c(
      "Plutôt pas confiance - Pas du tout confiance",
      "Plutôt confiance - Pas du tout confiance",
      "Tout à fait confiance - Pas du tout confiance"
    ),
    q35 = c(
      "Très élevé - Vous ne savez pas",
      "Assez élevé - Vous ne savez pas",
      "Plutôt faible - Vous ne savez pas",
      "Très faible - Vous ne savez pas"
    ),
    sat = c(
      "Plutôt pas satisfait(e) - Pas du tout satisfait(e)",
      "Plutôt satisfait(e) - Pas du tout satisfait(e)",
      "Très satisfait(e) - Pas du tout satisfait(e)"
    )
  )
  ame_dt <- as.data.table(ame)
  contrast_rank <- vapply(
    seq_len(nrow(ame_dt)),
    function(i) {
      term_contrast_order <- contrast_order[[ame_dt$term[i]]]
      if (is.null(term_contrast_order)) {
        return(Inf)
      }

      rank <- match(ame_dt$contrast[i], term_contrast_order)
      ifelse(is.na(rank), length(term_contrast_order) + 1L, rank)
    },
    numeric(1)
  )

  ordering_frame <- data.table(
    term_rank = fifelse(
      is.na(match(ame_dt$term, term_order)),
      length(term_order) + 1L,
      match(ame_dt$term, term_order)
    ),
    contrast_rank = contrast_rank,
    original_rank = seq_len(nrow(ame_dt))
  )
  order_idx <- do.call(order, ordering_frame)
  ame[order_idx, ]
}

# Collapse q35 into high vs low perceived conflict risk for the binary-risk models.
add_q35_binary <- function(data) {
  data[
    ,
    q35_binary := fcase(
      q35 %chin% c("Très élevé", "Assez élevé"), "Risque élevé",
      q35 %chin% c("Plutôt faible", "Très faible"), "Risque faible",
      default = NA_character_
    )
  ]
  data[!is.na(q35_binary)]
}

add_q35_nsp_collapsed <- function(data) {
  data[
    ,
    q35_nsp_collapsed := fcase(
      q35 == "Vous ne savez pas", "NSP",
      q35 %chin% c("Très élevé", "Assez élevé"), "Risque élevé",
      q35 %chin% c("Plutôt faible", "Très faible"), "Risque faible",
      default = NA_character_
    )
  ]
  data[!is.na(q35_nsp_collapsed)]
}

# Evaluate average predicted probabilities in every confidence x risk cell.
build_interaction_table <- function(model, data) {
  confidence_levels <- c(
    "Pas du tout confiance",
    "Plutôt pas confiance",
    "Plutôt confiance",
    "Tout à fait confiance"
  )
  risk_levels <- c("Risque faible", "Risque élevé")
  cells <- CJ(q30_q2 = confidence_levels, q35_binary = risk_levels)

  interaction_table <- rbindlist(
    lapply(
      seq_len(nrow(cells)),
      function(i) {
        counterfactual <- copy(data)
        counterfactual[, q30_q2 := factor(cells$q30_q2[i], levels = confidence_levels)]
        counterfactual[, q35_binary := factor(cells$q35_binary[i], levels = risk_levels)]

        data.table(
          confidence_label = cells$q30_q2[i],
          risk_label = cells$q35_binary[i],
          predicted = weighted.mean(
            predict(model, newdata = counterfactual, type = "response"),
            w = counterfactual$weights
          )
        )
      }
    )
  )

  interaction_table[, confidence_label := factor(confidence_label, levels = confidence_levels)]
  interaction_table[, risk_label := factor(risk_label, levels = risk_levels)]
  setorder(interaction_table, confidence_label, risk_label)
  interaction_table
}

# Print the interaction table and a short interpretation derived from the estimates.
print_interaction_results <- function(interaction_table) {
  interaction_display <- copy(interaction_table)
  interaction_display[
    ,
    `Probabilité prédite` := sprintf("%.1f%%", 100 * predicted)
  ]
  interaction_display <- interaction_display[
    ,
    .(
      `Confiance État` = as.character(confidence_label),
      `Risque conflit` = as.character(risk_label),
      `Probabilité prédite` = `Probabilité prédite`
    )
  ]

  risk_shift <- dcast(
    copy(interaction_table),
    confidence_label ~ risk_label,
    value.var = "predicted"
  )
  risk_shift[, diff_high_minus_low := `Risque élevé` - `Risque faible`]

  highest_cell <- interaction_table[which.max(predicted)]
  lowest_cell <- interaction_table[which.min(predicted)]
  low_risk_range <- interaction_table[risk_label == "Risque faible", max(predicted) - min(predicted)]
  high_risk_range <- interaction_table[risk_label == "Risque élevé", max(predicted) - min(predicted)]

  gradient_sentence <- if (high_risk_range < low_risk_range - 0.005) {
    "Le gradient de confiance se resserre quand le risque perçu est élevé."
  } else if (high_risk_range > low_risk_range + 0.005) {
    "Le gradient de confiance s'accentue quand le risque perçu est élevé."
  } else {
    "Le gradient de confiance reste d'ampleur proche quand le risque perçu est élevé."
  }

  cat(
    "\nTable 4. Interaction entre confiance dans l'État et risque de conflit\n",
    "Probabilité prédite pondérée de refuser une baisse des dépenses militaires en échange d'une baisse d'impôts.\n\n",
    sep = ""
  )
  print(interaction_display)

  cat("\nInterprétation\n")
  cat(
    sprintf(
      "- La probabilité prédite la plus élevée est observée pour \"%s\" avec \"%s\" (%.1f%%).\n",
      as.character(highest_cell$confidence_label),
      as.character(highest_cell$risk_label),
      100 * highest_cell$predicted
    )
  )
  cat(
    sprintf(
      "- La plus faible est observée pour \"%s\" avec \"%s\" (%.1f%%).\n",
      as.character(lowest_cell$confidence_label),
      as.character(lowest_cell$risk_label),
      100 * lowest_cell$predicted
    )
  )

  for (i in seq_len(nrow(risk_shift))) {
    diff_pp <- 100 * risk_shift$diff_high_minus_low[i]
    direction <- if (diff_pp >= 0) "augmente" else "diminue"
    cat(
      sprintf(
        "- À niveau de confiance \"%s\", un risque perçu élevé %s la probabilité prédite de %.1f points.\n",
        as.character(risk_shift$confidence_label[i]),
        direction,
        abs(diff_pp)
      )
    )
  }

  cat(sprintf("- %s\n", gradient_sentence))
}

# Match the exported LaTeX table to the paper layout after modelsummary writes it.
postprocess_defense_table <- function(path) {
  term_labels <- c(
    "Confiance dans l'État",
    "Acte citoyen",
    "Niveau d'imposition",
    "Risque de conflit",
    "Genre",
    "Age",
    "Age\\textsuperscript{2}",
    "Revenus"
  )
  reference_labels <- c(
    "Confiance dans l'État" = "Pas du tout confiance",
    "Acte citoyen" = "Pas du tout d’accord",
    "Niveau d'imposition" = "Juste",
    "Risque de conflit" = "Très élevé",
    "Genre" = "Femme",
    "Revenus" = "< 12k"
  )

  split_table_row <- function(line) {
    row_end <- " \\\\"
    if (endsWith(line, row_end)) {
      line <- substring(line, 1L, nchar(line) - nchar(row_end))
    }
    strsplit(line, " & ", fixed = TRUE)[[1L]]
  }

  build_table_row <- function(cells) {
    paste0(paste(cells, collapse = " & "), " \\\\")
  }

  combine_estimate_and_se <- function(estimate_cells, se_cells) {
    for (j in 3:5) {
      if (nzchar(estimate_cells[[j]]) && nzchar(se_cells[[j]])) {
        estimate_cells[[j]] <- paste(estimate_cells[[j]], se_cells[[j]])
      }
    }
    estimate_cells
  }

  parse_block_rows <- function(term_label, block) {
    if (term_label == "Risque de conflit") {
      blank_term_idx <- grepl("^Risque de conflit &  &  & \\\\num\\{", block)
      block[blank_term_idx] <- sub(
        "^Risque de conflit &  &",
        "Risque de conflit & NSP &",
        block[blank_term_idx]
      )

      blank_continuation_idx <- grepl("^&  &  & \\\\num\\{", block)
      block[blank_continuation_idx] <- sub(
        "^&  &",
        "& NSP &",
        block[blank_continuation_idx]
      )
    }

    rows <- list()
    i <- 1L
    while (i <= length(block)) {
      cells <- split_table_row(block[[i]])
      if (length(cells) != 5L) {
        i <- i + 1L
        next
      }

      if (identical(cells[[1L]], term_label)) {
        cells[[1L]] <- ""
      }

      if (i < length(block)) {
        next_cells <- split_table_row(block[[i + 1L]])
        is_standard_error_row <- length(next_cells) == 5L &&
          !nzchar(next_cells[[1L]]) &&
          !nzchar(next_cells[[2L]]) &&
          any(startsWith(next_cells[3:5], "(\\num{"))

        if (is_standard_error_row) {
          cells <- combine_estimate_and_se(cells, next_cells)
          i <- i + 2L
        } else {
          i <- i + 1L
        }
      } else {
        i <- i + 1L
      }

      rows[[length(rows) + 1L]] <- cells
    }

    if (term_label %in% names(reference_labels)) {
      reference_cells <- c(
        paste0("\\textbf{", term_label, "}"),
        reference_labels[[term_label]],
        rep("\\textit{Ref}", 3L)
      )
      rows <- c(list(reference_cells), rows)
    } else if (length(rows) > 0L) {
      rows[[1L]][[1L]] <- paste0("\\textbf{", term_label, "}")
    }

    vapply(rows, build_table_row, character(1))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  header_idx <- grep("TinyTableHeader", lines, fixed = TRUE)
  if (length(header_idx) != 1L) {
    stop("Could not locate the defense table header row for post-processing.", call. = FALSE)
  }

  lines[header_idx] <- "\\midrule %% TinyTableHeader"
  spanner_line <- "& & \\SetCell[c=3]{c} Refus de baisser la dépense de défense \\\\"
  lines <- append(lines, spanner_line, after = header_idx - 1L)

  first_term_idx <- which(vapply(
    lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))[1]
  observations_idx <- which(startsWith(lines, "Observations &"))

  if (length(first_term_idx) != 1L || length(observations_idx) != 1L || first_term_idx >= observations_idx) {
    stop("Could not locate the defense table body for post-processing.", call. = FALSE)
  }

  body_lines <- lines[first_term_idx:(observations_idx - 1L)]
  block_starts <- which(vapply(
    body_lines,
    function(line) any(startsWith(line, paste0(term_labels, " &"))),
    logical(1)
  ))

  if (length(block_starts) == 0L) {
    stop("Could not identify term blocks in the defense table body.", call. = FALSE)
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
      blocks[[matched_term]] <- c(blocks[[matched_term]], block)
    }
  }

  if (!any(lengths(blocks[term_labels]) > 0L)) {
    stop("No term blocks available after reordering the defense table.", call. = FALSE)
  }

  reordered_lines <- character(0)

  for (term_label in term_labels) {
    block <- blocks[[term_label]]
    if (length(block) == 0L) {
      next
    }

    block_rows <- parse_block_rows(term_label, block)
    if (length(reordered_lines) > 0L) {
      block_rows[[1L]] <- paste0("\\midrule\n", block_rows[[1L]])
    }
    reordered_lines <- c(reordered_lines, block_rows)
  }

  lines <- c(
    lines[seq_len(first_term_idx - 1L)],
    reordered_lines,
    lines[observations_idx:length(lines)]
  )

  observations_idx <- which(startsWith(lines, "Observations &"))
  lines[observations_idx] <- paste0("\\midrule\\midrule\n", lines[observations_idx])

  start_idx <- grep("^\\\\begin\\{table\\}", lines)
  end_idx <- grep("^\\\\end\\{table\\}", lines)
  talltblr_end_idx <- grep("^\\\\end\\{talltblr\\}", lines)
  outer_open_idx <- grep("^\\\\begin\\{talltblr\\}", lines)
  inner_close_idx <- grep("^\\}                     %% tabularray inner close$", lines)

  if (
    length(start_idx) != 1L ||
      length(end_idx) != 1L ||
      length(talltblr_end_idx) != 1L ||
      length(outer_open_idx) != 1L ||
      length(inner_close_idx) != 1L
  ) {
    stop("Could not locate the LaTeX wrapper for the defense table.", call. = FALSE)
  }

  note_text <- paste(
    "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )

  outer_lines <- c(
    "\\begin{table*}[!t]",
    "\\centering",
    "\\scriptsize \\selectfont",
    "\\setlength{\\tabcolsep}{3pt}",
    "",
    "\\begin{talltblr}[",
    "caption={Les déterminants du refus de baisser la dépense de défense},",
    "label={tab:deter_consent_def},",
    paste0("note{}={", note_text, "},"),
    "]{"
  )
  inner_lines <- c(
    "width=0.97\\textwidth,",
    "\\rowsep = 0pt,",
    "colspec={",
    "Q[l,wd=0.22\\textwidth]",
    "Q[l,wd=0.20\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "},",
    "column{3,4,5}={halign=c},",
    "column{1,2}={halign=l},",
    "row{1}={font=\\bfseries},",
    "}"
  )

  lines <- c(
    outer_lines,
    inner_lines,
    lines[(inner_close_idx + 1L):(talltblr_end_idx - 1L)],
    "\\end{talltblr}",
    "\\end{table*}"
  )

  writeLines(lines, path, useBytes = TRUE)
}

format_ame_cell <- function(ame, term_label, contrast_label) {
  row <- as.data.table(ame)[term == term_label & contrast == contrast_label]
  if (nrow(row) == 0L) {
    return("")
  }

  estimate <- row$estimate[[1L]]
  std_error <- row$std.error[[1L]]
  p_value <- row$p.value[[1L]]
  if (abs(estimate) < 0.0005) {
    estimate <- 0
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

  sprintf("\\num{%.3f}%s (\\num{%.3f})", estimate, stars, std_error)
}

format_ame_cell_html <- function(ame, term_label, contrast_label) {
  row <- as.data.table(ame)[term == term_label & contrast == contrast_label]
  if (nrow(row) == 0L) {
    return("")
  }

  estimate <- row$estimate[[1L]]
  std_error <- row$std.error[[1L]]
  p_value <- row$p.value[[1L]]
  if (abs(estimate) < 0.0005) {
    estimate <- 0
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

  sprintf("%.3f%s (%.3f)", estimate, stars, std_error)
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

write_defense_table <- function(path,
                                ame_gender_only,
                                ame_baseline,
                                ame_controls,
                                nobs_gender_only,
                                nobs_baseline,
                                nobs_controls,
                                age_terms = c("Age", "Age\\textsuperscript{2}"),
                                age_reference_label = NULL,
                                age_display_labels = NULL,
                                include_q28 = FALSE,
                                risk_reference_label = "NSP",
                                risk_contrast_labels = c("Très élevé", "Assez élevé", "Plutôt faible", "Très faible"),
                                risk_ref_cells = rep("\\textit{Ref}", 3L),
                                table_caption = "Les déterminants du refus de baisser la dépense de défense",
                                table_label = "tab:deter_consent_def",
                                note_text = paste(
                                  "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, la satisfaction et le paiement de l'impôt sur le revenu.",
                                  "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
                                  "la taille d'agglomération, le diplôme et la proximité partisane.",
                                  "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
                                  "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
                                  "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
                                )) {
  models <- list(ame_gender_only, ame_baseline, ame_controls)
  model_cell <- function(term_label, contrast_label) {
    vapply(models, format_ame_cell, character(1), term_label = term_label, contrast_label = contrast_label)
  }
  add_row <- function(term_label, contrast_label, cells) {
    paste(c(term_label, contrast_label, cells), collapse = " & ")
  }
  add_ref <- function(term_label, reference_label, cells = rep("\\textit{Ref}", 3L)) {
    add_row(paste0("\\textbf{", term_label, "}"), reference_label, cells)
  }
  add_estimate <- function(term_label, contrast_label, display_label, bold = FALSE) {
    first_cell <- if (bold) paste0("\\textbf{", term_label, "}") else ""
    add_row(first_cell, display_label, model_cell(term_label, contrast_label))
  }
  age_rows <- if (is.null(age_reference_label)) {
    unlist(
      lapply(
        seq_along(age_terms),
        function(i) {
          c(
            add_estimate(age_terms[i], "", "", bold = TRUE),
            if (i < length(age_terms)) "\\midrule" else NULL
          )
        }
      ),
      use.names = FALSE
    )
  } else {
    c(
      add_ref(age_terms[1], age_reference_label),
      vapply(
        age_display_labels,
        function(label) add_estimate(age_terms[1], label, label),
        character(1)
      )
    )
  }
  risk_rows <- c(
    add_ref("Risque de conflit", risk_reference_label, cells = risk_ref_cells),
    vapply(
      risk_contrast_labels,
      function(label) add_estimate("Risque de conflit", label, label),
      character(1)
    )
  )
  q28_rows <- if (include_q28) {
    c(
      add_ref("Satisfaction Utilisation Argent Public", "Pas du tout satisfait(e)"),
      add_estimate("Satisfaction Utilisation Argent Public", "Plutôt pas satisfait(e)", "Plutôt pas satisfait(e)"),
      add_estimate("Satisfaction Utilisation Argent Public", "Plutôt satisfait(e)", "Plutôt satisfait(e)"),
      add_estimate("Satisfaction Utilisation Argent Public", "Très satisfait(e)", "Très satisfait(e)")
    )
  } else {
    character()
  }

  body <- c(
    add_ref("Acte citoyen", "Pas du tout d’accord"),
    add_estimate("Acte citoyen", "Plutôt pas d’accord", "Plutôt pas d’accord"),
    add_estimate("Acte citoyen", "Plutôt d’accord", "Plutôt d’accord"),
    add_estimate("Acte citoyen", "Tout à fait d’accord", "Tout à fait d’accord"),
    "\\midrule",
    add_ref("Niveau d'imposition", "Juste"),
    add_estimate("Niveau d'imposition", "Trop élevés", "Trop élevés"),
    add_estimate("Niveau d'imposition", "Pas assez", "Pas assez"),
    "\\midrule",
    add_ref("Impôt sur le revenu", "Non"),
    add_estimate("Impôt sur le revenu", "Oui", "Oui"),
    "\\midrule",
    risk_rows,
    "\\midrule",
    q28_rows,
    if (include_q28) "\\midrule" else NULL,
    add_ref("Genre", "Femme"),
    add_estimate("Genre", "Homme", "Homme"),
    "\\midrule",
    age_rows,
    "\\midrule",
    add_ref("Revenus", "< 12k"),
    add_estimate("Revenus", "12k--18k", "12k--18k"),
    add_estimate("Revenus", "18k--24k", "18k--24k"),
    add_estimate("Revenus", "24k--36k", "24k--36k"),
    add_estimate("Revenus", "36k--48k", "36k--48k"),
    add_estimate("Revenus", "48k--60k", "48k--60k"),
    add_estimate("Revenus", "60k--72k", "60k--72k"),
    add_estimate("Revenus", "72k--144k", "72k--144k"),
    add_estimate("Revenus", "144k+", "144k+"),
    add_estimate("Revenus", "NSP", "NSP")
  )
  body <- paste0(body, " \\\\")
  body[body == "\\midrule \\\\"] <- "\\midrule"

  lines <- c(
    "\\begin{table*}[!t]",
    "\\centering",
    "\\scriptsize \\selectfont",
    "\\setlength{\\tabcolsep}{3pt}",
    "",
    "\\begin{talltblr}[",
    paste0("caption={", table_caption, "},"),
    paste0("label={", table_label, "},"),
    paste0("note{}={", note_text, "},"),
    "]{",
    "width=0.97\\textwidth,",
    "\\rowsep = 0pt,",
    "colspec={",
    "Q[l,wd=0.22\\textwidth]",
    "Q[l,wd=0.20\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "Q[c,wd=0.16\\textwidth]",
    "},",
    "column{3,4,5}={halign=c},",
    "column{1,2}={halign=l},",
    "row{1}={font=\\bfseries},",
    "}",
    "\\toprule",
    "& & \\SetCell[c=3]{c} Refus de baisser la dépense de défense \\\\",
    "\\midrule",
    body,
    "\\midrule\\midrule",
    paste0(add_row("Observations", "", c(nobs_gender_only, nobs_baseline, nobs_controls)), " \\\\"),
    paste0(add_row("Contrôle", "", c("Non", "Non", "Oui")), " \\\\"),
    "\\bottomrule",
    "\\end{talltblr}",
    "\\end{table*}"
  )

  writeLines(lines, path, useBytes = TRUE)
}

write_defense_table_html <- function(path,
                                     ame_gender_only,
                                     ame_baseline,
                                     ame_controls,
                                     nobs_gender_only,
                                     nobs_baseline,
                                     nobs_controls,
                                     age_terms = c("Age", "Age\\textsuperscript{2}"),
                                     age_reference_label = NULL,
                                     age_display_labels = NULL,
                                     include_q28 = FALSE,
                                     risk_reference_label = "NSP",
                                     risk_contrast_labels = c("Très élevé", "Assez élevé", "Plutôt faible", "Très faible"),
                                     risk_ref_cells = rep("Ref", 3L),
                                     table_caption = "Les déterminants du refus de baisser la dépense de défense",
                                     note_text = paste(
                                       "Note: Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, la satisfaction et le paiement de l'impôt sur le revenu.",
                                       "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
                                       "la taille d'agglomération, le diplôme et la proximité partisane.",
                                       "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
                                       "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
                                       "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
                                     )) {
  models <- list(ame_gender_only, ame_baseline, ame_controls)
  model_cell <- function(term_label, contrast_label) {
    vapply(models, format_ame_cell_html, character(1), term_label = term_label, contrast_label = contrast_label)
  }

  rows <- data.table(
    term = character(),
    contrast = character(),
    gender_only = character(),
    baseline = character(),
    controls = character(),
    section_start = logical()
  )
  add_row <- function(term_label, contrast_label, cells, section_start = FALSE) {
    rows <<- rbind(
      rows,
      data.table(
        term = term_label,
        contrast = contrast_label,
        gender_only = cells[[1]],
        baseline = cells[[2]],
        controls = cells[[3]],
        section_start = section_start
      )
    )
  }
  current_term <- NULL
  add_ref <- function(term_label, reference_label, section_start = FALSE, cells = rep("Ref", 3L)) {
    current_term <<- term_label
    add_row(term_label, reference_label, cells, section_start = section_start)
  }
  add_estimate <- function(term_label, contrast_label, display_label, section_start = FALSE) {
    lookup_term <- if (nzchar(term_label)) term_label else current_term
    if (is.null(lookup_term)) {
      stop("No current term is available for the HTML table row.", call. = FALSE)
    }
    if (nzchar(term_label)) {
      current_term <<- term_label
    }
    add_row(term_label, display_label, model_cell(lookup_term, contrast_label), section_start = section_start)
  }

  add_ref("Acte citoyen", "Pas du tout d'accord")
  add_estimate("", "Plutôt pas d’accord", "Plutôt pas d'accord")
  add_estimate("", "Plutôt d’accord", "Plutôt d'accord")
  add_estimate("", "Tout à fait d’accord", "Tout à fait d'accord")
  add_ref("Niveau d'imposition", "Juste", section_start = TRUE)
  add_estimate("", "Trop élevés", "Trop élevés")
  add_estimate("", "Pas assez", "Pas assez")
  add_ref("Impôt sur le revenu", "Non", section_start = TRUE)
  add_estimate("", "Oui", "Oui")
  add_ref("Risque de conflit", risk_reference_label, section_start = TRUE, cells = risk_ref_cells)
  for (label in risk_contrast_labels) {
    add_estimate("", label, label)
  }
  if (include_q28) {
    add_ref("Satisfaction Utilisation Argent Public", "Pas du tout satisfait(e)", section_start = TRUE)
    add_estimate("", "Plutôt pas satisfait(e)", "Plutôt pas satisfait(e)")
    add_estimate("", "Plutôt satisfait(e)", "Plutôt satisfait(e)")
    add_estimate("", "Très satisfait(e)", "Très satisfait(e)")
  }
  add_ref("Genre", "Femme", section_start = TRUE)
  add_estimate("", "Homme", "Homme")

  if (is.null(age_reference_label)) {
    for (i in seq_along(age_terms)) {
      add_estimate(age_terms[i], "", "", section_start = i == 1L)
    }
  } else {
    add_ref(age_terms[1], age_reference_label, section_start = TRUE)
    for (label in age_display_labels) {
      add_estimate("", label, label)
    }
  }

  add_ref("Revenus", "< 12k", section_start = TRUE)
  for (label in c("12k--18k", "18k--24k", "24k--36k", "36k--48k", "48k--60k", "60k--72k", "72k--144k", "144k+", "NSP")) {
    add_estimate("", label, label)
  }
  add_row("Observations", "", c(nobs_gender_only, nobs_baseline, nobs_controls), section_start = TRUE)
  add_row("Contrôle", "", c("Non", "Non", "Oui"))

  render_row <- function(i) {
    row <- rows[i]
    cls <- if (row$section_start) " class=\"section\"" else ""
    term <- if (nzchar(row$term)) paste0("<strong>", html_escape(row$term), "</strong>") else ""
    paste0(
      "<tr", cls, ">",
      "<td>", term, "</td>",
      "<td>", html_escape(row$contrast), "</td>",
      "<td>", html_escape(row$gender_only), "</td>",
      "<td>", html_escape(row$baseline), "</td>",
      "<td>", html_escape(row$controls), "</td>",
      "</tr>"
    )
  }

  lines <- c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    paste0("<title>", html_escape(table_caption), "</title>"),
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
    paste0("<caption>", html_escape(table_caption), "</caption>"),
    "<thead>",
    "<tr><th></th><th></th><th colspan=\"3\">Refus de baisser la dépense de défense</th></tr>",
    "<tr><th>Variable</th><th>Modalité</th><th>Modèle 1</th><th>Modèle 2</th><th>Modèle 3</th></tr>",
    "</thead>",
    "<tbody>",
    vapply(seq_len(nrow(rows)), render_row, character(1)),
    "</tbody>",
    "<tfoot>",
    paste0("<tr><td colspan=\"5\">", html_escape(note_text), "</td></tr>"),
    "</tfoot>",
    "</table>",
    "</body>",
    "</html>"
  )

  writeLines(lines, path, useBytes = TRUE)
}

write_modelsummary_outputs <- function(models,
                                       file_stub,
                                       title,
                                       coef_omit = NULL,
                                       notes = NULL) {
  for (ext in c("tex", "html")) {
    output_dir <- if (ext == "tex") table_latex_dir else table_html_dir
    output_file <- file.path(output_dir, paste0(file_stub, ".", ext))
    modelsummary(
      models,
      title = title,
      shape = term + contrast ~ model,
      estimate = "{estimate}{stars}",
      statistic = "({std.error})",
      gof_omit = ".*",
      coef_omit = coef_omit,
      notes = notes,
      output = output_file
    )
    message("Table saved to: ", output_file)
  }
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x <- gsub("#", "\\\\#", x, fixed = TRUE)
  x
}

write_interaction_table_outputs <- function(interaction_table, file_stub) {
  display <- copy(interaction_table)
  display[, `Probabilité prédite` := sprintf("%.1f%%", 100 * predicted)]
  display <- display[
    ,
    .(
      `Confiance État` = as.character(confidence_label),
      `Risque conflit` = as.character(risk_label),
      `Probabilité prédite`
    )
  ]

  title <- "Interaction entre confiance dans l'État et risque de conflit"
  note <- "Probabilité prédite pondérée de refuser une baisse des dépenses militaires en échange d'une baisse d'impôts."

  latex_rows <- apply(
    display,
    1L,
    function(row) paste(paste(latex_escape(row), collapse = " & "), "\\\\")
  )
  tex_lines <- c(
    "\\begin{table}[!t]",
    "\\centering",
    paste0("\\caption{", latex_escape(title), "}"),
    "\\begin{tabular}{lll}",
    "\\toprule",
    paste(paste(latex_escape(names(display)), collapse = " & "), "\\\\"),
    "\\midrule",
    latex_rows,
    "\\bottomrule",
    "\\end{tabular}",
    paste0("\\\\ \\footnotesize{", latex_escape(note), "}"),
    "\\end{table}"
  )
  tex_file <- file.path(table_latex_dir, paste0(file_stub, ".tex"))
  writeLines(tex_lines, tex_file, useBytes = TRUE)
  message("Table saved to: ", tex_file)

  html_rows <- apply(
    display,
    1L,
    function(row) {
      paste0(
        "<tr>",
        paste0("<td>", html_escape(row), "</td>", collapse = ""),
        "</tr>"
      )
    }
  )
  html_lines <- c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    paste0("<title>", html_escape(title), "</title>"),
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:24px;color:#111827;}table{border-collapse:collapse;font-size:13px;}caption{text-align:left;font-weight:700;margin-bottom:8px;}th,td{border-bottom:1px solid #e5e7eb;padding:5px 8px;text-align:left;}tfoot td{font-size:12px;color:#374151;}</style>",
    "</head>",
    "<body>",
    "<table>",
    paste0("<caption>", html_escape(title), "</caption>"),
    "<thead>",
    paste0("<tr>", paste0("<th>", html_escape(names(display)), "</th>", collapse = ""), "</tr>"),
    "</thead>",
    "<tbody>",
    html_rows,
    "</tbody>",
    "<tfoot>",
    paste0("<tr><td colspan=\"", ncol(display), "\">", html_escape(note), "</td></tr>"),
    "</tfoot>",
    "</table>",
    "</body>",
    "</html>"
  )
  html_file <- file.path(table_html_dir, paste0(file_stub, ".html"))
  writeLines(html_lines, html_file, useBytes = TRUE)
  message("Table saved to: ", html_file)
}

# Common settings ----

# `militaryexp_binary` is created upstream from the four-point answer to the
# military-spending question. It equals 1 when the respondent refuses a cut in
# military spending in exchange for lower taxes or lower social contributions.
model_vars <- c("militaryexp_binary", "partisane", "q30_q2", "q35", "q19")

# Table 1 uses a richer complete-case sample because it adds socioeconomic controls.
comparison_vars <- c(
  "militaryexp_binary", "q35", "q19", "q5", "pcs", "matri", "foyer",
  "continuous_age", "q23", "tailleagglo5", "tranche_revenus", "diplome",
  "partisane", "gender", "sat"
)
comparison_char_vars <- c(
  "q35", "q19", "q5", "q23", "pcs", "matri", "foyer",
  "tailleagglo5", "tranche_revenus", "diplome", "partisane", "gender", "sat"
)
adjusted_model_vars <- c(model_vars, "sat")
adjusted_model_char_vars <- c("partisane", "q30_q2", "q35", "q19", "sat")


# Common samples ----

# The analysis focuses on the control group only.
survey2025 <- fread(file.path(data_final_dir, "final_survey2025.csv"))[treatment == 0]
survey2025 <- filter_valid_weights(survey2025)

# Sample used for Table 1: richer controls imply a stricter complete-case filter.
consent_comparison <- copy(survey2025)
consent_comparison[
  ,
  (comparison_char_vars) := lapply(.SD, empty_to_na),
  .SDcols = comparison_char_vars
]
consent_comparison <- consent_comparison[q5 %chin% c("Oui", "Non")]
comparison_sample_vars <- c(comparison_vars, "weights")
consent_comparison <- consent_comparison[
  complete.cases(consent_comparison[, ..comparison_sample_vars])
]
consent_comparison[
  ,
  `:=`(
    acte_citoyen = q23
  )
]
consent_comparison_main <- copy(consent_comparison)
consent_comparison_main[, q28 := empty_to_na(q28)]
consent_comparison_main <- consent_comparison_main[complete.cases(consent_comparison_main[, .(q28)])]

age_mean <- mean(consent_comparison_main$continuous_age)
age_sd <- sd(consent_comparison_main$continuous_age)
if (!is.finite(age_sd) || age_sd <= 0) {
  stop("Cannot normalize continuous_age: standard deviation is zero or missing.", call. = FALSE)
}
consent_comparison_main[
  ,
  `:=`(
    continuous_age_norm = (continuous_age - age_mean) / age_sd,
    continuous_age_norm_sq = ((continuous_age - age_mean) / age_sd)^2
  )
]

# Sample used for the console-only tables: only variables entering the core models.
consent <- copy(survey2025)
consent[
  ,
  c("partisane", "q30_q2", "q35", "q19") := lapply(.SD, empty_to_na),
  .SDcols = c("partisane", "q30_q2", "q35", "q19")
]
model_sample_vars <- c(model_vars, "weights")
consent <- consent[complete.cases(consent[, ..model_sample_vars])]

consent_adjusted <- copy(survey2025)
consent_adjusted[
  ,
  (adjusted_model_char_vars) := lapply(.SD, empty_to_na),
  .SDcols = adjusted_model_char_vars
]
adjusted_model_sample_vars <- c(adjusted_model_vars, "weights")
consent_adjusted <- consent_adjusted[
  complete.cases(consent_adjusted[, ..adjusted_model_sample_vars])
]

# Table 1: Core model plus controlled model ----

# First column omits perceived war risk so the gender AME can be compared to the baseline model.
consent_gender_only <- copy(consent_comparison_main)
consent_gender_only[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_gender_only[, q5 := relevel_q5_factor(q5)]
consent_gender_only[, q28 := relevel_q28_factor(q28)]
consent_gender_only[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_gender_only[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_gender_only[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_gender_only[, sat := relevel_sat_factor(sat)]

consent_military_exp_gender_only <- feglm(
  militaryexp_binary ~ q19 + q5 + continuous_age_norm + continuous_age_norm_sq +
    acte_citoyen + tranche_revenus + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_gender_only
)

ame_gender_only <- avg_slopes(
  consent_military_exp_gender_only,
  newdata = consent_gender_only,
  wts = "weights"
)

# Core specification keeps the attitudinal covariates and gender.
consent_baseline <- add_q35_nsp_collapsed(copy(consent_comparison_main))
consent_baseline[, q35_nsp_collapsed := relevel_q35_nsp_collapsed_factor(q35_nsp_collapsed)]
consent_baseline[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline[, q5 := relevel_q5_factor(q5)]
consent_baseline[, q28 := relevel_q28_factor(q28)]
consent_baseline[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_baseline[, sat := relevel_sat_factor(sat)]

consent_military_exp_baseline <- feglm(
  militaryexp_binary ~ q35_nsp_collapsed + q19 + q5 + continuous_age_norm + continuous_age_norm_sq +
    acte_citoyen + tranche_revenus + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_baseline
)

ame_baseline <- avg_slopes(
  consent_military_exp_baseline,
  newdata = consent_baseline,
  wts = "weights"
)

# Extended specification adds demographics and party proximity to the same core set.
consent_with_controls <- add_q35_nsp_collapsed(copy(consent_comparison_main))
consent_with_controls[, q35_nsp_collapsed := relevel_q35_nsp_collapsed_factor(q35_nsp_collapsed)]
consent_with_controls[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls[, q5 := relevel_q5_factor(q5)]
consent_with_controls[, q28 := relevel_q28_factor(q28)]
consent_with_controls[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_with_controls[, sat := relevel_sat_factor(sat)]
consent_with_controls[
  ,
  `:=`(
    pcs = relevel_factor(pcs, ref = "Retraité", var_name = "pcs"),
    matri = factor(matri),
    foyer = factor(foyer),
    tailleagglo5 = factor(tailleagglo5),
    tranche_revenus = relevel_income_factor(tranche_revenus),
    diplome = factor(diplome)
  )
]

consent_military_exp_controls <- feglm(
  militaryexp_binary ~ q35_nsp_collapsed + q19 + q5 + pcs + matri + foyer +
    continuous_age_norm + continuous_age_norm_sq + acte_citoyen + tailleagglo5 +
    tranche_revenus + diplome + partisane + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_with_controls
)

ame_controls <- avg_slopes(
  consent_military_exp_controls,
  newdata = consent_with_controls,
  wts = "weights"
)

# Detailed conflict-risk specification keeps all q35 responses and uses the
# lowest perceived risk as the reference category.
consent_baseline_q35_detailed <- copy(consent_comparison_main)
consent_baseline_q35_detailed[, q35 := relevel_q35_tres_faible_factor(q35)]
consent_baseline_q35_detailed[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline_q35_detailed[, q5 := relevel_q5_factor(q5)]
consent_baseline_q35_detailed[, q28 := relevel_q28_factor(q28)]
consent_baseline_q35_detailed[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline_q35_detailed[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline_q35_detailed[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_baseline_q35_detailed[, sat := relevel_sat_factor(sat)]

consent_military_exp_baseline_q35_detailed <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + continuous_age_norm + continuous_age_norm_sq +
    acte_citoyen + tranche_revenus + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_baseline_q35_detailed
)

ame_baseline_q35_detailed <- avg_slopes(
  consent_military_exp_baseline_q35_detailed,
  newdata = consent_baseline_q35_detailed,
  wts = "weights"
)

consent_with_controls_q35_detailed <- copy(consent_comparison_main)
consent_with_controls_q35_detailed[, q35 := relevel_q35_tres_faible_factor(q35)]
consent_with_controls_q35_detailed[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls_q35_detailed[, q5 := relevel_q5_factor(q5)]
consent_with_controls_q35_detailed[, q28 := relevel_q28_factor(q28)]
consent_with_controls_q35_detailed[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls_q35_detailed[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls_q35_detailed[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_with_controls_q35_detailed[, sat := relevel_sat_factor(sat)]
consent_with_controls_q35_detailed[
  ,
  `:=`(
    pcs = relevel_factor(pcs, ref = "Retraité", var_name = "pcs"),
    matri = factor(matri),
    foyer = factor(foyer),
    tailleagglo5 = factor(tailleagglo5),
    tranche_revenus = relevel_income_factor(tranche_revenus),
    diplome = factor(diplome)
  )
]

consent_military_exp_controls_q35_detailed <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + pcs + matri + foyer +
    continuous_age_norm + continuous_age_norm_sq + acte_citoyen + tailleagglo5 +
    tranche_revenus + diplome + partisane + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_with_controls_q35_detailed
)

ame_controls_q35_detailed <- avg_slopes(
  consent_military_exp_controls_q35_detailed,
  newdata = consent_with_controls_q35_detailed,
  wts = "weights"
)

# Alternative detailed conflict-risk specification uses NSP as the reference
# category while keeping the same sample and covariates as the detailed table.
consent_baseline_q35_detailed_ref_nsp <- copy(consent_comparison_main)
consent_baseline_q35_detailed_ref_nsp[, q35 := relevel_q35_nsp_factor(q35)]
consent_baseline_q35_detailed_ref_nsp[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline_q35_detailed_ref_nsp[, q5 := relevel_q5_factor(q5)]
consent_baseline_q35_detailed_ref_nsp[, q28 := relevel_q28_factor(q28)]
consent_baseline_q35_detailed_ref_nsp[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline_q35_detailed_ref_nsp[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline_q35_detailed_ref_nsp[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_baseline_q35_detailed_ref_nsp[, sat := relevel_sat_factor(sat)]

consent_military_exp_baseline_q35_detailed_ref_nsp <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + continuous_age_norm + continuous_age_norm_sq +
    acte_citoyen + tranche_revenus + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_baseline_q35_detailed_ref_nsp
)

ame_baseline_q35_detailed_ref_nsp <- avg_slopes(
  consent_military_exp_baseline_q35_detailed_ref_nsp,
  newdata = consent_baseline_q35_detailed_ref_nsp,
  wts = "weights"
)

consent_with_controls_q35_detailed_ref_nsp <- copy(consent_comparison_main)
consent_with_controls_q35_detailed_ref_nsp[, q35 := relevel_q35_nsp_factor(q35)]
consent_with_controls_q35_detailed_ref_nsp[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls_q35_detailed_ref_nsp[, q5 := relevel_q5_factor(q5)]
consent_with_controls_q35_detailed_ref_nsp[, q28 := relevel_q28_factor(q28)]
consent_with_controls_q35_detailed_ref_nsp[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls_q35_detailed_ref_nsp[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls_q35_detailed_ref_nsp[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_with_controls_q35_detailed_ref_nsp[, sat := relevel_sat_factor(sat)]
consent_with_controls_q35_detailed_ref_nsp[
  ,
  `:=`(
    pcs = relevel_factor(pcs, ref = "Retraité", var_name = "pcs"),
    matri = factor(matri),
    foyer = factor(foyer),
    tailleagglo5 = factor(tailleagglo5),
    tranche_revenus = relevel_income_factor(tranche_revenus),
    diplome = factor(diplome)
  )
]

consent_military_exp_controls_q35_detailed_ref_nsp <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + pcs + matri + foyer +
    continuous_age_norm + continuous_age_norm_sq + acte_citoyen + tailleagglo5 +
    tranche_revenus + diplome + partisane + q28 + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_with_controls_q35_detailed_ref_nsp
)

ame_controls_q35_detailed_ref_nsp <- avg_slopes(
  consent_military_exp_controls_q35_detailed_ref_nsp,
  newdata = consent_with_controls_q35_detailed_ref_nsp,
  wts = "weights"
)

ame_gender_only <- order_main_table_rows(clean_ame_labels(ame_gender_only))
ame_baseline <- order_main_table_rows(clean_ame_labels(ame_baseline))
ame_controls <- order_main_table_rows(clean_ame_labels(ame_controls))
ame_baseline_q35_detailed <- order_main_table_rows(clean_ame_labels(ame_baseline_q35_detailed))
ame_controls_q35_detailed <- order_main_table_rows(clean_ame_labels(ame_controls_q35_detailed))
ame_baseline_q35_detailed_ref_nsp <- order_main_table_rows(clean_ame_labels(ame_baseline_q35_detailed_ref_nsp))
ame_controls_q35_detailed_ref_nsp <- order_main_table_rows(clean_ame_labels(ame_controls_q35_detailed_ref_nsp))

write_defense_table(
  path = file.path(table_latex_dir, "defense_ame_baseline_controls.tex"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline,
  ame_controls = ame_controls,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline)),
  nobs_controls = as.character(nobs(consent_military_exp_controls)),
  include_q28 = TRUE,
  risk_reference_label = "Risque faible",
  risk_contrast_labels = c("Risque élevé", "NSP"),
  risk_ref_cells = c("", "\\textit{Ref}", "\\textit{Ref}"),
  note_text = paste(
    "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent le risque de conflit regroupé en risque faible, risque élevé et NSP, avec Risque faible comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

write_defense_table_html(
  path = file.path(table_html_dir, "defense_ame_baseline_controls.html"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline,
  ame_controls = ame_controls,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline)),
  nobs_controls = as.character(nobs(consent_military_exp_controls)),
  include_q28 = TRUE,
  risk_reference_label = "Risque faible",
  risk_contrast_labels = c("Risque élevé", "NSP"),
  risk_ref_cells = c("", "Ref", "Ref"),
  note_text = paste(
    "Note: Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent le risque de conflit regroupé en risque faible, risque élevé et NSP, avec Risque faible comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

message("Table saved to: ", file.path(table_latex_dir, "defense_ame_baseline_controls.tex"))
message("Table saved to: ", file.path(table_html_dir, "defense_ame_baseline_controls.html"))

write_defense_table(
  path = file.path(table_latex_dir, "defense_ame_baseline_controls_q35_detailed.tex"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline_q35_detailed,
  ame_controls = ame_controls_q35_detailed,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_q35_detailed)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_q35_detailed)),
  include_q28 = TRUE,
  risk_reference_label = "Très faible",
  risk_contrast_labels = c("Plutôt faible", "Assez élevé", "Très élevé", "NSP"),
  risk_ref_cells = c("", "\\textit{Ref}", "\\textit{Ref}"),
  table_caption = "Les déterminants du refus de baisser la dépense de défense (q35 détaillé)",
  table_label = "tab:deter_consent_def_q35_detailed",
  note_text = paste(
    "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent les cinq réponses au risque de conflit, avec Très faible comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

write_defense_table_html(
  path = file.path(table_html_dir, "defense_ame_baseline_controls_q35_detailed.html"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline_q35_detailed,
  ame_controls = ame_controls_q35_detailed,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_q35_detailed)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_q35_detailed)),
  include_q28 = TRUE,
  risk_reference_label = "Très faible",
  risk_contrast_labels = c("Plutôt faible", "Assez élevé", "Très élevé", "NSP"),
  risk_ref_cells = c("", "Ref", "Ref"),
  table_caption = "Les déterminants du refus de baisser la dépense de défense (q35 détaillé)",
  note_text = paste(
    "Note: Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent les cinq réponses au risque de conflit, avec Très faible comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

message("Table saved to: ", file.path(table_latex_dir, "defense_ame_baseline_controls_q35_detailed.tex"))
message("Table saved to: ", file.path(table_html_dir, "defense_ame_baseline_controls_q35_detailed.html"))

write_defense_table(
  path = file.path(table_latex_dir, "defense_ame_baseline_controls_q35_detailed_ref_nsp.tex"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline_q35_detailed_ref_nsp,
  ame_controls = ame_controls_q35_detailed_ref_nsp,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_q35_detailed_ref_nsp)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_q35_detailed_ref_nsp)),
  include_q28 = TRUE,
  risk_reference_label = "NSP",
  risk_contrast_labels = c("Très élevé", "Assez élevé", "Plutôt faible", "Très faible"),
  risk_ref_cells = c("", "\\textit{Ref}", "\\textit{Ref}"),
  table_caption = "Les déterminants du refus de baisser la dépense de défense (q35 détaillé, réf. NSP)",
  table_label = "tab:deter_consent_def_q35_detailed_ref_nsp",
  note_text = paste(
    "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent les cinq réponses au risque de conflit, avec NSP comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

write_defense_table_html(
  path = file.path(table_html_dir, "defense_ame_baseline_controls_q35_detailed_ref_nsp.html"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline_q35_detailed_ref_nsp,
  ame_controls = ame_controls_q35_detailed_ref_nsp,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_q35_detailed_ref_nsp)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_q35_detailed_ref_nsp)),
  include_q28 = TRUE,
  risk_reference_label = "NSP",
  risk_contrast_labels = c("Très élevé", "Assez élevé", "Plutôt faible", "Très faible"),
  risk_ref_cells = c("", "Ref", "Ref"),
  table_caption = "Les déterminants du refus de baisser la dépense de défense (q35 détaillé, réf. NSP)",
  note_text = paste(
    "Note: Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus, l'acte citoyen, Satisfaction Utilisation Argent Public, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Les modèles 2 et 3 ajoutent les cinq réponses au risque de conflit, avec NSP comme catégorie de référence.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

message("Table saved to: ", file.path(table_latex_dir, "defense_ame_baseline_controls_q35_detailed_ref_nsp.tex"))
message("Table saved to: ", file.path(table_html_dir, "defense_ame_baseline_controls_q35_detailed_ref_nsp.html"))


# Table 1B: Core model plus controlled model with age2 ----

consent_gender_only_altage <- copy(consent_comparison)
consent_gender_only_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_gender_only_altage[, q5 := relevel_q5_factor(q5)]
consent_gender_only_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_gender_only_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_gender_only_altage[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_gender_only_altage[, age2 := relevel_age2_factor(age2)]
consent_gender_only_altage[, sat := relevel_sat_factor(sat)]

consent_military_exp_gender_only_altage <- feglm(
  militaryexp_binary ~ q19 + q5 + age2 + acte_citoyen + tranche_revenus + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_gender_only_altage
)

ame_gender_only_altage <- avg_slopes(
  consent_military_exp_gender_only_altage,
  newdata = consent_gender_only_altage,
  wts = "weights"
)

consent_baseline_altage <- copy(consent_comparison)
consent_baseline_altage[, q35 := relevel_q35_nsp_factor(q35)]
consent_baseline_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline_altage[, q5 := relevel_q5_factor(q5)]
consent_baseline_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline_altage[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_baseline_altage[, age2 := relevel_age2_factor(age2)]
consent_baseline_altage[, sat := relevel_sat_factor(sat)]

consent_military_exp_baseline_altage <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + age2 + acte_citoyen + tranche_revenus + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_baseline_altage
)

ame_baseline_altage <- avg_slopes(
  consent_military_exp_baseline_altage,
  newdata = consent_baseline_altage,
  wts = "weights"
)

consent_with_controls_altage <- copy(consent_comparison)
consent_with_controls_altage[, q35 := relevel_q35_nsp_factor(q35)]
consent_with_controls_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls_altage[, q5 := relevel_q5_factor(q5)]
consent_with_controls_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls_altage[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_with_controls_altage[, age2 := relevel_age2_factor(age2)]
consent_with_controls_altage[, sat := relevel_sat_factor(sat)]
consent_with_controls_altage[
  ,
  `:=`(
    pcs = relevel_factor(pcs, ref = "Retraité", var_name = "pcs"),
    matri = factor(matri),
    foyer = factor(foyer),
    tailleagglo5 = factor(tailleagglo5),
    tranche_revenus = relevel_income_factor(tranche_revenus),
    diplome = factor(diplome)
  )
]

consent_military_exp_controls_altage <- feglm(
  militaryexp_binary ~ q35 + q19 + q5 + pcs + matri + foyer +
    age2 + acte_citoyen + tailleagglo5 + tranche_revenus +
    diplome + partisane + gender + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_with_controls_altage
)

ame_controls_altage <- avg_slopes(
  consent_military_exp_controls_altage,
  newdata = consent_with_controls_altage,
  wts = "weights"
)
ame_gender_only_altage <- order_main_table_rows(clean_ame_labels(ame_gender_only_altage))
ame_baseline_altage <- order_main_table_rows(clean_ame_labels(ame_baseline_altage))
ame_controls_altage <- order_main_table_rows(clean_ame_labels(ame_controls_altage))

write_defense_table(
  path = file.path(table_latex_dir, "defense_ame_baseline_controls_altage.tex"),
  ame_gender_only = ame_gender_only_altage,
  ame_baseline = ame_baseline_altage,
  ame_controls = ame_controls_altage,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only_altage)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_altage)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_altage)),
  age_terms = "age_2",
  age_reference_label = "18 à 24 ans",
  age_display_labels = c(
    "25 à 29 ans",
    "30 à 34 ans",
    "35 à 39 ans",
    "40 à 44 ans",
    "45 à 49 ans",
    "50 à 54 ans",
    "55 à 59 ans",
    "60 à 64 ans",
    "65 à 69 ans",
    "70 ans et plus"
  ),
  note_text = paste(
    "\\textbf{Note:} Les trois modèles incluent le genre, la variable catégorielle age_2, les revenus, l'acte citoyen, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

write_defense_table_html(
  path = file.path(table_html_dir, "defense_ame_baseline_controls_altage.html"),
  ame_gender_only = ame_gender_only_altage,
  ame_baseline = ame_baseline_altage,
  ame_controls = ame_controls_altage,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only_altage)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline_altage)),
  nobs_controls = as.character(nobs(consent_military_exp_controls_altage)),
  age_terms = "age_2",
  age_reference_label = "18 à 24 ans",
  age_display_labels = c(
    "25 à 29 ans",
    "30 à 34 ans",
    "35 à 39 ans",
    "40 à 44 ans",
    "45 à 49 ans",
    "50 à 54 ans",
    "55 à 59 ans",
    "60 à 64 ans",
    "65 à 69 ans",
    "70 ans et plus"
  ),
  note_text = paste(
    "Note: Les trois modèles incluent le genre, la variable catégorielle age_2, les revenus, l'acte citoyen, la satisfaction et le paiement de l'impôt sur le revenu.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

message("Table saved to: ", file.path(table_latex_dir, "defense_ame_baseline_controls_altage.tex"))
message("Table saved to: ", file.path(table_html_dir, "defense_ame_baseline_controls_altage.html"))


# Table 2A: Full q35 model, partisan reference = Aucun ----

# This console-only table keeps the detailed q35 categories and hides the party coefficients.
consent_q35_tres_eleve <- copy(consent_adjusted)
consent_q35_tres_eleve[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_q35_tres_eleve[, q30_q2 := relevel_q30_factor(q30_q2)]
consent_q35_tres_eleve[, q35 := relevel_q35_nsp_factor(q35)]
consent_q35_tres_eleve[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_q35_tres_eleve[, sat := relevel_sat_factor(sat)]

consent_military_exp_q35_tres_eleve <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + partisane + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_q35_tres_eleve
)

ame_q35_tres_eleve <- avg_slopes(
  consent_military_exp_q35_tres_eleve,
  newdata = consent_q35_tres_eleve,
  wts = "weights"
)
ame_q35_tres_eleve <- order_console_ame_rows(ame_q35_tres_eleve)

write_modelsummary_outputs(
  models = list("AME (Aucun)" = ame_q35_tres_eleve),
  file_stub = "defense_table2a_ame_q35_ref_nsp",
  title = "Table 2A. Modèle q35 détaillé, référence partisane Aucun",
  coef_omit = "partisane",
  notes = "Average marginal effects from a weighted probit model using survey weights. q35 reference category is NSP. The model adjusts for tax-level views, party proximity, and satisfaction. Heteroskedasticity-robust SE in parentheses."
)


# Table 2B: Full q35 model, partisan reference = Renaissance ----

# This companion console table changes the party reference to compare party distances to Renaissance.
consent_partisane_renaissance <- copy(consent_adjusted)
consent_partisane_renaissance[, partisane := relevel_factor(partisane, ref = "Renaissance (ex-La République En Marche)", var_name = "partisane")]
consent_partisane_renaissance[, q30_q2 := relevel_q30_factor(q30_q2)]
consent_partisane_renaissance[, q35 := relevel_q35_nsp_factor(q35)]
consent_partisane_renaissance[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_partisane_renaissance[, sat := relevel_sat_factor(sat)]

consent_military_exp_partisane_renaissance <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + partisane + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_partisane_renaissance
)

ame_partisane_renaissance <- avg_slopes(
  consent_military_exp_partisane_renaissance,
  newdata = consent_partisane_renaissance,
  wts = "weights"
)
ame_partisane_renaissance <- order_console_ame_rows(ame_partisane_renaissance)

write_modelsummary_outputs(
  models = list("AME (Renaissance)" = ame_partisane_renaissance),
  file_stub = "defense_table2b_ame_partisane_renaissance_q35_ref_nsp",
  title = "Table 2B. Modèle q35 détaillé, référence partisane Renaissance",
  notes = "Average marginal effects from a weighted probit model using survey weights. q35 reference category is NSP. The model adjusts for tax-level views, party proximity, and satisfaction. Heteroskedasticity-robust SE in parentheses."
)


# Table 3: Binary risk specification ----

# Here q35 is simplified to a high-versus-low risk indicator.
consent_q35_binary <- copy(consent)
consent_q35_binary[
  ,
  q35_binary := fcase(
    q35 %chin% c("Très élevé", "Assez élevé"), "Risque eleve",
    q35 %chin% c("Plutôt faible", "Très faible"), "Risque faible",
    default = NA_character_
  )
]
consent_q35_binary <- consent_q35_binary[!is.na(q35_binary)]
consent_q35_binary[, q30_q2 := relevel_q30_factor(q30_q2)]
consent_q35_binary[, q35_binary := relevel_factor(q35_binary, ref = "Risque faible", var_name = "q35_binary")]

consent_military_exp_q35_binary <- feglm(
  militaryexp_binary ~ q30_q2 + q35_binary,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_q35_binary
)

ame_q35_binary <- avg_slopes(
  consent_military_exp_q35_binary,
  newdata = consent_q35_binary,
  wts = "weights"
)
ame_q35_binary <- order_console_ame_rows(ame_q35_binary)

write_modelsummary_outputs(
  models = list("AME (q35 binaire)" = ame_q35_binary),
  file_stub = "defense_table3_ame_q35_binary",
  title = "Table 3. Spécification binaire du risque de conflit",
  notes = "Average marginal effects from a weighted probit model using survey weights. q35 is coded as high risk (Tres eleve/Assez eleve) versus low risk (Plutot faible/Tres faible). 'Vous ne savez pas' is excluded. Heteroskedasticity-robust SE in parentheses."
)


# Table 4: Confidence x risk interaction ----

# This final console table reports fitted probabilities for every confidence x binary-risk cell.
consent_interaction <- add_q35_binary(copy(consent_adjusted))
consent_interaction[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_interaction[, q30_q2 := relevel_q30_factor(q30_q2)]
consent_interaction[, q35_binary := factor(q35_binary, levels = c("Risque faible", "Risque élevé"))]
consent_interaction[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_interaction[, sat := relevel_sat_factor(sat)]

consent_military_exp_interaction <- feglm(
  militaryexp_binary ~ q30_q2 * q35_binary + q19 + partisane + sat,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_interaction
)

interaction_table <- build_interaction_table(
  model = consent_military_exp_interaction,
  data = consent_interaction
)

write_interaction_table_outputs(
  interaction_table = interaction_table,
  file_stub = "defense_table4_interaction_q30_q35_binary"
)

print_interaction_results(interaction_table)
