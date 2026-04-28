# This script estimates the main defense-spending consent models and
# produces one exported LaTeX table plus several console-only tables.

# Setup ----
library(data.table)
library(fixest)
library(marginaleffects)
library(modelsummary)

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)


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
  ame$term[ame$term == "gender"] <- "Genre"
  ame$term[ame$term == "continuous_age"] <- "Age"
  ame$term[ame$term == "continuous_age_sq"] <- "Age\\textsuperscript{2}"
  ame$term[ame$term == "age2"] <- "age_2"
  ame$term[ame$term == "acte_citoyen"] <- "Acte citoyen"
  ame$term[ame$term == "tranche_revenus"] <- "Revenus"

  ame$contrast[ame$contrast == "Pas assez élevés - Ni trop, ni pas assez élevés"] <- "Pas assez - Juste"
  ame$contrast[ame$contrast == "Trop élevés - Ni trop, ni pas assez élevés"] <- "Trop élevés - Juste"
  ame$contrast[ame$contrast == "Plutôt confiance - Pas du tout confiance"] <- "Plutôt conf. - Pas du tout"
  ame$contrast[ame$contrast == "Plutôt pas confiance - Pas du tout confiance"] <- "Plutôt pas conf. - Pas du tout"
  ame$contrast[ame$contrast == "Tout à fait confiance - Pas du tout confiance"] <- "Tout à fait conf. - Pas du tout"
  ame$contrast[ame$contrast == "Vous ne savez pas - Très élevé"] <- "NSP - Très élevé"
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
    "Confiance dans l'État",
    "Acte citoyen",
    "Niveau d'imposition",
    "Risque de conflit",
    "Genre",
    "age_2",
    "Age",
    "Age\\textsuperscript{2}",
    "Revenus"
  )
  contrast_order <- list(
    "Confiance dans l'État" = c("Plutôt pas conf.", "Plutôt conf.", "Tout à fait conf."),
    "Acte citoyen" = c("Plutôt pas d’accord", "Plutôt d’accord", "Tout à fait d’accord"),
    "Niveau d'imposition" = c("Trop élevés", "Pas assez"),
    "Risque de conflit" = c("Assez élevé", "Plutôt faible", "Très faible", "NSP"),
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
    "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus et l'acte citoyen.",
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
                                note_text = paste(
                                  "\\textbf{Note:} Les trois modèles incluent le genre, l'âge, l'âge au carré, les revenus et l'acte citoyen.",
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
  add_ref <- function(term_label, reference_label) {
    add_row(paste0("\\textbf{", term_label, "}"), reference_label, rep("\\textit{Ref}", 3L))
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

  body <- c(
    add_ref("Confiance dans l'État", "Pas du tout confiance"),
    add_estimate("Confiance dans l'État", "Plutôt pas conf.", "Plutôt pas conf."),
    add_estimate("Confiance dans l'État", "Plutôt conf.", "Plutôt conf."),
    add_estimate("Confiance dans l'État", "Tout à fait conf.", "Tout à fait conf."),
    "\\midrule",
    add_ref("Acte citoyen", "Pas du tout d’accord"),
    add_estimate("Acte citoyen", "Plutôt pas d’accord", "Plutôt pas d’accord"),
    add_estimate("Acte citoyen", "Plutôt d’accord", "Plutôt d’accord"),
    add_estimate("Acte citoyen", "Tout à fait d’accord", "Tout à fait d’accord"),
    "\\midrule",
    add_ref("Niveau d'imposition", "Juste"),
    add_estimate("Niveau d'imposition", "Trop élevés", "Trop élevés"),
    add_estimate("Niveau d'imposition", "Pas assez", "Pas assez"),
    "\\midrule",
    add_ref("Risque de conflit", "Très élevé"),
    add_estimate("Risque de conflit", "Assez élevé", "Assez élevé"),
    add_estimate("Risque de conflit", "Plutôt faible", "Plutôt faible"),
    add_estimate("Risque de conflit", "Très faible", "Très faible"),
    add_estimate("Risque de conflit", "NSP", "NSP"),
    "\\midrule",
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
    "caption={Les déterminants du refus de baisser la dépense de défense},",
    "label={tab:deter_consent_def},",
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

# Common settings ----

# `militaryexp_binary` is created upstream from the four-point answer to the
# military-spending question. It equals 1 when the respondent refuses a cut in
# military spending in exchange for lower taxes or lower social contributions.
model_vars <- c("militaryexp_binary", "partisane", "q30_q2", "q35", "q19")

# Table 1 uses a richer complete-case sample because it adds socioeconomic controls.
comparison_vars <- c(
  "militaryexp_binary", "q30_q2", "q35", "q19", "pcs", "matri", "foyer",
  "continuous_age", "q23", "tailleagglo5", "tranche_revenus", "diplome", "partisane", "gender"
)
comparison_char_vars <- c(
  "q30_q2", "q35", "q19", "q23", "pcs", "matri", "foyer",
  "tailleagglo5", "tranche_revenus", "diplome", "partisane", "gender"
)


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
comparison_sample_vars <- c(comparison_vars, "weights")
consent_comparison <- consent_comparison[
  complete.cases(consent_comparison[, ..comparison_sample_vars])
]
consent_comparison[
  ,
  `:=`(
    continuous_age_sq = continuous_age^2,
    acte_citoyen = q23
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

# Table 1: Core model plus controlled model ----

# First column omits perceived war risk so the gender AME can be compared to the baseline model.
consent_gender_only <- copy(consent_comparison)
consent_gender_only[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_gender_only[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_gender_only[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_gender_only[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_gender_only[, tranche_revenus := relevel_income_factor(tranche_revenus)]

consent_military_exp_gender_only <- feglm(
  militaryexp_binary ~ q30_q2 + q19 + continuous_age + continuous_age_sq +
    acte_citoyen + tranche_revenus + gender,
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
consent_baseline <- copy(consent_comparison)
consent_baseline[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_baseline[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_baseline[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline[, tranche_revenus := relevel_income_factor(tranche_revenus)]

consent_military_exp_baseline <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + continuous_age + continuous_age_sq +
    acte_citoyen + tranche_revenus + gender,
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
consent_with_controls <- copy(consent_comparison)
consent_with_controls[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_with_controls[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_with_controls[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
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
  militaryexp_binary ~ q30_q2 + q35 + q19 + pcs + matri + foyer +
    continuous_age + continuous_age_sq + acte_citoyen + tailleagglo5 +
    tranche_revenus + diplome + partisane + gender,
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
ame_gender_only <- order_main_table_rows(clean_ame_labels(ame_gender_only))
ame_baseline <- order_main_table_rows(clean_ame_labels(ame_baseline))
ame_controls <- order_main_table_rows(clean_ame_labels(ame_controls))

write_defense_table(
  path = file.path(table_dir, "defense_ame_baseline_controls.tex"),
  ame_gender_only = ame_gender_only,
  ame_baseline = ame_baseline,
  ame_controls = ame_controls,
  nobs_gender_only = as.character(nobs(consent_military_exp_gender_only)),
  nobs_baseline = as.character(nobs(consent_military_exp_baseline)),
  nobs_controls = as.character(nobs(consent_military_exp_controls))
)

message("Table saved to: ", file.path(table_dir, "defense_ame_baseline_controls.tex"))


# Table 1B: Core model plus controlled model with age2 ----

consent_gender_only_altage <- copy(consent_comparison)
consent_gender_only_altage[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_gender_only_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_gender_only_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_gender_only_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_gender_only_altage[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_gender_only_altage[, age2 := relevel_age2_factor(age2)]

consent_military_exp_gender_only_altage <- feglm(
  militaryexp_binary ~ q30_q2 + q19 + age2 + acte_citoyen + tranche_revenus + gender,
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
consent_baseline_altage[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_baseline_altage[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_baseline_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_baseline_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_baseline_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_baseline_altage[, tranche_revenus := relevel_income_factor(tranche_revenus)]
consent_baseline_altage[, age2 := relevel_age2_factor(age2)]

consent_military_exp_baseline_altage <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + age2 + acte_citoyen + tranche_revenus + gender,
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
consent_with_controls_altage[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_with_controls_altage[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_with_controls_altage[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_with_controls_altage[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_with_controls_altage[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_with_controls_altage[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_with_controls_altage[, age2 := relevel_age2_factor(age2)]
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
  militaryexp_binary ~ q30_q2 + q35 + q19 + pcs + matri + foyer +
    age2 + acte_citoyen + tailleagglo5 + tranche_revenus +
    diplome + partisane + gender,
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
  path = file.path(table_dir, "defense_ame_baseline_controls_altage.tex"),
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
    "\\textbf{Note:} Les trois modèles incluent le genre, la variable catégorielle age_2, les revenus et l'acte citoyen.",
    "Le modèle avec contrôles ajoute la catégorie socio-professionnelle, la situation matrimoniale, le foyer,",
    "la taille d'agglomération, le diplôme et la proximité partisane.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )
)

message("Table saved to: ", file.path(table_dir, "defense_ame_baseline_controls_altage.tex"))


# Table 2A: Full q35 model, partisan reference = Aucun ----

# This console-only table keeps the detailed q35 categories and hides the party coefficients.
consent_q35_tres_eleve <- copy(consent)
consent_q35_tres_eleve[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_q35_tres_eleve[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_q35_tres_eleve[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_q35_tres_eleve[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]

consent_military_exp_q35_tres_eleve <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + partisane,
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

modelsummary(
  list("AME (Aucun)" = ame_q35_tres_eleve),
  shape = term + contrast ~ model,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = ".*",
  coef_omit = "partisane",
  notes = "Average marginal effects from a weighted probit model using survey weights. Heteroskedasticity-robust SE in parentheses."
)


# Table 2B: Full q35 model, partisan reference = Renaissance ----

# This companion console table changes the party reference to compare party distances to Renaissance.
consent_partisane_renaissance <- copy(consent)
consent_partisane_renaissance[, partisane := relevel_factor(partisane, ref = "Renaissance (ex-La République En Marche)", var_name = "partisane")]
consent_partisane_renaissance[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
consent_partisane_renaissance[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
consent_partisane_renaissance[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]

consent_military_exp_partisane_renaissance <- feglm(
  militaryexp_binary ~ q30_q2 + q35 + q19 + partisane,
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

modelsummary(
  list("AME (Renaissance)" = ame_partisane_renaissance),
  shape = term + contrast ~ model,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = ".*",
  notes = "Average marginal effects from a weighted probit model using survey weights. Heteroskedasticity-robust SE in parentheses."
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
consent_q35_binary[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
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

modelsummary(
  list("AME (q35 binaire)" = ame_q35_binary),
  shape = term + contrast ~ model,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  gof_omit = ".*",
  notes = "Average marginal effects from a weighted probit model using survey weights. q35 is coded as high risk (Tres eleve/Assez eleve) versus low risk (Plutot faible/Tres faible). 'Vous ne savez pas' is excluded. Heteroskedasticity-robust SE in parentheses."
)


# Table 4: Confidence x risk interaction ----

# This final console table reports fitted probabilities for every confidence x binary-risk cell.
consent_interaction <- add_q35_binary(copy(consent))
consent_interaction[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_interaction[, q30_q2 := factor(q30_q2, levels = c(
  "Pas du tout confiance",
  "Plutôt pas confiance",
  "Plutôt confiance",
  "Tout à fait confiance"
))]
consent_interaction[, q35_binary := factor(q35_binary, levels = c("Risque faible", "Risque élevé"))]
consent_interaction[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]

consent_military_exp_interaction <- feglm(
  militaryexp_binary ~ q30_q2 * q35_binary + q19 + partisane,
  family = binomial(link = "probit"),
  vcov = "hetero",
  weights = ~ weights,
  data = consent_interaction
)

interaction_table <- build_interaction_table(
  model = consent_military_exp_interaction,
  data = consent_interaction
)

print_interaction_results(interaction_table)
