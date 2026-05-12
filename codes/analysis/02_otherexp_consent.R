# This code compares the main controlled defense-spending model across expenditure outcomes.

library(data.table)
library(fixest)
library(marginaleffects)

data_final_dir <- path_data_final()
table_dir <- output_path("tables")
table_latex_dir <- file.path(table_dir, "latex")
table_html_dir <- file.path(table_dir, "html")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_latex_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_html_dir, recursive = TRUE, showWarnings = FALSE)

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
  sat_levels <- c(
    "Pas du tout satisfait(e)",
    "Plutôt pas satisfait(e)",
    "Plutôt satisfait(e)",
    "Très satisfait(e)"
  )
  x_chr <- as.character(x)
  unexpected <- setdiff(unique(na.omit(x_chr)), sat_levels)

  if (length(unexpected) > 0L) {
    stop(
      "Unexpected level(s) in ", var_name, ": ",
      paste(unexpected, collapse = ", "),
      call. = FALSE
    )
  }

  if (!sat_levels[1] %chin% x_chr) {
    stop("Reference level '", sat_levels[1], "' not found in ", var_name, call. = FALSE)
  }

  factor(x_chr, levels = sat_levels)
}

relevel_sat_factor <- function(x) {
  relevel_satisfaction_factor(x, var_name = "sat")
}

relevel_q28_factor <- function(x) {
  relevel_satisfaction_factor(x, var_name = "q28")
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
  ame$term[ame$term == "q5"] <- "Impôt sur le revenu"
  ame$term[ame$term == "q28"] <- "Satisfaction Utilisation Argent Public"
  ame$term[ame$term == "gender"] <- "Genre"
  ame$term[ame$term == "continuous_age"] <- "Age"
  ame$term[ame$term == "continuous_age_sq"] <- "Age\\textsuperscript{2}"
  ame$term[ame$term == "continuous_age_norm"] <- "Age"
  ame$term[ame$term == "continuous_age_norm_sq"] <- "Age\\textsuperscript{2}"
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
    " - Très élevé",
    " - Très faible",
    " - Non",
    " - Pas du tout satisfait(e)",
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

order_otherexp_table_rows <- function(ame) {
  term_order <- c(
    "Acte citoyen",
    "Niveau d'imposition",
    "Impôt sur le revenu",
    "Risque de conflit",
    "Satisfaction Utilisation Argent Public",
    "Genre",
    "Age",
    "Age\\textsuperscript{2}",
    "Revenus"
  )
  contrast_order <- list(
    "Acte citoyen" = c("Plutôt pas d’accord", "Plutôt d’accord", "Tout à fait d’accord"),
    "Niveau d'imposition" = c("Trop élevés", "Pas assez"),
    "Impôt sur le revenu" = "Oui",
    "Risque de conflit" = c("Plutôt faible", "Assez élevé", "Très élevé", "NSP"),
    "Satisfaction Utilisation Argent Public" = c("Plutôt pas satisfait(e)", "Plutôt satisfait(e)", "Très satisfait(e)"),
    "Genre" = "Homme",
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

fit_comparison_model <- function(data, outcome) {
  data <- copy(data)
  model_formula <- as.formula(
    sprintf(
      "%s ~ q35 + q19 + q5 + pcs + matri + foyer + continuous_age_norm + continuous_age_norm_sq + acte_citoyen + tailleagglo5 + tranche_revenus + diplome + partisane + q28 + gender + sat",
      outcome
    )
  )

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

write_otherexp_table <- function(path, ames, nobs_by_outcome) {
  model_cell <- function(term_label, contrast_label) {
    vapply(ames, format_ame_cell, character(1), term_label = term_label, contrast_label = contrast_label)
  }
  add_row <- function(term_label, contrast_label, cells) {
    paste(c(term_label, contrast_label, cells), collapse = " & ")
  }
  add_ref <- function(term_label, reference_label) {
    add_row(paste0("\\textbf{", term_label, "}"), reference_label, rep("\\textit{Ref}", length(ames)))
  }
  add_estimate <- function(term_label, contrast_label, display_label, bold = FALSE) {
    first_cell <- if (bold) paste0("\\textbf{", term_label, "}") else ""
    add_row(first_cell, display_label, model_cell(term_label, contrast_label))
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
    add_ref("Risque de conflit", "Très faible"),
    add_estimate("Risque de conflit", "Plutôt faible", "Plutôt faible"),
    add_estimate("Risque de conflit", "Assez élevé", "Assez élevé"),
    add_estimate("Risque de conflit", "Très élevé", "Très élevé"),
    add_estimate("Risque de conflit", "NSP", "NSP"),
    "\\midrule",
    add_ref("Satisfaction Utilisation Argent Public", "Pas du tout satisfait(e)"),
    add_estimate("Satisfaction Utilisation Argent Public", "Plutôt pas satisfait(e)", "Plutôt pas satisfait(e)"),
    add_estimate("Satisfaction Utilisation Argent Public", "Plutôt satisfait(e)", "Plutôt satisfait(e)"),
    add_estimate("Satisfaction Utilisation Argent Public", "Très satisfait(e)", "Très satisfait(e)"),
    "\\midrule",
    add_ref("Genre", "Femme"),
    add_estimate("Genre", "Homme", "Homme"),
    "\\midrule",
    add_estimate("Age", "", "", bold = TRUE),
    "\\midrule",
    add_estimate("Age\\textsuperscript{2}", "", "", bold = TRUE),
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

  note_text <- paste(
    "\\textbf{Note:} Les quatre modèles sont estimés sur un échantillon commun.",
    "Ils reprennent la spécification du modèle 3 de la table détaillée de défense : risque de conflit détaillé, niveau d'imposition,",
    "paiement de l'impôt sur le revenu, Satisfaction Utilisation Argent Public,",
    "la catégorie socio-professionnelle, la situation matrimoniale, le foyer, l'âge, l'âge au carré,",
    "l'acte citoyen, la taille d'agglomération, les revenus, le diplôme, la proximité partisane,",
    "le genre et la satisfaction.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l’hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )

  lines <- c(
    "\\begin{table*}[!t]",
    "\\centering",
    "\\scriptsize \\selectfont",
    "\\setlength{\\tabcolsep}{2pt}",
    "",
    "\\begin{talltblr}[",
    "caption={Les déterminants du refus de baisser les dépenses publiques par poste},",
    "label={tab:deter_consent_depenses},",
    paste0("note{}={", note_text, "},"),
    "]{",
    "width=0.98\\textwidth,",
    "\\rowsep = 0pt,",
    "colspec={",
    "Q[l,wd=0.17\\textwidth]",
    "Q[l,wd=0.16\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "Q[c,wd=0.15\\textwidth]",
    "},",
    "column{3,4,5,6}={halign=c},",
    "column{1,2}={halign=l},",
    "row{1,2}={font=\\bfseries},",
    "}",
    "\\toprule",
    "& & \\SetCell[c=4]{c} Refus de baisser la dépense considérée \\\\",
    paste0("& & ", paste(names(ames), collapse = " & "), " \\\\"),
    "\\midrule",
    body,
    "\\midrule\\midrule",
    paste0(add_row("Observations", "", as.character(nobs_by_outcome)), " \\\\"),
    paste0(add_row("Contrôle", "", rep("Oui", length(ames))), " \\\\"),
    "\\bottomrule",
    "\\end{talltblr}",
    "\\end{table*}"
  )

  writeLines(lines, path, useBytes = TRUE)
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

write_otherexp_table_html <- function(path, ames, nobs_by_outcome) {
  model_cell <- function(term_label, contrast_label) {
    vapply(ames, format_ame_cell_html, character(1), term_label = term_label, contrast_label = contrast_label)
  }

  rows <- data.table(
    term = character(),
    contrast = character(),
    section_start = logical()
  )
  for (outcome_label in names(ames)) {
    rows[, (outcome_label) := character()]
  }

  add_row <- function(term_label, contrast_label, cells, section_start = FALSE) {
    row <- data.table(
      term = term_label,
      contrast = contrast_label,
      section_start = section_start
    )
    for (outcome_label in names(ames)) {
      row[, (outcome_label) := cells[[outcome_label]]]
    }
    rows <<- rbind(rows, row, fill = TRUE)
  }
  add_ref <- function(term_label, reference_label) {
    add_row(
      term_label,
      reference_label,
      setNames(rep("Ref", length(ames)), names(ames)),
      section_start = nrow(rows) > 0L
    )
  }
  add_estimate <- function(term_label, contrast_label, display_label, bold = FALSE) {
    add_row(
      if (bold) term_label else "",
      display_label,
      model_cell(term_label, contrast_label),
      section_start = bold
    )
  }

  add_ref("Acte citoyen", "Pas du tout d'accord")
  add_estimate("Acte citoyen", "Plutôt pas d’accord", "Plutôt pas d'accord")
  add_estimate("Acte citoyen", "Plutôt d’accord", "Plutôt d'accord")
  add_estimate("Acte citoyen", "Tout à fait d’accord", "Tout à fait d'accord")
  add_ref("Niveau d'imposition", "Juste")
  add_estimate("Niveau d'imposition", "Trop élevés", "Trop élevés")
  add_estimate("Niveau d'imposition", "Pas assez", "Pas assez")
  add_ref("Impôt sur le revenu", "Non")
  add_estimate("Impôt sur le revenu", "Oui", "Oui")
  add_ref("Risque de conflit", "Très faible")
  add_estimate("Risque de conflit", "Plutôt faible", "Plutôt faible")
  add_estimate("Risque de conflit", "Assez élevé", "Assez élevé")
  add_estimate("Risque de conflit", "Très élevé", "Très élevé")
  add_estimate("Risque de conflit", "NSP", "NSP")
  add_ref("Satisfaction Utilisation Argent Public", "Pas du tout satisfait(e)")
  add_estimate("Satisfaction Utilisation Argent Public", "Plutôt pas satisfait(e)", "Plutôt pas satisfait(e)")
  add_estimate("Satisfaction Utilisation Argent Public", "Plutôt satisfait(e)", "Plutôt satisfait(e)")
  add_estimate("Satisfaction Utilisation Argent Public", "Très satisfait(e)", "Très satisfait(e)")
  add_ref("Genre", "Femme")
  add_estimate("Genre", "Homme", "Homme")
  add_estimate("Age", "", "", bold = TRUE)
  add_estimate("Age\\textsuperscript{2}", "", "", bold = TRUE)
  add_ref("Revenus", "< 12k")
  add_estimate("Revenus", "12k--18k", "12k--18k")
  add_estimate("Revenus", "18k--24k", "18k--24k")
  add_estimate("Revenus", "24k--36k", "24k--36k")
  add_estimate("Revenus", "36k--48k", "36k--48k")
  add_estimate("Revenus", "48k--60k", "48k--60k")
  add_estimate("Revenus", "60k--72k", "60k--72k")
  add_estimate("Revenus", "72k--144k", "72k--144k")
  add_estimate("Revenus", "144k+", "144k+")
  add_estimate("Revenus", "NSP", "NSP")
  add_row("Observations", "", setNames(as.character(nobs_by_outcome), names(ames)), section_start = TRUE)
  add_row("Contrôle", "", setNames(rep("Oui", length(ames)), names(ames)))

  render_term <- function(term) {
    term <- html_escape(term)
    term <- gsub("Age\\\\textsuperscript\\{2\\}", "Age<sup>2</sup>", term)
    if (nzchar(term)) {
      paste0("<strong>", term, "</strong>")
    } else {
      ""
    }
  }

  render_row <- function(i) {
    row <- rows[i]
    class_attr <- if (isTRUE(row$section_start)) " class=\"section-start\"" else ""
    cells <- c(
      paste0("<td>", render_term(row$term), "</td>"),
      paste0("<td>", html_escape(row$contrast), "</td>"),
      vapply(
        names(ames),
        function(outcome_label) paste0("<td>", html_escape(row[[outcome_label]]), "</td>"),
        character(1)
      )
    )
    paste0("<tr", class_attr, ">", paste(cells, collapse = ""), "</tr>")
  }

  note_text <- paste(
    "Note: Les quatre modèles sont estimés sur un échantillon commun.",
    "Ils reprennent la spécification du modèle 3 de la table détaillée de défense : risque de conflit détaillé, niveau d'imposition,",
    "paiement de l'impôt sur le revenu, Satisfaction Utilisation Argent Public,",
    "la catégorie socio-professionnelle, la situation matrimoniale, le foyer, l'âge, l'âge au carré,",
    "l'acte citoyen, la taille d'agglomération, les revenus, le diplôme, la proximité partisane,",
    "le genre et la satisfaction.",
    "Les coefficients présentés correspondent aux effets marginaux moyens estimés.",
    "Les écarts-types robustes à l'hétéroscédasticité figurent entre parenthèses à côté des estimations.",
    "Seuil de confiance : + p < 0,10 ; * p < 0,05 ; ** p < 0,01 ; *** p < 0,001."
  )

  lines <- c(
    "<!doctype html>",
    "<html lang=\"fr\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<title>Les déterminants du refus de baisser les dépenses publiques par poste</title>",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;margin:24px;color:#111827;}table{border-collapse:collapse;font-size:13px;}caption{caption-side:top;text-align:left;font-weight:700;margin-bottom:8px;}th,td{border-bottom:1px solid #e5e7eb;padding:5px 7px;text-align:left;vertical-align:top;}th:nth-child(n+3),td:nth-child(n+3){text-align:center;}tr.section-start td{border-top:2px solid #111827;}tfoot td{border-top:2px solid #111827;font-size:12px;color:#374151;text-align:left;}</style>",
    "</head>",
    "<body>",
    "<table>",
    "<caption>Les déterminants du refus de baisser les dépenses publiques par poste</caption>",
    "<thead>",
    "<tr><th></th><th></th><th colspan=\"4\">Refus de baisser la dépense considérée</th></tr>",
    paste0("<tr><th></th><th></th>", paste0("<th>", html_escape(names(ames)), "</th>", collapse = ""), "</tr>"),
    "</thead>",
    "<tbody>",
    vapply(seq_len(nrow(rows)), render_row, character(1)),
    "</tbody>",
    "<tfoot>",
    paste0("<tr><td colspan=\"", length(ames) + 2L, "\">", html_escape(note_text), "</td></tr>"),
    "</tfoot>",
    "</table>",
    "</body>",
    "</html>"
  )

  writeLines(lines, path, useBytes = TRUE)
}

outcome_labels <- c(
  militaryexp_binary = "Défense",
  healthexp_binary = "Santé",
  pensionexp_binary = "Retraites",
  povertyexp_binary = "Pauvreté"
)

comparison_covariates <- c(
  "q35", "q19", "q5", "pcs", "matri", "foyer",
  "continuous_age", "q23", "tailleagglo5", "tranche_revenus",
  "diplome", "partisane", "q28", "gender", "sat"
)
comparison_char_vars <- c(
  "q35", "q19", "q5", "q23", "pcs", "matri", "foyer",
  "tailleagglo5", "tranche_revenus", "diplome", "partisane", "q28", "gender", "sat"
)
comparison_sample_vars <- c(names(outcome_labels), comparison_covariates, "weights")
required_columns <- c(comparison_sample_vars, "treatment")

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
  (comparison_char_vars) := lapply(.SD, empty_to_na),
  .SDcols = comparison_char_vars
]
consent_other <- consent_other[q5 %chin% c("Oui", "Non")]
consent_other <- consent_other[complete.cases(consent_other[, ..comparison_sample_vars])]

if (nrow(consent_other) == 0L) {
  stop("No complete-case observations for the common Table 2 sample.", call. = FALSE)
}

invalid_outcome_values <- names(outcome_labels)[
  vapply(
    names(outcome_labels),
    function(outcome) {
      any(!consent_other[[outcome]] %in% c(0L, 1L))
    },
    logical(1)
  )
]
if (length(invalid_outcome_values) > 0L) {
  stop(
    "Outcome columns must be coded 0/1: ",
    paste(invalid_outcome_values, collapse = ", "),
    call. = FALSE
  )
}

age_mean <- mean(consent_other$continuous_age)
age_sd <- sd(consent_other$continuous_age)
if (!is.finite(age_sd) || age_sd <= 0) {
  stop("Cannot normalize continuous_age: standard deviation is zero or missing.", call. = FALSE)
}
consent_other[
  ,
  `:=`(
    continuous_age_norm = (continuous_age - age_mean) / age_sd,
    continuous_age_norm_sq = ((continuous_age - age_mean) / age_sd)^2,
    acte_citoyen = q23
  )
]
consent_other[, q35 := relevel_q35_tres_faible_factor(q35)]
consent_other[, q19 := relevel_factor(q19, ref = "Ni trop, ni pas assez élevés", var_name = "q19")]
consent_other[, q5 := relevel_q5_factor(q5)]
consent_other[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
consent_other[, acte_citoyen := relevel_factor(acte_citoyen, ref = "Pas du tout d’accord", var_name = "acte_citoyen")]
consent_other[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
consent_other[, q28 := relevel_q28_factor(q28)]
consent_other[, sat := relevel_sat_factor(sat)]
consent_other[
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

comparison_model_outputs <- setNames(
  lapply(
    names(outcome_labels),
    function(outcome) fit_comparison_model(consent_other, outcome)
  ),
  unname(outcome_labels)
)
comparison_ames <- lapply(
  comparison_model_outputs,
  function(model_output) order_otherexp_table_rows(clean_ame_labels(model_output$ame))
)
comparison_nobs <- vapply(comparison_model_outputs, `[[`, integer(1), "nobs")

tex_output_file <- file.path(table_latex_dir, "otherexp_ame_baseline.tex")
html_output_file <- file.path(table_html_dir, "otherexp_ame_baseline.html")
write_otherexp_table(
  path = tex_output_file,
  ames = comparison_ames,
  nobs_by_outcome = comparison_nobs
)
write_otherexp_table_html(
  path = html_output_file,
  ames = comparison_ames,
  nobs_by_outcome = comparison_nobs
)

message("Table saved to: ", tex_output_file)
message("Table saved to: ", html_output_file)
