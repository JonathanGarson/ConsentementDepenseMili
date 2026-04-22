#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(marginaleffects)
  library(ragg)
  library(scales)
})


# Labels and constants ----

comparison_vars <- c(
  "militaryexp_binary", "q30_q2", "q35", "q19", "pcs", "matri", "foyer",
  "continuous_age", "tailleagglo5", "tranche_revenus", "diplome", "partisane", "gender"
)

comparison_char_vars <- c(
  "q30_q2", "q35", "q19", "pcs", "matri", "foyer",
  "tailleagglo5", "tranche_revenus", "diplome", "partisane", "gender"
)

party_label_map <- c(
  "Le NPA (Nouveau Parti Anticapitaliste)" = "Le NPA",
  "Les Ecologistes (ex-Europe Ecologie Les Verts)" = "Les Ecologistes",
  "Renaissance (ex-La République En Marche)" = "Renaissance",
  "Horizons (d'Édouard Philippe)" = "Horizons",
  "L’UDI (Union des Démocrates et Indépendants)" = "UDI",
  "Debout la France (de Nicolas Dupont-Aignan)" = "Debout la France",
  "Reconquête ! (d'Éric Zemmour)" = "Reconquête"
)

ideological_party_order <- c(
  "Le NPA",
  "Lutte Ouvrière",
  "Le Parti Communiste",
  "La France Insoumise",
  "Les Ecologistes",
  "Le Parti socialiste",
  "Renaissance",
  "Le MoDem",
  "Horizons",
  "UDI",
  "Les Républicains",
  "Debout la France",
  "Le Rassemblement National",
  "Reconquête"
)

non_party_order <- c("Ne sait pas")

pcs_label_map <- c(
  "Agriculteur exploitant" = "Agriculteur",
  "Artisan, commerçant et assimilé, chef d’entreprise" = "Artisan / commerçant / chef d'entreprise",
  "Autre, sans activité professionnelle" = "Autre sans activité",
  "Cadre d'entreprise, cadre de la fonction publique, profession intellectuelle et artistique supérieure" = "Cadre / profession intellectuelle sup.",
  "Elève, étudiant" = "Élève / étudiant",
  "En recherche d'un premier emploi" = "Premier emploi",
  "Femme, homme au foyer" = "Au foyer",
  "Employé" = "Employé",
  "Ouvrier" = "Ouvrier",
  "Profession intermédiaire (technicien, contremaître, agent de maîtrise, professeur des écoles, instituteur, infirmier, éducateur ...)" = "Profession intermédiaire",
  "Profession libérale et assimilée" = "Profession libérale",
  "Retraité" = "Retraité"
)


# Helper functions ----

pick_font_family <- function() {
  candidates <- c("Avenir Next", "Avenir", "Helvetica Neue", "Gill Sans", "Arial")

  for (family in candidates) {
    matched <- tryCatch(systemfonts::match_fonts(family), error = function(e) NULL)

    if (!is.null(matched) && nrow(matched) > 0 && nzchar(matched$path[1])) {
      return(family)
    }
  }

  "sans"
}

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

clean_party_labels <- function(x) {
  mapped <- unname(party_label_map[x])
  x[!is.na(mapped)] <- mapped[!is.na(mapped)]
  x
}

extract_contrast_label <- function(x) {
  sub(" - .*$", "", x)
}

build_party_order <- function(labels) {
  observed <- unique(labels)
  unexpected <- sort(setdiff(observed, c(ideological_party_order, non_party_order)))

  c(
    ideological_party_order[ideological_party_order %chin% observed],
    unexpected,
    non_party_order[non_party_order %chin% observed]
  )
}

prepare_model3_sample <- function() {
  survey2025 <- fread(path_data_final("final_survey2025.csv"))[treatment == 0]
  survey2025 <- filter_valid_weights(survey2025)

  consent_comparison <- copy(survey2025)
  consent_comparison[
    ,
    (comparison_char_vars) := lapply(.SD, empty_to_na),
    .SDcols = comparison_char_vars
  ]

  sample_vars <- c(comparison_vars, "weights")
  consent_comparison <- consent_comparison[
    complete.cases(consent_comparison[, ..sample_vars])
  ]

  consent_comparison[, q30_q2 := relevel_factor(q30_q2, ref = "Pas du tout confiance", var_name = "q30_q2")]
  consent_comparison[, q35 := relevel_factor(q35, ref = "Très élevé", var_name = "q35")]
  consent_comparison[, q19 := relevel_factor(q19, ref = "Trop élevés", var_name = "q19")]
  consent_comparison[, gender := relevel_factor(gender, ref = "Femme", var_name = "gender")]
  consent_comparison[, partisane := relevel_factor(partisane, ref = "Aucun", var_name = "partisane")]
  consent_comparison[
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

  consent_comparison
}

fit_model3 <- function(data) {
  feglm(
    militaryexp_binary ~ q30_q2 + q35 + q19 + pcs + matri + foyer +
      continuous_age + tailleagglo5 + tranche_revenus + diplome + partisane + gender,
    family = binomial(link = "probit"),
    vcov = "hetero",
    weights = ~ weights,
    data = data
  )
}

prepare_ame_plot_data <- function(ame, term_name, label_map = NULL, order_levels = NULL, sort_by_estimate = FALSE) {
  plot_data <- copy(as.data.table(ame))[term == term_name]

  if (nrow(plot_data) == 0) {
    stop("No AME rows found for term '", term_name, "'.", call. = FALSE)
  }

  plot_data[
    ,
    category := extract_contrast_label(contrast)
  ]

  if (!is.null(label_map)) {
    mapped <- unname(label_map[plot_data$category])
    plot_data[, category_label := fifelse(is.na(mapped), category, mapped)]
  } else {
    plot_data[, category_label := category]
  }

  plot_data[
    ,
    `:=`(
      estimate_pp = 100 * estimate,
      conf.low_pp = 100 * conf.low,
      conf.high_pp = 100 * conf.high,
      effect_direction = fifelse(estimate >= 0, "Positive", "Negative")
    )
  ]

  if (!is.null(order_levels)) {
    plot_levels <- rev(order_levels[order_levels %chin% plot_data$category_label])
  } else if (sort_by_estimate) {
    setorder(plot_data, estimate_pp)
    plot_levels <- plot_data$category_label
  } else {
    plot_levels <- rev(unique(plot_data$category_label))
  }

  plot_data[, category_label := factor(category_label, levels = plot_levels)]
  plot_data
}

plot_ame_estimates <- function(plot_data,
                               output_file,
                               font_family,
                               height,
                               reference_group_label) {
  x_range <- range(c(plot_data$conf.low_pp, plot_data$conf.high_pp), na.rm = TRUE)
  x_padding <- max(1.2, 0.08 * diff(x_range))

  p <- ggplot(plot_data, aes(x = estimate_pp, y = category_label)) +
    geom_vline(
      xintercept = 0,
      color = "#9CA3AF",
      linewidth = 0.6,
      linetype = "dashed"
    ) +
    geom_segment(
      aes(x = conf.low_pp, xend = conf.high_pp, yend = category_label),
      color = "#94A3B8",
      linewidth = 1.2,
      lineend = "round"
    ) +
    geom_point(
      aes(fill = effect_direction),
      shape = 21,
      size = 3.3,
      stroke = 0.4,
      color = "#111827"
    ) +
    scale_fill_manual(
      values = c(
        "Positive" = "#14532D",
        "Negative" = "#B45309"
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      labels = label_number(
        accuracy = 0.1,
        decimal.mark = ",",
        suffix = " pp"
      ),
      limits = c(x_range[1] - x_padding, x_range[2] + x_padding),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_discrete(labels = label_wrap(34)) +
    labs(
      x = "Effet marginal moyen",
      y = NULL,
      caption = paste(
        "Source : Baromètre opinion 2025.",
        "Modèle 3 du tableau central.",
        "Probit pondéré, IC à 95 %.",
        paste0("Groupe de référence : ", reference_group_label, ".")
      )
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.text.x = element_text(color = "#374151", size = 11),
      axis.text.y = element_text(color = "#374151", size = 11, lineheight = 0.95),
      axis.title.x = element_text(color = "#111827", size = 12, margin = margin(t = 12)),
      plot.caption = element_text(color = "#6B7280", size = 10, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.margin = margin(t = 18, r = 18, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 11.8,
    height = height,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}


# Build figures ----

font_family <- pick_font_family()
figure_dir <- output_path("figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

consent_model3_sample <- prepare_model3_sample()
consent_model3 <- fit_model3(consent_model3_sample)

ame_model3 <- avg_slopes(
  consent_model3,
  variables = c("pcs", "partisane"),
  newdata = consent_model3_sample,
  wts = "weights"
)

pcs_plot_data <- prepare_ame_plot_data(
  ame = ame_model3,
  term_name = "pcs",
  label_map = pcs_label_map,
  sort_by_estimate = TRUE
)

party_plot_data <- prepare_ame_plot_data(
  ame = ame_model3,
  term_name = "partisane",
  label_map = party_label_map,
  order_levels = build_party_order(clean_party_labels(unique(as.character(consent_model3_sample$partisane))))
)

plot_ame_estimates(
  plot_data = pcs_plot_data,
  output_file = output_path("figures", "defense_model3_ame_pcs.png"),
  font_family = font_family,
  height = 8.1,
  reference_group_label = "Retraité"
)

plot_ame_estimates(
  plot_data = party_plot_data,
  output_file = output_path("figures", "defense_model3_ame_partisane.png"),
  font_family = font_family,
  height = 8.7,
  reference_group_label = "Aucun"
)

message("Figures saved to: ", output_path("figures", "defense_model3_ame_pcs.png"))
message("Figures saved to: ", output_path("figures", "defense_model3_ame_partisane.png"))
