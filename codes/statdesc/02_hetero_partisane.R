#!/usr/bin/env Rscript

# This script produces descriptive figures on partisan heterogeneity and on
# preferred ways to finance higher military spending.

# Setup ----
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ragg)
  library(scales)
})


# Labels and style constants ----

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

non_party_order <- c("Aucun", "Ne sait pas")

q36_option_labels <- c(
  q36_q1 = "Baisse des autres dépenses publiques",
  q36_q2 = "Hausse des impôts ou cotisations sociales",
  q36_q3 = "Augmentation du déficit et de la dette",
  q36_q4 = "Augmentation du nombre d'heures ou de jours travaillés"
)

# Proxy labels for support to military spending. In the cleaned survey,
# `militaryexp_binary = 1` means refusing a cut in military spending.
military_support_labels <- c(
  `1` = "Refuse une baisse",
  `0` = "Accepte une baisse"
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

# Convert blank strings to missing values before computing weighted summaries.
empty_to_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

# Harmonize long party labels so plots remain readable.
clean_party_labels <- function(x) {
  x <- empty_to_na(x)
  mapped <- unname(party_label_map[x])
  x[!is.na(mapped)] <- mapped[!is.na(mapped)]
  x
}

# Read the final survey file and coerce the columns used by the figures.
read_survey <- function(path) {
  survey <- fread(path)
  required_columns <- c(
    "partisane", "q35", "q30_q2", "militaryexp_binary",
    names(q36_option_labels), "weights"
  )
  missing_columns <- setdiff(required_columns, names(survey))

  if (length(missing_columns) > 0) {
    stop(
      "Required columns missing from final_survey2025.csv: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  survey[
    ,
    `:=`(
      partisane = clean_party_labels(partisane),
      q35 = empty_to_na(q35),
      q30_q2 = empty_to_na(q30_q2),
      militaryexp_binary = as.integer(militaryexp_binary),
      weights = as.numeric(weights)
    )
  ]

  survey[
    ,
    (names(q36_option_labels)) := lapply(.SD, as.integer),
    .SDcols = names(q36_option_labels)
  ]

  if (all(is.na(survey$weights))) {
    stop("Column 'weights' is empty or non-numeric.", call. = FALSE)
  }

  survey
}

# Order parties on an ideological axis and keep non-party categories at the end.
build_party_order <- function(survey) {
  observed_parties <- unique(survey[!is.na(partisane) & !is.na(weights), partisane])
  unexpected_parties <- sort(setdiff(observed_parties, c(ideological_party_order, non_party_order)))
  party_order <- c(
    ideological_party_order[ideological_party_order %chin% observed_parties],
    unexpected_parties,
    non_party_order[non_party_order %chin% observed_parties]
  )

  if (!length(party_order)) {
    stop("No valid party labels found in 'partisane'.", call. = FALSE)
  }

  party_order
}

# Compute weighted response shares by party for single-choice questions.
compute_weighted_shares <- function(survey, question, categories, party_order) {
  distribution <- survey[
    !is.na(partisane) & !is.na(weights),
    .(partisane, response = empty_to_na(get(question)), weights)
  ][
    response %chin% categories
  ][
    ,
    .(weighted_n = sum(weights)),
    by = .(partisane, response)
  ]

  if (nrow(distribution) == 0) {
    stop("No valid observations found for question '", question, "'.", call. = FALSE)
  }

  distribution <- distribution[
    CJ(partisane = party_order, response = categories, unique = TRUE),
    on = .(partisane, response)
  ]

  distribution[is.na(weighted_n), weighted_n := 0]
  distribution[, party_total := sum(weighted_n), by = partisane]
  distribution <- distribution[party_total > 0]
  distribution[, share := weighted_n / party_total]
  distribution[, partisane := factor(partisane, levels = party_order)]
  distribution[, response := factor(response, levels = categories)]

  distribution
}

# Compute overall weighted shares for each q36 multiple-response option.
compute_q36_shares <- function(survey, option_labels) {
  shares <- rbindlist(
    lapply(
      names(option_labels),
      function(option_col) {
        option_data <- survey[!is.na(weights) & !is.na(get(option_col))]

        if (nrow(option_data) == 0) {
          return(data.table(option = option_labels[[option_col]], share = NA_real_))
        }

        data.table(
          option = option_labels[[option_col]],
          share = option_data[get(option_col) == 1L, sum(weights)] / option_data[, sum(weights)]
        )
      }
    )
  )

  shares[, option := factor(option, levels = unname(q36_option_labels))]
  shares
}

# Compute weighted mean selection rates for q36 options by defense-support proxy.
compute_q36_shares_by_support <- function(survey, option_labels, support_labels) {
  support_order <- unname(support_labels)

  shares <- rbindlist(
    lapply(
      names(option_labels),
      function(option_col) {
        option_data <- survey[
          !is.na(weights) & !is.na(militaryexp_binary) & !is.na(get(option_col))
        ][
          ,
          support_group := fcase(
            militaryexp_binary == 1L, support_labels[["1"]],
            militaryexp_binary == 0L, support_labels[["0"]],
            default = NA_character_
          )
        ][
          !is.na(support_group)
        ][
          ,
          .(share = weighted.mean(get(option_col), w = weights)),
          by = support_group
        ][
          ,
          option := option_labels[[option_col]]
        ]

        option_data
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(shares) == 0) {
    stop("No valid observations found for q36 by military-support groups.", call. = FALSE)
  }

  shares <- shares[
    CJ(option = unname(option_labels), support_group = support_order, unique = TRUE),
    on = .(option, support_group)
  ]

  shares[is.na(share), share := 0]
  shares[, option := factor(option, levels = rev(unname(option_labels)))]
  shares[, support_group := factor(support_group, levels = support_order)]

  shares
}

# Plot weighted shares by party using the visual language shared across statdesc figures.
plot_weighted_distribution <- function(distribution,
                                       y_label,
                                       fill_values,
                                       output_file,
                                       font_family) {
  p <- ggplot(distribution, aes(x = partisane, y = share, fill = response)) +
    geom_col(
      position = position_dodge2(width = 0.7, padding = 0.18, preserve = "single"),
      width = 0.54,
      color = "#FCFCF8",
      linewidth = 0.25
    ) +
    scale_fill_manual(values = fill_values) +
    scale_x_discrete(
      labels = label_wrap(12),
      guide = guide_axis(n.dodge = 2)
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = NULL,
      y = y_label,
      fill = NULL,
      caption = "Source : Baromètre opinion 2025. Pondération : poids de propension."
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.text.x = element_text(color = "#374151", size = 10, lineheight = 0.95, margin = margin(t = 6)),
      axis.text.y = element_text(color = "#374151", size = 11),
      axis.title.y = element_text(color = "#111827", size = 12, margin = margin(r = 12)),
      plot.caption = element_text(color = "#6B7280", size = 10, margin = margin(t = 14)),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(color = "#111827", size = 10.5),
      legend.key.width = grid::unit(1.4, "lines"),
      plot.caption.position = "plot",
      plot.margin = margin(t = 18, r = 18, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 16.5,
    height = 8.2,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}

# Plot overall q36 shares across the full sample.
plot_q36_distribution <- function(distribution,
                                  output_file,
                                  font_family) {
  label_fun <- label_percent(accuracy = 0.1)

  p <- ggplot(distribution, aes(x = share, y = option)) +
    geom_col(
      width = 0.62,
      fill = "#2F855A",
      color = "#FCFCF8",
      linewidth = 0.3
    ) +
    geom_text(
      aes(label = label_fun(share)),
      hjust = -0.08,
      family = font_family,
      size = 4,
      color = "#111827"
    ) +
    scale_x_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    scale_y_discrete(labels = label_wrap(34)) +
    labs(
      x = "Part des répondants",
      y = NULL,
      caption = "Source : Baromètre opinion 2025. Pondération : poids de propension. Plusieurs réponses possibles."
    ) +
    coord_cartesian(clip = "off") +
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
      plot.margin = margin(t = 18, r = 42, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 11.5,
    height = 6.8,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}

# Plot q36 shares by defense-support proxy using grouped horizontal bars.
plot_q36_by_support <- function(distribution,
                                output_file,
                                font_family) {
  label_fun <- label_percent(accuracy = 0.1)
  bar_position <- position_dodge2(width = 0.72, padding = 0.18, preserve = "single")

  p <- ggplot(distribution, aes(x = share, y = option, fill = support_group)) +
    geom_col(
      position = bar_position,
      width = 0.62,
      color = "#FCFCF8",
      linewidth = 0.3
    ) +
    geom_text(
      aes(label = label_fun(share)),
      position = bar_position,
      hjust = -0.08,
      family = font_family,
      size = 3.9,
      color = "#111827"
    ) +
    scale_fill_manual(
      values = c(
        "Refuse une baisse" = "#14532D",
        "Accepte une baisse" = "#86D39D"
      )
    ) +
    scale_x_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    scale_y_discrete(labels = label_wrap(34)) +
    labs(
      x = "Part des répondants",
      y = NULL,
      fill = NULL,
      caption = paste(
        "Source : Baromètre opinion 2025. Pondération : poids de propension.",
        "Proxy de soutien : refus d'une baisse des dépenses militaires.",
        "Plusieurs réponses possibles."
      )
    ) +
    coord_cartesian(clip = "off") +
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
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(color = "#111827", size = 10.5),
      legend.key.width = grid::unit(1.4, "lines"),
      plot.caption.position = "plot",
      plot.margin = margin(t = 18, r = 48, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 12.2,
    height = 7.2,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}


# Data preparation ----

font_family <- pick_font_family()
figure_dir <- output_path("figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

survey <- read_survey(path_data_final("final_survey2025.csv"))
party_order <- build_party_order(survey)


# Party heterogeneity figures ----

question_specs <- list(
  q35 = list(
    categories = c("Très élevé", "Assez élevé", "Plutôt faible", "Très faible"),
    fill_values = c(
      "Très élevé" = "#14532D",
      "Assez élevé" = "#2F855A",
      "Plutôt faible" = "#86D39D",
      "Très faible" = "#CFEFC8"
    ),
    y_label = "Part des répondants",
    output_file = output_path("figures", "hetero_partisane_q35.png")
  ),
  q30_q2 = list(
    categories = c(
      "Pas du tout confiance",
      "Plutôt pas confiance",
      "Plutôt confiance",
      "Tout à fait confiance"
    ),
    fill_values = c(
      "Pas du tout confiance" = "#14532D",
      "Plutôt pas confiance" = "#2F855A",
      "Plutôt confiance" = "#86D39D",
      "Tout à fait confiance" = "#CFEFC8"
    ),
    y_label = "Part des répondants",
    output_file = output_path("figures", "hetero_partisane_q30_q2.png")
  )
)

for (question in names(question_specs)) {
  spec <- question_specs[[question]]
  distribution <- compute_weighted_shares(
    survey = survey,
    question = question,
    categories = spec$categories,
    party_order = party_order
  )

  plot_weighted_distribution(
    distribution = distribution,
    y_label = spec$y_label,
    fill_values = spec$fill_values,
    output_file = spec$output_file,
    font_family = font_family
  )

  message("Figure saved to: ", spec$output_file)
}


# Overall q36 figure ----

q36_distribution <- compute_q36_shares(
  survey = survey,
  option_labels = q36_option_labels
)

q36_output_file <- output_path("figures", "q36_effort_militaire.png")

plot_q36_distribution(
  distribution = q36_distribution,
  output_file = q36_output_file,
  font_family = font_family
)

message("Figure saved to: ", q36_output_file)


# Q36 figure by defense-support proxy ----

q36_support_distribution <- compute_q36_shares_by_support(
  survey = survey,
  option_labels = q36_option_labels,
  support_labels = military_support_labels
)

q36_support_output_file <- output_path("figures", "q36_effort_militaire_by_support.png")

plot_q36_by_support(
  distribution = q36_support_distribution,
  output_file = q36_support_output_file,
  font_family = font_family
)

message("Figure saved to: ", q36_support_output_file)
