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

valid_q36_sample <- function(survey, option_labels) {
  option_cols <- names(option_labels)
  invalid_cols <- option_cols[
    !vapply(
      option_cols,
      function(col) all(is.na(survey[[col]]) | survey[[col]] %in% c(0L, 1L)),
      logical(1)
    )
  ]

  if (length(invalid_cols) > 0L) {
    stop("q36 columns must be coded 0/1: ", paste(invalid_cols, collapse = ", "), call. = FALSE)
  }

  q36_sample <- survey[
    !is.na(weights) & weights > 0 & complete.cases(survey[, ..option_cols])
  ]

  if (nrow(q36_sample) == 0L) {
    stop("No valid complete-case observations found for q36.", call. = FALSE)
  }

  q36_sample
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

compute_q36_selection_counts <- function(survey, option_labels) {
  option_cols <- names(option_labels)
  q36_sample <- valid_q36_sample(survey, option_labels)
  q36_sample[, selection_count := rowSums(.SD == 1L), .SDcols = option_cols]

  distribution <- q36_sample[
    ,
    .(
      weighted_n = sum(weights),
      unweighted_n = .N
    ),
    by = selection_count
  ]

  distribution <- distribution[
    data.table(selection_count = seq.int(0L, length(option_cols))),
    on = "selection_count"
  ]

  distribution[is.na(weighted_n), weighted_n := 0]
  distribution[is.na(unweighted_n), unweighted_n := 0L]
  distribution[, share := weighted_n / sum(weighted_n)]
  distribution[
    ,
    count_label := fifelse(
      selection_count == 0L,
      "0 réponse",
      fifelse(selection_count == 1L, "1 réponse", paste(selection_count, "réponses"))
    )
  ]
  distribution[, count_label := factor(count_label, levels = count_label)]
  distribution[]
}

compute_q36_single_answer_distribution <- function(survey, option_labels) {
  option_cols <- names(option_labels)
  q36_sample <- valid_q36_sample(survey, option_labels)
  q36_sample[, selection_count := rowSums(.SD == 1L), .SDcols = option_cols]
  single_answer_sample <- q36_sample[selection_count == 1L]

  if (nrow(single_answer_sample) == 0L) {
    stop("No respondents selected exactly one q36 option.", call. = FALSE)
  }

  distribution <- rbindlist(
    lapply(
      option_cols,
      function(option_col) {
        data.table(
          option = option_labels[[option_col]],
          weighted_n = single_answer_sample[get(option_col) == 1L, sum(weights)],
          unweighted_n = single_answer_sample[get(option_col) == 1L, .N]
        )
      }
    )
  )

  distribution[is.na(weighted_n), weighted_n := 0]
  distribution[is.na(unweighted_n), unweighted_n := 0L]
  distribution[, share := weighted_n / sum(weighted_n)]
  distribution[, option := factor(option, levels = rev(unname(option_labels)))]
  distribution[]
}

compute_q36_pairwise_lift <- function(survey, option_labels) {
  option_cols <- names(option_labels)
  q36_sample <- valid_q36_sample(survey, option_labels)
  total_weight <- q36_sample[, sum(weights)]

  marginal_shares <- data.table(
    option_col = option_cols,
    option = unname(option_labels),
    marginal_share = vapply(
      option_cols,
      function(option_col) q36_sample[get(option_col) == 1L, sum(weights)] / total_weight,
      numeric(1)
    )
  )

  zero_marginals <- marginal_shares[marginal_share <= 0 | is.na(marginal_share), option]
  if (length(zero_marginals) > 0L) {
    stop(
      "Cannot compute q36 lift because at least one option has zero weighted selections: ",
      paste(zero_marginals, collapse = ", "),
      call. = FALSE
    )
  }

  pairwise <- rbindlist(
    combn(
      option_cols,
      2L,
      simplify = FALSE,
      FUN = function(cols) {
        option_a_col <- cols[[1L]]
        option_b_col <- cols[[2L]]
        option_a_share <- marginal_shares[option_col == option_a_col, marginal_share]
        option_b_share <- marginal_shares[option_col == option_b_col, marginal_share]
        observed_share <- q36_sample[
          get(option_a_col) == 1L & get(option_b_col) == 1L,
          sum(weights)
        ] / total_weight
        expected_share <- option_a_share * option_b_share

        data.table(
          option_a = option_labels[[option_a_col]],
          option_b = option_labels[[option_b_col]],
          observed_share = observed_share,
          expected_share = expected_share,
          lift = observed_share / expected_share
        )
      }
    )
  )

  pairwise[, lift_label := sprintf("%.2f", lift)]
  pairwise[, option_a := factor(option_a, levels = rev(unname(option_labels)))]
  pairwise[, option_b := factor(option_b, levels = unname(option_labels))]
  pairwise[]
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
      caption = "Pondération : poids de propension."
    ) +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.text.x = element_text(color = "#374151", size = 12.5, lineheight = 0.95, margin = margin(t = 6)),
      axis.text.y = element_text(color = "#374151", size = 14),
      axis.title.y = element_text(color = "#111827", size = 15, margin = margin(r = 12)),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(color = "#111827", size = 13),
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
      size = 5,
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
      caption = "Pondération : poids de propension. Plusieurs réponses possibles."
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.text.x = element_text(color = "#374151", size = 14),
      axis.text.y = element_text(color = "#374151", size = 14, lineheight = 0.95),
      axis.title.x = element_text(color = "#111827", size = 15, margin = margin(t = 12)),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
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

plot_q36_selection_counts <- function(distribution,
                                      output_file,
                                      font_family) {
  label_fun <- label_percent(accuracy = 0.1)

  p <- ggplot(distribution, aes(x = count_label, y = share)) +
    geom_col(
      width = 0.62,
      fill = "#2F855A",
      color = "#FCFCF8",
      linewidth = 0.3
    ) +
    geom_text(
      aes(label = label_fun(share)),
      vjust = -0.35,
      family = font_family,
      size = 5,
      color = "#111827"
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      x = "Nombre de réponses sélectionnées en q36",
      y = "Part des répondants",
      caption = paste(
        "Pondération : poids de propension.",
        "Champ : répondants avec indicateurs q36 complets."
      )
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E7EB", linewidth = 0.45),
      axis.text.x = element_text(color = "#374151", size = 14),
      axis.text.y = element_text(color = "#374151", size = 14),
      axis.title.x = element_text(color = "#111827", size = 15, margin = margin(t = 12)),
      axis.title.y = element_text(color = "#111827", size = 15, margin = margin(r = 12)),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.margin = margin(t = 18, r = 30, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 9.4,
    height = 6.4,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}

plot_q36_single_answer_distribution <- function(distribution,
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
      size = 5,
      color = "#111827"
    ) +
    scale_x_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    scale_y_discrete(labels = label_wrap(34)) +
    labs(
      x = "Part des répondants avec une seule réponse",
      y = NULL,
      caption = paste(
        "Pondération : poids de propension.",
        "Champ : répondants ayant sélectionné exactement une option en q36."
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
      axis.text.x = element_text(color = "#374151", size = 14),
      axis.text.y = element_text(color = "#374151", size = 14, lineheight = 0.95),
      axis.title.x = element_text(color = "#111827", size = 15, margin = margin(t = 12)),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
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

plot_q36_pairwise_lift <- function(association,
                                   output_file,
                                   font_family) {
  p <- ggplot(association, aes(x = option_b, y = option_a, fill = lift)) +
    geom_tile(
      color = "#FCFCF8",
      linewidth = 0.9
    ) +
    geom_text(
      aes(label = lift_label),
      family = font_family,
      size = 5.2,
      color = "#111827"
    ) +
    scale_fill_gradient2(
      low = "#9CC9E2",
      mid = "#F8FAFC",
      high = "#14532D",
      midpoint = 1,
      labels = label_number(accuracy = 0.1),
      name = "Lift"
    ) +
    scale_x_discrete(labels = label_wrap(24), drop = FALSE) +
    scale_y_discrete(labels = label_wrap(24), drop = FALSE) +
    labs(
      x = NULL,
      y = NULL,
      caption = paste(
        "Pondération : poids de propension.",
        "Lift = co-sélection observée / co-sélection attendue sous indépendance ; 1 = indépendance."
      )
    ) +
    coord_fixed(clip = "off") +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid = element_blank(),
      axis.text.x = element_text(
        color = "#374151",
        size = 9.2,
        lineheight = 0.9,
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      axis.text.y = element_text(color = "#374151", size = 12.5, lineheight = 0.95),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(color = "#111827", size = 13),
      legend.text = element_text(color = "#374151", size = 12),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
      plot.caption.position = "plot",
      plot.margin = margin(t = 18, r = 24, b = 90, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 10.8,
    height = 8.4,
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
      size = 4.9,
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
        "Pondération : poids de propension.",
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
      axis.text.x = element_text(color = "#374151", size = 14),
      axis.text.y = element_text(color = "#374151", size = 14, lineheight = 0.95),
      axis.title.x = element_text(color = "#111827", size = 15, margin = margin(t = 12)),
      plot.caption = element_text(color = "#6B7280", size = 12.5, margin = margin(t = 14)),
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(color = "#111827", size = 13),
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


# Q36 multiple-response diagnostics ----

q36_selection_count <- compute_q36_selection_counts(
  survey = survey,
  option_labels = q36_option_labels
)

q36_selection_count_output_file <- output_path("figures", "q36_selection_count.png")

plot_q36_selection_counts(
  distribution = q36_selection_count,
  output_file = q36_selection_count_output_file,
  font_family = font_family
)

message("Figure saved to: ", q36_selection_count_output_file)

q36_single_answer_distribution <- compute_q36_single_answer_distribution(
  survey = survey,
  option_labels = q36_option_labels
)

q36_single_answer_output_file <- output_path("figures", "q36_single_answer_distribution.png")

plot_q36_single_answer_distribution(
  distribution = q36_single_answer_distribution,
  output_file = q36_single_answer_output_file,
  font_family = font_family
)

message("Figure saved to: ", q36_single_answer_output_file)

q36_pairwise_lift <- compute_q36_pairwise_lift(
  survey = survey,
  option_labels = q36_option_labels
)

q36_pairwise_lift_output_file <- output_path("figures", "q36_pairwise_lift.png")

plot_q36_pairwise_lift(
  association = q36_pairwise_lift,
  output_file = q36_pairwise_lift_output_file,
  font_family = font_family
)

message("Figure saved to: ", q36_pairwise_lift_output_file)
