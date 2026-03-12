#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggrepel)
  library(ragg)
  library(scales)
})

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

prepare_panel <- function(path, value_col, country_labels) {
  panel <- fread(path)

  if (!all(c("country", "year", value_col) %chin% names(panel))) {
    stop("Unexpected columns in input file: ", path, call. = FALSE)
  }

  panel <- panel[
    ,
    .(
      country,
      year = as.integer(year),
      value = as.numeric(get(value_col))
    )
  ]

  panel[
    ,
    country_label := country_labels[country]
  ]

  if (anyNA(panel$country_label)) {
    stop("Country label mapping is incomplete.", call. = FALSE)
  }

  panel[, country := factor(country, levels = names(country_labels))]
  panel[, country_label := factor(country_label, levels = unname(country_labels))]
  setorder(panel, country, year)
  panel
}

build_end_labels <- function(panel) {
  panel[!is.na(value), .SD[.N], by = country]
}

plot_country_lines <- function(panel,
                               title,
                               subtitle,
                               y_label,
                               output_file,
                               palette,
                               font_family) {
  end_labels <- build_end_labels(panel)
  max_year <- max(panel$year, na.rm = TRUE)
  min_year <- min(panel$year, na.rm = TRUE)

  p <- ggplot(panel, aes(x = year, y = value, color = country, group = country)) +
    geom_line(linewidth = 1.25, alpha = 0.95, na.rm = TRUE) +
    geom_point(
      data = end_labels,
      size = 2.6,
      stroke = 0.4,
      shape = 21,
      fill = "white",
      na.rm = TRUE
    ) +
    geom_text_repel(
      data = end_labels,
      aes(label = country_label),
      family = font_family,
      size = 4.2,
      direction = "y",
      hjust = 0,
      nudge_x = 2.6,
      box.padding = 0.22,
      point.padding = 0.18,
      force = 1.2,
      min.segment.length = 0,
      segment.color = "#9CA3AF",
      segment.size = 0.35,
      seed = 20260310,
      show.legend = FALSE
    ) +
    scale_color_manual(values = palette) +
    scale_x_continuous(
      breaks = seq(min_year, max_year, by = 4),
      limits = c(min_year, max_year + 5),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 0.1),
      expand = expansion(mult = c(0.03, 0.18))
    ) +
    labs(
      # title = title,
      # subtitle = subtitle,
      x = NULL,
      y = y_label,
      caption = "Source : SIPRI. Extraction et mise en forme par les auteurs"
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_family = font_family) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#ffffff", linewidth = 0.45),
      axis.text = element_text(color = "#374151", size = 11),
      axis.title.y = element_text(color = "#111827", size = 12, margin = margin(r = 12)),
      plot.title = element_text(color = "#111827", size = 23, face = "bold"),
      plot.subtitle = element_text(color = "#4B5563", size = 12.5, margin = margin(b = 14)),
      plot.caption = element_text(color = "#6B7280", size = 10, margin = margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "none",
      plot.margin = margin(t = 18, r = 115, b = 18, l = 18)
    )

  ggsave(
    filename = output_file,
    plot = p,
    device = agg_png,
    width = 10.8,
    height = 6.8,
    units = "in",
    dpi = 320,
    bg = "#FCFCF8"
  )
}

font_family <- pick_font_family()
figure_dir <- output_path("figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

country_labels <- c(
  "United States of America" = "États-Unis",
  "France" = "France",
  "Germany" = "Allemagne",
  "United Kingdom" = "Royaume-Uni",
  "Poland" = "Pologne"
)

palette <- c(
  "United States of America" = "#111827",
  "France" = "#0F766E",
  "Germany" = "#D97706",
  "United Kingdom" = "#2563EB",
  "Poland" = "#B91C1C"
)

share_gdp <- prepare_panel(
  path = path_data_final("share_gdp.csv"),
  value_col = "share_gdp",
  country_labels = country_labels
)

share_expenditure <- prepare_panel(
  path = path_data_final("share_expenditure.csv"),
  value_col = "share_expenditure",
  country_labels = country_labels
)

plot_country_lines(
  panel = share_gdp,
  # title = "Dépenses militaires en part du PIB",
  # subtitle = "Évolution annuelle de cinq pays sélectionnés, 1985-2024",
  y_label = "Part du PIB",
  output_file = output_path("figures", "miliexpenditure_share_gdp.png"),
  palette = palette,
  font_family = font_family
)

plot_country_lines(
  panel = share_expenditure,
  # title = "Dépenses militaires en part des dépenses publiques",
  # subtitle = "Évolution annuelle de cinq pays sélectionnés, 1988-2024",
  y_label = "Part des dépenses publiques",
  output_file = output_path("figures", "miliexpenditure_share_expenditure.png"),
  palette = palette,
  font_family = font_family
)

message("Figures saved to: ", figure_dir)
