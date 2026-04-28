#!/usr/bin/env Rscript

# This script produces the descriptive figure on preferences about lowering
# military spending.

# Setup ----
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ragg)
  library(scales)
})


# Labels and style constants ----

q34_response_order <- c(
  "Oui, tout à fait",
  "Oui, plutôt",
  "Non, plutôt pas",
  "Non, pas du tout",
  "Vous ne savez pas"
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

  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

read_survey <- function(path) {
  survey <- fread(path)
  required_columns <- c("weights", "q34")
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
      q34 = empty_to_na(q34),
      weights = as.numeric(weights)
    )
  ]

  if (all(is.na(survey$weights))) {
    stop("Column 'weights' is empty or non-numeric.", call. = FALSE)
  }

  survey
}

compute_q34_shares <- function(survey, response_order) {
  distribution <- survey[
    !is.na(weights) & weights > 0 & q34 %chin% response_order,
    .(weighted_n = sum(weights)),
    by = q34
  ]

  if (nrow(distribution) == 0) {
    stop("No valid observations found for q34.", call. = FALSE)
  }

  distribution <- distribution[
    data.table(q34 = response_order),
    on = "q34"
  ]

  distribution[is.na(weighted_n), weighted_n := 0]
  distribution[, share := weighted_n / sum(weighted_n)]
  distribution[, response := factor(q34, levels = rev(response_order))]
  distribution[]
}

plot_q34_distribution <- function(distribution,
                                  output_file,
                                  font_family) {
  label_fun <- label_percent(accuracy = 0.1)

  p <- ggplot(distribution, aes(x = share, y = response)) +
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
      caption = "Pondération : poids de propension."
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


# Data preparation ----

font_family <- pick_font_family()
figure_dir <- output_path("figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

survey <- read_survey(path_data_final("final_survey2025.csv"))


# Q34 figure ----

q34_distribution <- compute_q34_shares(
  survey = survey,
  response_order = q34_response_order
)

q34_output_file <- output_path("figures", "q34_defense_expenditure_pref.png")

plot_q34_distribution(
  distribution = q34_distribution,
  output_file = q34_output_file,
  font_family = font_family
)

message("Figure saved to: ", q34_output_file)
