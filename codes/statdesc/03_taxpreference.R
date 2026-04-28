#!/usr/bin/env Rscript

# This script produces the descriptive figure on preferred taxes among
# respondents who support financing higher military spending through taxes.

# Setup ----
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ragg)
  library(scales)
})


# Labels and style constants ----

q37_option_labels <- c(
  q37_q1 = "Impôt sur le revenu",
  q37_q2 = "Taxe sur la valeur ajoutée (TVA)",
  q37_q3 = "Impôt sur les sociétés",
  q37_q4 = "Impôts sur le patrimoine",
  q37_q5 = "Cotisations salariales",
  q37_q6 = "Cotisations patronales",
  q37_q7 = "Contribution sociale généralisée (CSG)",
  q37_q8 = "Autre"
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

read_survey <- function(path, option_labels) {
  survey <- fread(path)
  required_columns <- c("weights", "q36_q2", names(option_labels))
  missing_columns <- setdiff(required_columns, names(survey))

  if (length(missing_columns) > 0) {
    stop(
      "Required columns missing from final_survey2025.csv: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  survey[, weights := as.numeric(weights)]
  survey[, q36_q2 := as.integer(q36_q2)]
  survey[, (names(option_labels)) := lapply(.SD, as.integer), .SDcols = names(option_labels)]

  if (all(is.na(survey$weights))) {
    stop("Column 'weights' is empty or non-numeric.", call. = FALSE)
  }

  survey
}

compute_q37_shares <- function(survey, option_labels) {
  option_cols <- names(option_labels)
  tax_sample <- survey[!is.na(weights) & weights > 0 & q36_q2 == 1L]
  tax_sample <- tax_sample[complete.cases(tax_sample[, ..option_cols])]

  if (nrow(tax_sample) == 0) {
    stop(
      "No valid observations found for q37 among respondents selecting q36_q2.",
      call. = FALSE
    )
  }

  shares <- rbindlist(
    lapply(
      option_cols,
      function(option_col) {
        data.table(
          option = option_labels[[option_col]],
          share = weighted.mean(tax_sample[[option_col]], w = tax_sample$weights)
        )
      }
    )
  )

  setorder(shares, share, option)
  shares[, option := factor(option, levels = option)]
  shares
}

plot_q37_distribution <- function(distribution,
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
      caption = paste(
        "Pondération : poids de propension.",
        "Champ : répondants proposant une hausse des impôts ou cotisations sociales en q36.",
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

survey <- read_survey(
  path = path_data_final("final_survey2025.csv"),
  option_labels = q37_option_labels
)


# Q37 figure ----

q37_distribution <- compute_q37_shares(
  survey = survey,
  option_labels = q37_option_labels
)

q37_output_file <- output_path("figures", "q37_taxpreference.png")

plot_q37_distribution(
  distribution = q37_distribution,
  output_file = q37_output_file,
  font_family = font_family
)

message("Figure saved to: ", q37_output_file)
