#!/usr/bin/env Rscript

library(data.table)
library(readxl)
library(stringi)

clean_names_stable <- function(x) {
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x, perl = TRUE)
  x <- gsub("^_|_$", "", x, perl = TRUE)
  make.unique(x, sep = "_")
}

normalize_measure <- function(x) {
  x <- trimws(x)
  x[x %chin% c("", "...")] <- NA_character_
  as.numeric(x)
}

excel_path <- path_data_raw("SIPRI.xlsx")
output_file <- path_data_final("share_expenditure.csv")

target_countries <- c(
  "United States of America",
  "France",
  "Germany",
  "United Kingdom",
  "Poland"
)

if (!file.exists(excel_path)) {
  stop("Input file not found: ", excel_path, call. = FALSE)
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

raw_expenditure <- as.data.table(read_excel(
  excel_path,
  sheet = "Share of Govt. spending",
  skip = 7,
  col_types = "text"
))

setnames(raw_expenditure, clean_names_stable(names(raw_expenditure)))

year_cols <- names(raw_expenditure)[grepl("^x?[0-9]{4}$", names(raw_expenditure))]
year_lookup <- data.table(
  column_name = year_cols,
  year = as.integer(sub("^x", "", year_cols))
)[year >= 1988]

share_expenditure <- raw_expenditure[
  country %chin% target_countries,
  c("country", year_lookup$column_name),
  with = FALSE
]

missing_countries <- setdiff(target_countries, unique(share_expenditure$country))
if (length(missing_countries) > 0) {
  stop(
    "Countries not found in Share of Govt. spending sheet: ",
    paste(missing_countries, collapse = ", "),
    call. = FALSE
  )
}

share_expenditure <- melt(
  share_expenditure,
  id.vars = "country",
  variable.name = "year_column",
  value.name = "share_expenditure",
  variable.factor = FALSE
)

share_expenditure <- merge(
  share_expenditure,
  year_lookup,
  by.x = "year_column",
  by.y = "column_name",
  all.x = TRUE,
  sort = FALSE
)

share_expenditure[, share_expenditure := normalize_measure(share_expenditure)]
share_expenditure[, year_column := NULL]
setcolorder(share_expenditure, c("country", "year", "share_expenditure"))
setorder(share_expenditure, country, year)

fwrite(share_expenditure, output_file, na = "")

message("Expenditure share dataset saved to: ", output_file)
message("Rows: ", nrow(share_expenditure), " | Countries: ", uniqueN(share_expenditure$country))
