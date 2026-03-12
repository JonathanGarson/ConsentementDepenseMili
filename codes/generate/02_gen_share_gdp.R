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
output_file <- path_data_final("share_gdp.csv")

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

raw_gdp <- as.data.table(read_excel(
  excel_path,
  sheet = "Share of GDP",
  skip = 5,
  col_types = "text"
))

setnames(raw_gdp, clean_names_stable(names(raw_gdp)))

year_cols <- names(raw_gdp)[grepl("^x?[0-9]{4}$", names(raw_gdp))]
year_lookup <- data.table(
  column_name = year_cols,
  year = as.integer(sub("^x", "", year_cols))
)[year >= 1985]

share_gdp <- raw_gdp[
  country %chin% target_countries,
  c("country", year_lookup$column_name),
  with = FALSE
]

missing_countries <- setdiff(target_countries, unique(share_gdp$country))
if (length(missing_countries) > 0) {
  stop(
    "Countries not found in Share of GDP sheet: ",
    paste(missing_countries, collapse = ", "),
    call. = FALSE
  )
}

share_gdp <- melt(
  share_gdp,
  id.vars = "country",
  variable.name = "year_column",
  value.name = "share_gdp",
  variable.factor = FALSE
)

share_gdp <- merge(
  share_gdp,
  year_lookup,
  by.x = "year_column",
  by.y = "column_name",
  all.x = TRUE,
  sort = FALSE
)

share_gdp[, share_gdp := normalize_measure(share_gdp)]
share_gdp[, year_column := NULL]
setcolorder(share_gdp, c("country", "year", "share_gdp"))
setorder(share_gdp, country, year)

fwrite(share_gdp, output_file, na = "")

message("GDP share dataset saved to: ", output_file)
message("Rows: ", nrow(share_gdp), " | Countries: ", uniqueN(share_gdp$country))
