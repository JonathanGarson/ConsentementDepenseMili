#!/usr/bin/env Rscript

library(data.table)
library(readxl)
library(stringi)

excel_path <- path_data_raw("barometrePO2025_clean.xlsx")
raw_csv_path <- path_data_raw("barometrePO2025_clean.csv")
clean_csv_path <- path_data_final("final_survey2025.csv")

required_covariates <- c(
  "sexe",
  "matri",
  "diplome",
  "revenus_foyer",
  "recod_pcs_repondant"
)

clean_names_stable <- function(x) {
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x, perl = TRUE)
  x <- gsub("^_|_$", "", x, perl = TRUE)
  make.unique(x, sep = "_")
}

drop_rec_prefix <- function(x) {
  x <- sub("^rec_", "", x)
  make.unique(x, sep = "_")
}

trim_and_na <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

audit_convert_column <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  type.convert(x, as.is = TRUE, na.strings = c("", "NA"))
}

survey_rename_map <- function() {
  data.table(
    source_current = c(
      "pcs_rep", "uda5", "revenus_foyer", "dept", "age_1", "age3",
      "agglo_quota", "occup", "hab", "ref", "tailleagglo", "diplome_1",
      "weight", "diplome", "q3_1", "q3_2", "q3_3", "q3_4", "q3_5",
      "q4_1", "q4_2", "q4_3", "q4_4", "q4_5", "q4_6", "q4_7", "q8_1",
      "q9", "q1_25", "q2_25", "q3_25", "q4x25_1", "q4x25_2", "q4x25_3",
      "q4x25_4", "q4x25_5", "q4x25_6", "q4x25_6_other", "q4bisx25_1",
      "q4bisx25_2", "q4bisx25_3", "q4bisx25_4", "q4bisx25_5",
      "q4bisx25_5_other", "q10_1", "q5_25_1", "q6_25", "q7_25", "q12",
      "q13", "q14", "q15", "q16", "q8_25", "q9_25", "q9_25_7_other",
      "q10_25", "q11_25", "q12_25_1", "q12_25_2", "q19", "q22", "q25_1",
      "q25_2", "q25_3", "q25_4", "q25_5", "q25_6", "q26", "q30", "q31",
      "q28", "q13_25", "q14x25_1", "q14x25_2", "q14x25_3", "q14x25_4",
      "q15x25_1", "q15x25_2", "q15x25_3", "q15x25_4", "q15x25_5",
      "q15x25_6", "q15x25_7", "q15x25_8", "q15x25_8_other",
      "test_echelle", "q32_1", "q33_1", "q34_1", "q34_2", "q34_3", "q35",
      "q37", "q16_25", "q17_25", "q18_25_1", "q18_25_2", "q18_25_3",
      "q18_25_4", "q36", "q42", "q43", "q44", "q45_1", "q45_2", "q46_1",
      "q46_2", "q46_3", "q46_4", "q46_5", "q46_5_other", "q46_6", "q47",
      "q48", "q50", "q51"
    ),
    target_clean = c(
      "pcs", "grande_region", "tranche_revenus", "departement",
      "continuous_age", "tranche_age", "milieu_geo", "statut_logement",
      "typ_hab", "contrib_principal", "taille_agglo", "niv_etude",
      "weights", "diplome", "q3_q1", "q3_q2", "q3_q3", "q3_q4", "q3_q5",
      "q4_q1", "q4_q2", "q4_q3", "q4_q4", "q4_q5", "q4_q6", "q4_q7",
      "q8", "q14", "q9", "q10", "q11", "q12_q1", "q12_q2", "q12_q3",
      "q12_q4", "q12_q5", "q12_q6", "q12_q6_detailed", "q13_q1",
      "q13_q2", "q13_q3", "q13_q4", "q13_q5", "q13_q5_detailed", "q15",
      "q16", "q17", "q18", "q19", "q20", "q21", "q22", "q23", "q24",
      "q25", "q25_detailed", "q26", "q27", "q27bis_q1", "q27bis_q2",
      "q28", "q29", "q30_q1", "q30_q2", "q30_q3", "q30_q4", "q30_q5",
      "q30_q6", "q31", "q32", "q33", "q34", "q35", "q36_q1", "q36_q2",
      "q36_q3", "q36_q4", "q37_q1", "q37_q2", "q37_q3", "q37_q4",
      "q37_q5", "q37_q6", "q37_q7", "q37_q8", "q37_q8_detailed",
      "ladder_test", "q38", "q39", "q40_q1", "q40_q2", "q40_q3", "q41",
      "q42", "q43", "q44", "q45_q1", "q45_q2", "q45_q3", "q45_q4",
      "q46", "q47", "q48", "q49", "q50_q1", "q50_q2", "q51_1", "q51_2",
      "q51_3", "q51_4", "q51_5", "q51_5_detailed", "q51_6", "q52", "q53",
      "q54", "q55"
    )
  )
}

apply_do_renames <- function(dt, rename_pairs) {
  rename_pairs <- rename_pairs[source_current != target_clean]

  if (nrow(rename_pairs) == 0) {
    return(invisible(dt))
  }

  target_collisions <- rename_pairs[
    target_clean %chin% names(dt) & !target_clean %chin% source_current
  ]
  if (nrow(target_collisions) > 0) {
    stop(
      "Rename target already exists: ",
      paste(unique(target_collisions$target_clean), collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(rename_pairs$target_clean)) {
    dup_targets <- unique(rename_pairs$target_clean[duplicated(rename_pairs$target_clean)])
    stop(
      "Duplicate rename targets in .do mapping: ",
      paste(dup_targets, collapse = ", "),
      call. = FALSE
    )
  }

  rename_pairs[, temp_name := sprintf("__rename_tmp_%03d__", .I)]
  setnames(dt, rename_pairs$source_current, rename_pairs$temp_name)
  setnames(dt, rename_pairs$temp_name, rename_pairs$target_clean)
}

if (!file.exists(excel_path)) {
  stop("Input file not found: ", excel_path, call. = FALSE)
}

dir.create(dirname(clean_csv_path), recursive = TRUE, showWarnings = FALSE)

raw_survey <- as.data.table(read_excel(excel_path, sheet = 1, col_names = TRUE))
raw_survey <- raw_survey[-1]
rename_pairs <- survey_rename_map()

fwrite(raw_survey, raw_csv_path, na = "")

clean_survey <- copy(raw_survey)
setnames(clean_survey, clean_names_stable(names(clean_survey)))
setnames(clean_survey, drop_rec_prefix(names(clean_survey)))

character_cols <- names(clean_survey)[vapply(clean_survey, is.character, logical(1))]
for (col in character_cols) {
  set(clean_survey, j = col, value = trim_and_na(clean_survey[[col]]))
}

for (col in names(clean_survey)) {
  set(clean_survey, j = col, value = audit_convert_column(clean_survey[[col]]))
}

missing_required <- setdiff(required_covariates, names(clean_survey))
if (length(missing_required) > 0) {
  stop(
    "Required covariates missing after name cleaning: ",
    paste(missing_required, collapse = ", "),
    call. = FALSE
  )
}

clean_survey[, sexe := ifelse(sexe == "Une femme", 0, 1)]
clean_survey[, final_sample := as.integer(complete.cases(.SD)), .SDcols = required_covariates]
apply_do_renames(clean_survey, rename_pairs)
clean_survey[, treated := ifelse(condition == "Indice", 1, 0)]
clean_survey[, q16 := as.numeric(sub("\\D+", "", q16))]

fwrite(clean_survey, clean_csv_path, na = "")

message("Raw CSV saved to: ", raw_csv_path)
message("Clean CSV saved to: ", clean_csv_path)
message("Rows: ", nrow(clean_survey), " | Columns: ", ncol(clean_survey))
