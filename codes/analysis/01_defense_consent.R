# This code generates probit tables on the determinant of consent to defense spending

library(data.table)
library(fixest)

data_final_dir <- path_data_final()

consent = fread(file.path(data_final_dir, "final_survey2025.csv"))

# We want to analyse the determinant of consenting to military expenditure
feglm(, family = binomial(link = "probit"))