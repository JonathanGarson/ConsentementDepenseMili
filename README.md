# Consentement à la dépense de défense

## Project description

The objective of this project is to study the determinants of support for defence spending using a survey conducted by the Cour des Comptes on tax consent. The repository also uses public SIPRI military expenditure data to build the comparative figures used in the note.

This repository is organised as a reproducible R project. Raw data stay in `data/raw/`, cleaned and derived datasets are written to `data/final/`, and figures are written to `output/figures/`.

## Folder organisation

The project keeps source data, intermediate outputs, code, and final outputs in separate folders so that each step of the workflow is easy to rerun and audit.

```text
.
├── codes/
│   ├── analysis/
│   ├── generate/
│   └── statdesc/
├── data/
│   ├── final/
│   ├── raw/
│   └── temp/
├── latex_files/
├── output/
│   ├── figures/
│   └── tables/
├── renv/
├── resources/
├── .gitignore
├── Rprofile_template
└── README.md
```

- `codes/` contains the R scripts to generate datasets, produce descriptive outputs, and run the analysis.
- `data/raw/` contains immutable input files such as the Cour des Comptes survey workbook and the SIPRI workbook.
- `data/temp/` is reserved for temporary files created during future processing steps.
- `data/final/` contains cleaned and analysis-ready datasets produced by the scripts.
- `output/` stores figures and tables created by the code.
- `resources/` stores auxiliary files that support the project, such as crosswalks.
- `renv/` contains the project-local reproducibility infrastructure created by `renv`.
- `latex_files/` contains the note drafting material already present in this repository.

## Replication process

### 3.1 How to setup codes and path for reproduction

1. Clone or download the repository to your machine.

```bash
git clone <your-repository-url>
cd ConsentementDepenseMili
```

2. Create your local `.Rprofile` from the committed template. This file defines the helper functions used by the scripts and should stay uncommitted.

```bash
cp Rprofile_template .Rprofile
```

3. Run all commands from the repository root. The scripts now call `path_data_raw(...)`, `path_data_final(...)`, and `output_path(...)` directly, so they rely on the project `.Rprofile` being loaded automatically from the working directory.

4. Open R from the repository root and restore the package environment with `renv`.

```r
install.packages("renv")
renv::restore()
```

If `renv` is already installed on your computer, you only need `renv::restore()`. This step matters because the scripts rely on packages such as `data.table`, `readxl`, `ggplot2`, `fixest`, and `ragg`.

If you want to work strictly inside the project-local `renv` library in an interactive R session, you can then run:

```r
source("renv/activate.R")
```

5. Run the scripts from the repository root in the following order.

```bash
Rscript codes/generate/01_gen_survey2025.R
Rscript codes/generate/02_gen_share_gdp.R
Rscript codes/generate/03_gen_share_expenditure.R
Rscript codes/statdesc/01_miliexpenditure.R
```

Do not use `Rscript --vanilla`, because that would skip the project `.Rprofile` and the path helper functions. The first three scripts create cleaned datasets in `data/final/`. The descriptive script then reads those datasets and writes the figures to `output/figures/`.

6. The analysis entrypoint currently present in the repository is:

```bash
Rscript codes/analysis/01_defense_consent.R
```

At the moment, the checked-in version of this script sets up the analysis environment but does not yet export regression tables to `output/tables/`. If you extend the analysis, that folder is the intended destination for table outputs.

### 3.2 What each codes does

- `codes/generate/01_gen_survey2025.R` reads `data/raw/barometrePO2025_clean.xlsx`, writes an audit copy to `data/raw/barometrePO2025_clean.csv`, cleans the survey variables, and exports `data/final/final_survey2025.csv`.
- `codes/generate/02_gen_share_gdp.R` reads the `Share of GDP` sheet from `data/raw/SIPRI.xlsx` and exports a country-year panel to `data/final/share_gdp.csv`.
- `codes/generate/03_gen_share_expenditure.R` reads the `Share of Govt. spending` sheet from `data/raw/SIPRI.xlsx` and exports a country-year panel to `data/final/share_expenditure.csv`.
- `codes/statdesc/01_miliexpenditure.R` reads the two SIPRI-derived final datasets and creates the comparative figures in `output/figures/`.
- `codes/analysis/01_defense_consent.R` is the analysis entrypoint for the probit work on consent to defence spending. In the current checked-in version, it prepares access to the raw data directory but does not yet write output files.

### 3.3 Data outputs produced by each code

| Code or script | Output file | Location | Description |
| --- | --- | --- | --- |
| `codes/generate/01_gen_survey2025.R` | `barometrePO2025_clean.csv` | `data/raw/` | CSV audit copy created from the original Cour des Comptes Excel file. |
| `codes/generate/01_gen_survey2025.R` | `final_survey2025.csv` | `data/final/` | Cleaned survey dataset used for downstream analysis. |
| `codes/generate/02_gen_share_gdp.R` | `share_gdp.csv` | `data/final/` | Country-year panel of military expenditure as a share of GDP. |
| `codes/generate/03_gen_share_expenditure.R` | `share_expenditure.csv` | `data/final/` | Country-year panel of military expenditure as a share of government expenditure. |
| `codes/statdesc/01_miliexpenditure.R` | No dataset output | `-` | This script consumes final datasets and produces figures only. |
| `codes/analysis/01_defense_consent.R` | No dataset output in current version | `-` | The checked-in script does not yet export a dataset. |

### 3.4 Tables and figures produced by each code

| Code or script | Table or figure identifier | Output location | Description |
| --- | --- | --- | --- |
| `codes/statdesc/01_miliexpenditure.R` | `miliexpenditure_share_gdp.png` | `output/figures/` | Line chart comparing military expenditure as a share of GDP for the United States, France, Germany, the United Kingdom, and Poland. |
| `codes/statdesc/01_miliexpenditure.R` | `miliexpenditure_share_expenditure.png` | `output/figures/` | Line chart comparing military expenditure as a share of government expenditure for the same five countries. |

No checked-in table file is currently produced by `codes/analysis/01_defense_consent.R`. If you add regression exports, write them to `output/tables/` and extend this table accordingly.

### 3.5 Optional: publish the repository to GitHub

If you use the GitHub CLI, check that you are authenticated and then create the remote repository from the project root.

```bash
gh auth status
gh repo create
```

If you prefer to do it manually, you can use the following commands after creating an empty repository on GitHub:

```bash
git init
git add .
git commit -m "Initial commit"
git branch -M main
git remote add origin <your-github-url>
git push -u origin main
```

## Data availability statement

The SIPRI military expenditure data used in this repository are public. The other data come from the Cour des Comptes and are also public.

## Hardware used for computation, time it took

The project was prepared on a MacBook Pro M4 Max with 24 GB of RAM. The end-to-end runtime of the full pipeline has not yet been measured.
