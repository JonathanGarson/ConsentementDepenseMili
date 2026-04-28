# Consentement à la dépense de défense

## Project description

The objective of this project is to study the determinants of support for defence spending using a survey conducted by the Cour des Comptes on tax consent. The repository also uses public SIPRI military expenditure data to build comparative descriptive figures for the note.

This repository is organised as a reproducible R project. Raw data stay in `data/raw/`, cleaned and derived datasets are written to `data/final/`, figures are written to `output/figures/`, and model tables are written to `output/tables/`.

The main unit of analysis for the survey work is the individual respondent. The comparative expenditure figures use country-year SIPRI panels.

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
- `data/raw/` contains immutable input files such as the Cour des Comptes survey workbook and the SIPRI workbook. These files are not committed to git.
- `data/temp/` is reserved for temporary files created during future processing steps.
- `data/final/` contains cleaned and analysis-ready datasets produced by the scripts. These files are regenerated from `data/raw/`.
- `output/figures/` stores figures created by the descriptive and analysis scripts.
- `output/tables/` stores regression tables, post-LASSO tables, and other tabular outputs created by the analysis scripts.
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

2. Place the required raw input files in `data/raw/`.

The current pipeline expects these file names:

```text
data/raw/barometrePO2025_clean.xlsx
data/raw/SIPRI.xlsx
```

Keep the raw files unchanged. If a new version of a raw file is used, keep a copy of the original source file and document the change before rerunning the generation scripts.

3. Create your local `.Rprofile` from the committed template. This file defines the helper functions used by the scripts and should stay uncommitted.

```bash
cp Rprofile_template .Rprofile
```

The template defines `path_data_raw(...)`, `path_data_temp(...)`, `path_data_final(...)`, and `output_path(...)`. The scripts use these helpers instead of absolute paths, so the project can be moved to another computer without rewriting file paths.

If you run commands from another working directory, set the environment variable `R_PROJECT_ROOT` to the repository root before starting R or before running `Rscript`. In the standard setup, no edit is needed: run all commands from the repository root. If raw data are stored elsewhere, copy them into `data/raw/` for replication or adjust your local `.Rprofile` without committing that local change.

4. Restore the package environment with `renv`.

```r
install.packages("renv")
renv::restore()
```

If `renv` is already installed on your computer, you only need `renv::restore()`. This step installs the package versions recorded in `renv.lock`.

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
Rscript codes/statdesc/02_hetero_partisane.R
Rscript codes/statdesc/03_taxpreference.R
Rscript codes/statdesc/04_defense_expenditure_pref.R

Rscript codes/analysis/01_defense_consent.R
Rscript codes/analysis/02_otherexp_consent.R
Rscript codes/analysis/03_defense_consent_model3_figures.R
Rscript codes/analysis/04_lasso.R
```

Do not use `Rscript --vanilla`, because that skips the project `.Rprofile` and the path helper functions.

The generation scripts write cleaned datasets to `data/final/`. The descriptive scripts write figures to `output/figures/`. The analysis scripts write LaTeX tables, CSV summary outputs, and model figures to `output/tables/` and `output/figures/`.

The scripts are designed to stop with explicit errors if required files, required columns, or valid positive survey weights are missing.

### 3.2 What each codes does

- `codes/generate/01_gen_survey2025.R` reads `data/raw/barometrePO2025_clean.xlsx`, writes an audit copy to `data/raw/barometrePO2025_clean.csv`, cleans and renames survey variables, builds binary expenditure outcomes, and exports `data/final/final_survey2025.csv`.
- `codes/generate/02_gen_share_gdp.R` reads the `Share of GDP` sheet from `data/raw/SIPRI.xlsx` and exports a country-year panel to `data/final/share_gdp.csv`.
- `codes/generate/03_gen_share_expenditure.R` reads the `Share of Govt. spending` sheet from `data/raw/SIPRI.xlsx` and exports a country-year panel to `data/final/share_expenditure.csv`.
- `codes/statdesc/01_miliexpenditure.R` reads the two SIPRI-derived final datasets and creates comparative military expenditure figures.
- `codes/statdesc/02_hetero_partisane.R` reads `final_survey2025.csv` and creates figures on partisan heterogeneity, preferred financing of higher military spending, and financing preferences by defence-spending support.
- `codes/statdesc/03_taxpreference.R` creates the descriptive figure on preferred taxes among respondents who support financing higher military spending through taxes.
- `codes/statdesc/04_defense_expenditure_pref.R` creates the descriptive figure on preferences about lowering military spending.
- `codes/analysis/01_defense_consent.R` estimates the main weighted probit models for refusal to lower defence spending and exports the main average marginal effect tables.
- `codes/analysis/02_otherexp_consent.R` mirrors the defence-spending analysis for health, pension, and poverty expenditure outcomes.
- `codes/analysis/03_defense_consent_model3_figures.R` refits the controlled defence model and exports average marginal effect figures for social group, party proximity, pooled party proximity, and diploma categories.
- `codes/analysis/04_lasso.R` screens respondent characteristics with weighted logistic LASSO, refits selected specifications as weighted probits, and exports selected-variable and post-LASSO tables.

### 3.3 Data outputs produced by each code

| Code or script | Output file | Location | Description |
| --- | --- | --- | --- |
| `codes/generate/01_gen_survey2025.R` | `barometrePO2025_clean.csv` | `data/raw/` | CSV audit copy created from the original Cour des Comptes Excel file. |
| `codes/generate/01_gen_survey2025.R` | `final_survey2025.csv` | `data/final/` | Cleaned survey dataset used for descriptive figures and downstream analysis. |
| `codes/generate/02_gen_share_gdp.R` | `share_gdp.csv` | `data/final/` | Country-year panel of military expenditure as a share of GDP. |
| `codes/generate/03_gen_share_expenditure.R` | `share_expenditure.csv` | `data/final/` | Country-year panel of military expenditure as a share of government expenditure. |
| `codes/statdesc/01_miliexpenditure.R` | No dataset output | `-` | This script consumes final datasets and produces figures only. |
| `codes/statdesc/02_hetero_partisane.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces figures only. |
| `codes/statdesc/03_taxpreference.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces a figure only. |
| `codes/statdesc/04_defense_expenditure_pref.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces a figure only. |
| `codes/analysis/01_defense_consent.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces LaTeX tables. |
| `codes/analysis/02_otherexp_consent.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces a LaTeX table. |
| `codes/analysis/03_defense_consent_model3_figures.R` | No dataset output | `-` | This script consumes `final_survey2025.csv` and produces model figures. |
| `codes/analysis/04_lasso.R` | `lasso_selected_variables.csv` | `output/tables/` | Summary of selected LASSO source variables by specification and lambda choice. |

### 3.4 Tables and figures produced by each code

| Code or script | Table or figure identifier | Output location | Description |
| --- | --- | --- | --- |
| `codes/statdesc/01_miliexpenditure.R` | `miliexpenditure_share_gdp.png` | `output/figures/` | Line chart comparing military expenditure as a share of GDP for the United States, France, Germany, the United Kingdom, and Poland. |
| `codes/statdesc/01_miliexpenditure.R` | `miliexpenditure_share_expenditure.png` | `output/figures/` | Line chart comparing military expenditure as a share of government expenditure for the same five countries. |
| `codes/statdesc/02_hetero_partisane.R` | `hetero_partisane_q35.png` | `output/figures/` | Weighted distribution of perceived conflict risk by partisan proximity. |
| `codes/statdesc/02_hetero_partisane.R` | `hetero_partisane_q30_q2.png` | `output/figures/` | Weighted distribution of trust in the state by partisan proximity. |
| `codes/statdesc/02_hetero_partisane.R` | `q36_effort_militaire.png` | `output/figures/` | Weighted shares for preferred ways to finance higher military spending. |
| `codes/statdesc/02_hetero_partisane.R` | `q36_effort_militaire_by_support.png` | `output/figures/` | Weighted q36 financing preferences by support for maintaining military spending. |
| `codes/statdesc/03_taxpreference.R` | `q37_taxpreference.png` | `output/figures/` | Preferred tax instruments among respondents selecting tax or contribution increases for q36. |
| `codes/statdesc/04_defense_expenditure_pref.R` | `q34_defense_expenditure_pref.png` | `output/figures/` | Weighted distribution of preferences about lowering military spending. |
| `codes/analysis/01_defense_consent.R` | `defense_ame_baseline_controls.tex` | `output/tables/` | Main LaTeX table of average marginal effects from weighted probit models with continuous age. |
| `codes/analysis/01_defense_consent.R` | `defense_ame_baseline_controls_altage.tex` | `output/tables/` | Alternative LaTeX table using categorical age groups. |
| `codes/analysis/02_otherexp_consent.R` | `otherexp_ame_baseline.tex` | `output/tables/` | LaTeX table for refusal to lower health, pension, and poverty expenditure. |
| `codes/analysis/03_defense_consent_model3_figures.R` | `defense_model3_ame_pcs.png` | `output/figures/` | Average marginal effects from model 3 by socio-professional category. |
| `codes/analysis/03_defense_consent_model3_figures.R` | `defense_model3_ame_partisane.png` | `output/figures/` | Average marginal effects from model 3 by detailed partisan proximity. |
| `codes/analysis/03_defense_consent_model3_figures.R` | `defense_model3_ame_partisane_pooled.png` | `output/figures/` | Average marginal effects from model 3 by pooled partisan proximity. |
| `codes/analysis/03_defense_consent_model3_figures.R` | `defense_model3_ame_diplome.png` | `output/figures/` | Average marginal effects from model 3 by diploma category. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_01_attitudes.tex` | `output/tables/` | Post-LASSO weighted probit AME table for attitude variables. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_02_demographie.tex` | `output/tables/` | Post-LASSO weighted probit AME table for demographic variables. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_03_revenus_activite.tex` | `output/tables/` | Post-LASSO weighted probit AME table for income and activity variables. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_04_diplome_pcs.tex` | `output/tables/` | Post-LASSO weighted probit AME table for diploma, education, and socio-professional category variables. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_05_politique.tex` | `output/tables/` | Post-LASSO weighted probit AME table for political variables. |
| `codes/analysis/04_lasso.R` | `lasso_postlasso_ame_06_territoire.tex` | `output/tables/` | Post-LASSO weighted probit AME table for territorial variables. |

Some exploratory or legacy HTML files may also be present in `output/tables/` after local work. The files listed above are the documented outputs of the current replication pipeline.

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

To replicate the project, obtain the raw files and place them in `data/raw/` with the exact names `barometrePO2025_clean.xlsx` and `SIPRI.xlsx`. The repository does not commit raw data files; it commits the code and folder structure needed to rebuild the derived datasets and outputs.

## Hardware used for computation, time it took

The project was prepared on a MacBook Pro M4 Max with 24 GB of RAM. The end-to-end runtime of the full pipeline has not yet been measured.
