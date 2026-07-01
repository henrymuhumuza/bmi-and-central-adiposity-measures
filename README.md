# NHANES Central Obesity and BMI Study

Reproducible Quarto/R workflow for:

**Central Obesity Versus Body Mass Index for Predicting Cardiometabolic Disease: A Cross-sectional Analysis of NHANES August 2021-August 2023**

The analysis compares body mass index, waist circumference, and waist-hip ratio for identifying hypertension, diabetes, chronic kidney disease, and cardiometabolic multimorbidity among US adults aged 20 years or older.

## Data Availability

This repository does not commit NHANES raw files, processed participant-level datasets, or rendered manuscript outputs.

NHANES August 2021-August 2023 public-use data are available from the National Center for Health Statistics:

<https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?Cycle=2021-2023>

The script `scripts/00_build_master.R` downloads the required public-use XPT files into `data/raw/` if they are not already present and rebuilds `data/processed/nhanes_master.csv` and `data/processed/nhanes_master.rds` locally.

## Project Structure

```text
R/                     Shared helper functions
scripts/               Reproducible analysis scripts
manuscript/            Quarto manuscript source
references/            Bibliography and citation style
data/                  Local NHANES data, ignored by Git
figures/               Generated figures, ignored by Git
tables/                Generated tables, ignored by Git
results/               Generated model outputs, ignored by Git
outputs/               Rendered manuscript outputs, ignored by Git
logs/                  Script logs, ignored by Git
```

## Run Order

Run from the project root:

```powershell
Rscript scripts/00_setup.R
Rscript scripts/00_build_master.R
Rscript scripts/01_data_preparation.R
Rscript scripts/02_descriptive_analysis.R
Rscript scripts/03_regression_models.R
Rscript scripts/04_roc_analysis.R
Rscript scripts/05_sensitivity_analyses.R
Rscript scripts/06_tables.R
Rscript scripts/07_figures.R
quarto render manuscript/manuscript.qmd
```

If `Rscript` is not on PATH, use the full path to `Rscript.exe`, for example:

```powershell
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' scripts\00_build_master.R
```

The rendered HTML manuscript is written under `outputs/manuscript/`.

## Notes

- Primary inference uses survey-weighted logistic regression with `wtmec2yr`, `sdmvpsu`, and `sdmvstra`.
- ROC/AUC analyses are secondary predictive discrimination analyses and are labeled as unweighted.
- Generated participant-level data files are intentionally excluded from version control.
