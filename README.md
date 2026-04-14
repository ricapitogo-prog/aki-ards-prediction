# Predicting Acute Kidney Injury in Mechanically Ventilated ICU Patients

A machine learning pipeline for predicting AKI Stage 2+ within 7 days of mechanical ventilation initiation, built on the [MIMIC-IV v3.1](https://physionet.org/content/mimiciv/3.1/) critical care database.

## Overview

Acute kidney injury (AKI) is a common and serious complication in mechanically ventilated ICU patients. Early prediction enables timely intervention — fluid management, nephrotoxin avoidance, and renal consultation. This project builds a complete predictive modeling pipeline from raw EHR data to interpretable model outputs.

## Pipeline

| Step | Script | Description |
|------|--------|-------------|
| 1 | `01_cohort_construction.Rmd` | Identify mechanically ventilated patients, apply clinical exclusion criteria (ESRD, elective surgery, pediatric), define AKI outcomes using KDIGO staging with imputed baseline creatinine |
| 2 | `02_feature_engineering.Rmd` | Extract 74 candidate predictors from labs, vitals, vasopressors, fluid balance, and comorbidities; derive BMI, P/F ratio, SOFA components |
| 3 | `03_imputation.Rmd` | Characterize missingness patterns; apply MICE (Multivariate Imputation by Chained Equations) for principled handling of missing data |
| 4 | `04_machine_learning.Rmd` | Train and evaluate logistic regression, random forest, XGBoost, and neural network models with pooled predictions across imputed datasets |
| 5 | `05_feature_analysis.Rmd` | SHAP-based feature importance and interpretation of model predictions |
| — | `shiny_app/` | Interactive Shiny dashboard for exploring cohort characteristics and model results |

## Key Results

*(Update this section with your final model performance metrics — AUC, sensitivity, specificity, calibration, etc.)*

## Skills Demonstrated

- **Clinical study design**: Reproducible cohort definition with CONSORT-style exclusion flow diagrams
- **Feature engineering**: Deriving clinically meaningful predictors from time-series EHR data (labs, vitals, medications)
- **Missing data handling**: MICE imputation with pre-imputation missingness analysis and sensitivity checks
- **Machine learning**: Multi-model comparison (logistic regression, RF, XGBoost, neural net) with proper cross-validation
- **Model interpretability**: SHAP values for global and local feature importance
- **Interactive reporting**: Shiny application for clinical stakeholder communication

## Data

This project uses [MIMIC-IV v3.1](https://physionet.org/content/mimiciv/3.1/), a freely-available critical care database. Access requires PhysioNet credentialing. **No patient data is included in this repository.**

## How to Run

1. Obtain MIMIC-IV access via [PhysioNet](https://physionet.org/content/mimiciv/3.1/)
2. Place data under `~/mimic/` (or update `mimic_path` in each script)
3. Run scripts in order: `01_` → `02_` → `03_` → `04_` → `05_`
4. Launch the Shiny app: `shiny::runApp("shiny_app/")`

### Requirements

- R ≥ 4.5
- Key packages: `tidyverse`, `tidymodels`, `xgboost`, `mice`, `DALEX`, `shiny`, `duckdb`

## Author

**Rica Mae Pitogo**
