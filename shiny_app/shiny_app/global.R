# global.R — loaded once at app startup

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(teal)
  library(teal.data)
  library(tidymodels)
  library(shapviz)
  library(forcats)
})

# ── Data path ──────────────────────────────────────────────────────────────────
DATA_PATH <- {
  candidates <- c("..", ".", "../final_project")
  found <- Filter(function(p) file.exists(file.path(p, "final_cohort_imputed_baseline.rds")),
                  candidates)
  if (length(found) == 0)
    stop("Cannot find data files. Run from final_project/ or final_project/shiny_app/.")
  found[1]
}

# ── Load core cohort datasets ──────────────────────────────────────────────────
message("Loading cohort data...")
cohort          <- readRDS(file.path(DATA_PATH, "final_cohort_imputed_baseline.rds"))
aki_staging     <- readRDS(file.path(DATA_PATH, "aki_staging_at_intubation_imputed.rds"))
exclusion_lists <- readRDS(file.path(DATA_PATH, "exclusion_patient_lists_v2_imputed.rds"))
message("Core data loaded.")

# ── Derived datasets ───────────────────────────────────────────────────────────
aki_cohort <- cohort %>%
  left_join(
    aki_staging %>%
      select(stay_id, aki_stage, baseline_creat,
             max_creat_at_intubation, creat_fold_increase),
    by = "stay_id"
  ) %>%
  mutate(race = case_when(
    grepl("WHITE",    toupper(race)) ~ "White",
    grepl("BLACK",    toupper(race)) ~ "Black",
    grepl("HISPANIC", toupper(race)) ~ "Hispanic",
    grepl("ASIAN",    toupper(race)) ~ "Asian",
    TRUE                             ~ "Other/Unknown"
  ))

# Lookup: hadm_id → stay_id + aki_stage (for joining with lab data)
hadm_to_aki <- cohort %>%
  select(subject_id, hadm_id, stay_id) %>%
  left_join(aki_staging %>% select(stay_id, aki_stage), by = "stay_id")

# ── Exclusion flow ─────────────────────────────────────────────────────────────
n_excluded_aki2plus <- nrow(exclusion_lists$stays_with_aki_stage2_plus)
n_staging           <- nrow(aki_staging)

exclusion_flow <- tibble(
  step = c(
    paste0("After all clinical exclusions\n& baseline creatinine\nn = ",
           format(n_staging, big.mark = ",")),
    paste0("FINAL COHORT\n(−",
           format(n_excluded_aki2plus, big.mark = ","), " AKI Stage 2+)")
  ),
  n        = c(n_staging, nrow(cohort)),
  bar_fill = c("initial", "final")
) %>%
  mutate(
    n_excluded    = c(0, abs(diff(n))),
    pct_remaining = round(n / n_staging * 100, 1),
    step          = factor(step, levels = step)
  )

# ── AKI color palette ──────────────────────────────────────────────────────────
aki_colors <- c(
  "No AKI"  = "#2ecc71",
  "Stage 1" = "#f39c12",
  "Stage 2" = "#e67e22",
  "Stage 3" = "#e74c3c"
)

# ── Lab data (lazy) ────────────────────────────────────────────────────────────
LAB_DATA_PATH     <- file.path(DATA_PATH, "aki_labs_long.rds")
FEAT_IMPUTED_PATH <- file.path(DATA_PATH, "feature_matrix_imputed_stacked.rds")

LAB_CATEGORIES <- c(
  "creatinine", "bun", "potassium", "sodium", "bicarbonate",
  "hemoglobin", "hematocrit", "platelet", "wbc", "albumin",
  "lactate", "glucose", "chloride", "calcium", "magnesium",
  "phosphate", "ph", "pao2", "paco2", "bnp",
  "troponin", "procalcitonin", "bilirubin", "inr", "ptt", "fibrinogen"
)

# ── ML Models (5 fits per model across MICE imputations) ──────────────────────
message("Loading ML models (141 MB)...")
logit_fits <- NULL
xgb_fits   <- NULL
tryCatch({
  ml         <- readRDS(file.path(DATA_PATH, "ml_fitted_models.rds"))
  logit_fits <<- ml$logit   # Logistic Regression (AUROC 0.8009)
  xgb_fits   <<- ml$xgb
}, error = function(e) {
  warning("Could not load ML models: ", e$message)
})
message(if (!is.null(logit_fits)) "ML models loaded." else "ML models unavailable.")

# ── ML feature defaults (medians for pre-filling input form) ──────────────────
AKI_PREVALENCE    <- NA_real_
IMPUTED_LAB_CATS  <- LAB_CATEGORIES   # fallback: all labs; overwritten below if file loads
ml_medians <- tryCatch({
  feat           <- readRDS(file.path(DATA_PATH, "feature_matrix_preimputation.rds"))
  AKI_PREVALENCE <<- mean(feat$aki_stage2_7d == 1L, na.rm = TRUE) * 100
  # Labs kept for MICE: those whose {lab}_mean column has ≤50% missingness
  IMPUTED_LAB_CATS <<- LAB_CATEGORIES[
    vapply(paste0(LAB_CATEGORIES, "_mean"), function(col)
      col %in% names(feat) && mean(is.na(feat[[col]])) <= 0.50,
      logical(1))
  ]
  feat %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), ~median(.x, na.rm = TRUE))) %>%
    as.list()
}, error = function(e) { message("WARNING: ml_medians failed: ", e$message); list() })

# ── ML predictor columns (must match training recipe) ─────────────────────────
PREDICTOR_COLS <- c(
  "age", "gender", "admission_type",
  "bicarbonate_mean", "chloride_mean", "glucose_mean", "hematocrit_mean",
  "hemoglobin_mean", "inr_mean", "magnesium_mean", "paco2_mean",
  "pao2_mean", "ph_mean", "platelet_mean", "potassium_mean",
  "prothrombin_mean", "ptt_mean", "sodium_mean", "troponin_mean",
  "wbc_mean", "albumin_mean", "bilirubin_mean", "calcium_mean",
  "lactate_mean", "phosphate_mean", "fibrinogen_mean", "bnp_mean",
  "fio2_mean", "gcs_eye_mean", "gcs_motor_mean", "gcs_verbal_mean",
  "heart_rate_mean", "map_invasive_mean", "map_noninvasive_mean",
  "peep_set_mean", "peep_total_mean", "plateau_pressure_mean",
  "resp_rate_obs_mean", "resp_rate_set_mean",
  "sbp_invasive_mean", "sbp_noninvasive_mean",
  "spo2_mean", "temp_f_mean", "tidal_vol_obs_mean", "tidal_vol_set_mean",
  "peep_mean", "driving_pressure_mean", "map_mean", "gcs_total_mean",
  "total_fluid_in_24h", "total_fluid_out_24h", "fluid_balance_24h",
  "vasopressor_24h", "vasopressor_hours_24h",
  "vaso_dose_221906", "vaso_dose_221289", "vaso_dose_221653", "vaso_dose_221662",
  "height_cm", "weight_kg",
  "dm", "htn", "ckd", "copd", "hf", "cad", "sepsis",
  "race",
  "bmi", "pf_ratio",
  "sofa_resp", "sofa_coag", "sofa_liver", "sofa_cardio", "sofa_neuro"
)

RACE_LEVELS       <- c("White", "Black", "Hispanic", "Asian", "Other/Unknown")
ADMISSION_LEVELS  <- c("DIRECT EMER.", "DIRECT OBSERVATION", "EU OBSERVATION",
                       "EW EMER.", "OBSERVATION ADMIT", "URGENT")

# ── ML performance table ───────────────────────────────────────────────────────
ML_PERF <- tryCatch(
  read.csv(file.path(DATA_PATH, "ml_performance_table.csv")),
  error = function(e) NULL
)

# ── Teal data object ───────────────────────────────────────────────────────────
app_data <- teal_data(
  aki_cohort = aki_cohort,
  aki_staging     = aki_staging
)
