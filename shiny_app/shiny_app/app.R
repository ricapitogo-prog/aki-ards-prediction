# app.R — AKI Prediction Shiny App (teal)
# Run with: shiny::runApp("shiny_app") from the final_project/ directory

source("global.R")
source("modules/mod_cohort_explorer.R")
source("modules/mod_patient_trajectory.R")
source("modules/mod_ml_prediction.R")

app <- teal::init(
  data    = app_data,
  modules = teal::modules(
    cohort_explorer_teal_module(),
    patient_trajectory_teal_module(),
    ml_prediction_teal_module()
  ),
  title = tags$span(
    bsicons::bs_icon("lungs-fill"), " AKI Prediction — MIMIC-IV"
  ),
  header = tagList(
    tags$small(
      class = "text-muted",
      "MIMIC-IV v3.1 | AKI Stage 2+ Prediction | BIOSTAT 212A and 203B Final Project | Winter 2026"
    ),
    tags$style(HTML("
      /* ── Hide Filter Manager ── */
      .filter_hamburger,
      [id$='-show_filter_manager'],
      .teal-filter-manager-modal,
      .filter_manager_content {
        display: none !important;
      }

      /* ── Hide Snapshot Manager ── */
      [id$='-show_snapshot_manager'],
      .snapshot_manager_modal {
        display: none !important;
      }

      /* ── Hide Reporter (Add to Report + Show R code) ── */
      .report_add_wrapper,
      .simple_reporter_container,
      .reporter-menu,
      [id$='-reporter_menu_container'],
      [id$='-source_code_wrapper'] {
        display: none !important;
      }
    "))
  )
)

shiny::shinyApp(ui = app$ui, server = app$server)
