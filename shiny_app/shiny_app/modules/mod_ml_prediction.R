# mod_ml_prediction.R — Clinician-facing AKI Risk Prediction
#   Uses XGBoost (AUROC 0.8003, AUPRC 0.4363, Brier 0.1075)
#   trained on MIMIC-IV MICE-imputed data (5 imputations, pooled by Rubin's rule).
#   Sidebar inputs: top 20 features by mean |SHAP| from population-level beeswarm analysis.
#   All other predictors are imputed with training-set medians.

# Helper: build a 1-row prediction data frame
build_pred_row <- function(input_vals) {
  # Start with NA for all predictor columns
  row <- as.list(rep(NA_real_, length(PREDICTOR_COLS)))
  names(row) <- PREDICTOR_COLS

  # Top-20 SHAP numeric inputs (entered by clinician)
  numeric_inputs <- c(
    # Clinical
    "age", "sofa_liver", "total_fluid_out_24h",
    "fluid_balance_24h", "pf_ratio", "bmi",
    # Vitals & Ventilator
    "spo2_mean", "fio2_mean", "gcs_motor_mean", "plateau_pressure_mean",
    # Vasopressor
    "vaso_dose_221653",
    # Labs
    "phosphate_mean", "bicarbonate_mean", "lactate_mean",
    "ptt_mean", "magnesium_mean", "platelet_mean"
  )

  for (feat in numeric_inputs) {
    val <- input_vals[[feat]]
    if (!is.null(val) && !is.na(val)) row[[feat]] <- as.numeric(val)
  }

  # Integer comorbidities (top-20: ckd, cad)
  row[["ckd"]] <- as.integer(isTRUE(input_vals[["ckd"]]))
  row[["cad"]] <- as.integer(isTRUE(input_vals[["cad"]]))
  # Structural zeros — not entered, not imputed
  for (flag in c("dm", "htn", "copd", "hf", "sepsis", "vasopressor_24h")) {
    row[[flag]] <- 0L
  }
  for (vaso in c("vaso_dose_221906", "vaso_dose_221289", "vaso_dose_221662")) {
    row[[vaso]] <- 0
  }
  row[["vasopressor_hours_24h"]] <- 0

  # Categorical (factors — required for step_dummy in recipe)
  row[["gender"]] <- factor("M", levels = c("M", "F"))
  row[["race"]] <- factor(input_vals[["race"]], levels = RACE_LEVELS)
  row[["admission_type"]] <- factor("EW EMER.", levels = ADMISSION_LEVELS)

  as_tibble(row)
}

# UI
mlPredictionUI <- function(id) {
  ns <- NS(id)

  if (is.null(xgb_fits)) {
    return(card(card_body(
      class = "text-center py-5",
      bs_icon("exclamation-triangle-fill", size = "3rem", class = "text-warning mb-3"),
      h4("ML models unavailable"),
      p(class = "text-muted",
        "ml_fitted_models.rds could not be loaded. Please check the data directory.")
    )))
  }

  layout_sidebar(
    sidebar = sidebar(
      title = "Patient Input",
      width = 340,

      # Labs
      tags$h6(class = "text-muted mt-2 mb-1",
              bs_icon("droplet"), " Labs (24h mean)"),
      fluidRow(
        column(6, numericInput(ns("phosphate_mean"), "Phosphate (mg/dL)",
                               value = 3.5, min = 0, max = 15, step = 0.1)),
        column(6, numericInput(ns("bicarbonate_mean"), "Bicarbonate (mEq/L)",
                               value = 22,  min = 5, max = 40, step = 0.5))
      ),
      fluidRow(
        column(6, numericInput(ns("lactate_mean"), "Lactate (mmol/L)",
                               value = 1.5,  min = 0, max = 20, step = 0.1)),
        column(6, numericInput(ns("ptt_mean"), "aPTT (sec)",
                               value = 30, min = 15, max = 150, step = 1))
      ),
      fluidRow(
        column(6, numericInput(ns("magnesium_mean"), "Magnesium (mg/dL)",
                               value = 2.0,  min = 0.5,  max = 6,    step = 0.1)),
        column(6, numericInput(ns("platelet_mean"), "Platelet (\u00d710\u00b3/\u00b5L)",
                               value = 180,  min = 10, max = 800,  step = 5))
      ),
      tags$hr(),

      # Vitals & Ventilator 
      tags$h6(class = "text-muted mb-1",
              bs_icon("lungs"), " Vitals & Ventilator (24h mean)"),
      fluidRow(
        column(6, numericInput(ns("spo2_mean"), "SpO2 (%)",
                               value = 97, min = 70, max = 100, step = 0.5)),
        column(6, numericInput(ns("fio2_mean"), "FiO2 (%)",
                               value = 40, min = 21, max = 100, step = 1))
      ),
      fluidRow(
        column(6, numericInput(ns("gcs_motor_mean"), "GCS Motor (1\u20136)",
                               value = 6, min = 1, max = 6, step = 1)),
        column(6, numericInput(ns("plateau_pressure_mean"), "Plateau Pressure (cmH\u2082O)",
                               value = 20, min = 5, max = 50, step = 1))
      ),
      numericInput(ns("vaso_dose_221653"), "Dobutamine dose (mcg/kg/min)",
                   value = 0, min = 0, max = 20, step = 0.5),
      tags$hr(),

      # Clinical 
      tags$h6(class = "text-muted mb-1",
              bs_icon("clipboard2-pulse"), " Clinical"),
      fluidRow(
        column(6, numericInput(ns("age"), "Age (years)",
                               value = 65, min = 18, max = 100,  step = 1)),
        column(6, numericInput(ns("sofa_liver"), "SOFA Liver (0\u20134)",
                               value = 0, min = 0, max = 4, step = 1))
      ),
      fluidRow(
        column(6, numericInput(ns("total_fluid_out_24h"), "Fluid Out 24h (mL)",
                               value = 1500, min = 0, max = 10000,step = 100)),
        column(6, numericInput(ns("fluid_balance_24h"), "Fluid Balance 24h (mL)",
                               value = 500, min = -5000,max = 10000,step = 100))
      ),
      fluidRow(
        column(6, numericInput(ns("pf_ratio"), "PF Ratio (PaO\u2082/FiO\u2082)",
                               value = 200, min = 50, max = 600, step = 5)),
        column(6, numericInput(ns("bmi"), "BMI (kg/m\u00b2)",
                               value = 27, min = 15, max = 60, step = 0.5))
      ),
      selectInput(ns("race"), "Race / Ethnicity",
                  choices = RACE_LEVELS, selected = "White"),
      tags$div(
        class = "d-flex gap-3 flex-wrap mt-1",
        checkboxInput(ns("ckd"), "CKD", value = FALSE),
        checkboxInput(ns("cad"), "CAD", value = FALSE)
      ),
      tags$hr(),
      actionButton(ns("predict_btn"), "Generate Prediction",
                   class = "btn-success btn-lg w-100",
                   icon = icon("heart-pulse")),
      actionButton(ns("reset_btn"), "Reset to Defaults",
                   class = "btn-outline-secondary btn-sm w-100 mt-2")
    ),

    # Main panel
    tagList(
      # Model performance context
      tags$p(
        class = "text-muted mb-1",
        style = "font-size: 0.82rem;",
        bs_icon("info-circle", class = "text-info me-1"),
        "XGBoost | AUROC = 0.800 | AUPRC = 0.436 | Brier = 0.108 | ",
        "Pooled across 5 MICE imputations | Optimal threshold: 0.13 (Youden's J = 0.478) | ",
        "Inputs: top 20 features by mean |SHAP|"
      ),
      tags$p(
        class = "text-muted mb-2",
        style = "font-size: 0.82rem;",
        bs_icon("mortarboard-fill", class = "text-secondary me-1"),
        tags$strong("Note:"),
        " For the purpose of this project, XGBoost is used as the prediction model.",
        " Although Logistic Regression achieved a marginally higher AUROC (0.8009 vs. 0.8003),",
        " the difference is not statistically significant (DeLong's test P = 0.936).",
        " XGBoost is selected for its ability to capture non-linear interactions",
        " and for providing SHAP-based interpretability."
      ),

      # Prediction result
      uiOutput(ns("pred_result_ui")),

      # Feature input summary
      card(
        class = "mb-2",
        card_header(
          tagList(bs_icon("table"), " Input Feature Summary (Top 20 SHAP Features)")
        ),
        card_body(class = "p-0", DTOutput(ns("input_summary_tbl")))
      ),

      # Clinical disclaimer
      tags$p(
        class = "text-muted mt-1 mb-0",
        style = "font-size: 0.80rem;",
        bs_icon("exclamation-triangle-fill", class = "text-warning me-1"),
        tags$strong("Research Use Only."),
        " Developed on MIMIC-IV for academic research.",
        " Not validated for clinical decision-making.",
        " Features not entered are imputed with training-set medians."
      )
    )
  )
}

# Server 
mlPredictionServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    pred_result <- reactiveVal(NULL)

    # Reset inputs 
    observeEvent(input$reset_btn, {
      defaults <- list(
        phosphate_mean = 3.5, bicarbonate_mean = 22,
        lactate_mean = 1.5, ptt_mean = 30,
        magnesium_mean = 2.0, platelet_mean = 180,
        spo2_mean = 97, fio2_mean = 40,
        gcs_motor_mean = 6, plateau_pressure_mean = 20,
        vaso_dose_221653 = 0,
        age = 65, sofa_liver = 0,
        total_fluid_out_24h = 1500, fluid_balance_24h = 500,
        pf_ratio = 200, bmi = 27
      )
      for (nm in names(defaults))
        updateNumericInput(session, nm, value = defaults[[nm]])
      updateSelectInput(session, "race", selected = "White")
      updateCheckboxInput(session, "ckd", value = FALSE)
      updateCheckboxInput(session, "cad", value = FALSE)
      pred_result(NULL)
    })

    # Generate prediction
    observeEvent(input$predict_btn, {
      if (is.null(xgb_fits)) {
        showNotification("XGBoost models not loaded.", type = "error")
        return()
      }

      withProgress(message = "Running XGBoost across 5 imputations...", value = 0.1, {
        input_vals <- reactiveValuesToList(input)
        new_row <- build_pred_row(input_vals)

        # Fill remaining NAs with training-set medians
        for (col in names(new_row)) {
          if (is.numeric(new_row[[col]]) && is.na(new_row[[col]])) {
            med_val <- ml_medians[[col]]
            if (!is.null(med_val) && !is.na(med_val))
              new_row[[col]] <- as.numeric(med_val)
          }
        }

        # Predict with each of 5 XGBoost workflows, pool by averaging
        probs <- tryCatch({
          purrr::map_dbl(xgb_fits, function(wf) {
            p <- predict(wf, new_data = new_row, type = "prob")
            p$.pred_AKI
          })
        }, error = function(e) {
          showNotification(paste("Prediction error:", e$message), type = "error")
          NULL
        })

        incProgress(0.8)

        if (!is.null(probs)) {
          pred_result(list(
            prob = mean(probs),
            prob_sd = sd(probs),
            prob_range = range(probs),
            threshold = 0.13,
            input_vals = input_vals
          ))
        }
      })
    })

    # Render prediction result
    output$pred_result_ui <- renderUI({
      if (is.null(pred_result())) {
        return(card(
          card_body(
            class = "text-center py-5",
            bs_icon("heart-pulse", size = "3rem", class = "text-muted mb-3"),
            h4("Enter patient values and click Generate Prediction",
               class = "text-muted")
          )
        ))
      }

      res <- pred_result()
      pct <- round(res$prob * 100, 1)
      sd_pct <- round(res$prob_sd * 100, 1)
      thresh <- res$threshold * 100  # 12%

      risk_level <- if (pct < 10) {
        list(label = "Low Risk", color = "success")
      } else if (pct < thresh) {
        list(label = "Borderline", color = "warning")
      } else if (pct < 30) {
        list(label = "Elevated Risk", color = "warning")
      } else {
        list(label = "High Risk", color = "danger")
      }

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_body(class = "text-center",
              plotlyOutput(session$ns("gauge_chart"), height = "280px")
            )
          ),
          card(
            card_header(
              tags$span(
                class = paste0("badge bg-", risk_level$color, " fs-5"),
                risk_level$label
              )
            ),
            card_body(
              tags$div(
                class = "mb-3",
                tags$span(class = "display-5 fw-bold",
                          paste0(pct, "%")),
                tags$span(class = "text-muted ms-2",
                          paste0("\u00b1 ", sd_pct, "% SD"))
              ),
              tags$hr(),
              tags$p(
                bs_icon("bar-chart-steps", class = "text-muted"),
                tags$span(class = "text-muted",
                          paste0("Range across 5 imputations: ",
                                 round(res$prob_range[1] * 100, 1), "% \u2013 ",
                                 round(res$prob_range[2] * 100, 1), "%"))
              ),
              tags$p(
                bs_icon("bullseye", class = "text-muted"),
                tags$span(class = "text-muted",
                          paste0("Optimal threshold: ", thresh, "% (Youden's J)"))
              ),
              tags$p(
                if (pct >= thresh) {
                  tagList(
                    bs_icon("exclamation-circle-fill", class = "text-danger"),
                    tags$strong(class = "text-danger",
                                " Above threshold \u2014 consider close monitoring")
                  )
                } else {
                  tagList(
                    bs_icon("check-circle-fill", class = "text-success"),
                    tags$span(class = "text-success",
                              " Below threshold")
                  )
                }
              )
            )
          )
        )
      )
    })

    output$gauge_chart <- renderPlotly({
      req(pred_result())
      res <- pred_result()
      pct <- round(res$prob * 100, 1)
      thresh <- res$threshold * 100

      risk_color <- if (pct < 10)          "#2ecc71"
                    else if (pct < thresh)  "#f39c12"
                    else if (pct < 30)      "#f39c12"
                    else                    "#e74c3c"

      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = pct,
        number = list(suffix = "%", font = list(size = 44, color = risk_color)),
        gauge = list(
          axis = list(range = list(0, 100), ticksuffix = "%", tickfont = list(size = 11)),
          bar = list(color = risk_color, thickness = 0.7),
          bgcolor = "white",
          borderwidth = 1,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 10), color = "#d5f5e3"),
            list(range = c(10, 20), color = "#fef9e7"),
            list(range = c(20, 40), color = "#fdebd0"),
            list(range = c(40, 100), color = "#fadbd8")
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            value = thresh
          )
        ),
        title = list(
          text = "AKI Stage 2+ Risk (7-day)",
          font = list(size = 14, color = "gray40")
        )
      ) %>%
        layout(
          height = 260,
          margin = list(t = 50, b = 10, l = 20, r = 20),
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    # Feature input summary table
    output$input_summary_tbl <- renderDT({
      all_labels <- tibble(
        rank = 1:20,
        feature = c(
          "phosphate_mean", "spo2_mean", "vaso_dose_221653", "bicarbonate_mean",
          "age", "total_fluid_out_24h", "ckd", "sofa_liver", "fio2_mean", "lactate_mean",
          "race", "gcs_motor_mean", "plateau_pressure_mean", "ptt_mean", "cad",
          "pf_ratio", "magnesium_mean", "platelet_mean", "fluid_balance_24h", "bmi"
        ),
        label = c(
          "Phosphate (mg/dL)", "SpO2 (%)", "Dobutamine dose (mcg/kg/min)",
          "Bicarbonate (mEq/L)", "Age (years)", "Fluid Out 24h (mL)",
          "CKD", "SOFA Liver (0\u20134)", "FiO2 (%)", "Lactate (mmol/L)",
          "Race / Ethnicity", "GCS Motor (1\u20136)", "Plateau Pressure (cmH\u2082O)",
          "aPTT (sec)", "CAD", "PF Ratio (PaO\u2082/FiO\u2082)",
          "Magnesium (mg/dL)", "Platelet (\u00d710\u00b3/\u00b5L)",
          "Fluid Balance 24h (mL)", "BMI (kg/m\u00b2)"
        )
      )

      vals <- if (!is.null(pred_result())) {
        iv <- pred_result()$input_vals
        all_labels %>%
          mutate(
            value = purrr::map_chr(feature, function(f) {
              v <- iv[[f]]
              if (is.null(v) || is.na(v)) return("\u2014")
              if (is.logical(v)) return(ifelse(v, "Yes", "No"))
              if (is.character(v)) return(v)
              as.character(round(as.numeric(v), 2))
            })
          )
      } else {
        all_labels %>% mutate(value = "\u2014")
      }

      vals %>%
        transmute(
          `SHAP Rank` = rank,
          Feature = label,
          `Entered Value` = value
        ) %>%
        datatable(
          rownames = FALSE,
          options = list(pageLength = 20, dom = "t", order = list()),
          class = "compact stripe"
        )
    })
  })
}

ml_prediction_teal_module <- function(label = "ML Prediction") {
  teal::module(
    label = label,
    ui = mlPredictionUI,
    server = function(id, data) { mlPredictionServer(id, data) },
    datanames = NULL  # no teal filter panel for this tab
  )
}
