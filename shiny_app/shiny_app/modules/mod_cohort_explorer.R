# mod_cohort_explorer.R — Cohort Explorer (consolidates Overview, Demographics,
#                          AKI Staging, Exclusion Flow, Lab Explorer)

cohortExplorerUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("cohort_tabs"),

    # Tab 1: Overview
    nav_panel(
      title = tagList(bs_icon("house-fill"), " Overview"),
      tagList(
        h4("AKI in Mechanically Ventilated ICU Patients — MIMIC-IV v3.1",
           class = "text-muted mb-4"),
        layout_columns(
          col_widths = c(4, 4, 4, 4, 4, 4),
          value_box(
            title = "ICU Stays",
            value = textOutput(ns("n_stays")),
            showcase = bs_icon("hospital"),
            theme = "primary"
          ),
          value_box(
            title = "Unique Patients",
            value = textOutput(ns("n_patients")),
            showcase = bs_icon("people"),
            theme = "info"
          ),
          value_box(
            title = "Hospital Mortality",
            value = textOutput(ns("mortality")),
            showcase = bs_icon("heartbreak"),
            theme = "danger"
          ),
          value_box(
            title = "Cohort Prevalence Rate",
            value = textOutput(ns("aki_rate")),
            showcase = bs_icon("activity"),
            theme = "warning"
          ),
          value_box(
            title = "Median ICU LOS",
            value = textOutput(ns("icu_los")),
            showcase = bs_icon("calendar3"),
            theme = "success"
          ),
          value_box(
            title = "Median Vent Duration",
            value = textOutput(ns("vent_dur")),
            showcase = bs_icon("lungs"),
            theme = "secondary"
          )
        ),
        card(
          card_header("Cohort Overview"),
          card_body(
            p("This cohort consists of mechanically ventilated ICU patients from ",
              strong("MIMIC-IV v3.1"), " (Beth Israel Deaconess Medical Center, 2008–2019)."),
            tags$div(
              tags$strong("Inclusion criteria:"),
              tags$ul(
                tags$li("Age ≥ 18 years"),
                tags$li("Mechanical ventilation ≥ 24 hours"),
                tags$li("Baseline creatinine available"),
                tags$li("Adequate follow-up data")
              )
            ),
            tags$div(
              tags$strong("Exclusion criteria:"),
              tags$ul(
                tags$li("End-stage renal disease (ESRD) or chronic dialysis"),
                tags$li("Kidney transplant recipients"),
                tags$li("AKI Stage 2+ at intubation"),
                tags$li("Death within 48h of intubation"),
                tags$li("Elective surgical cases")
              )
            ),
            p("The primary aim is to ",
              strong("predict AKI Stage 2+ within 7 days of mechanical ventilation"),
              " using first-24h clinical, laboratory, and ventilator data."),
            tags$hr(),
            tags$small(class = "text-muted",
              "Data source: MIMIC-IV v3.1 | Study period: 2008–2019"
            )
          )
        )
      )
    ),

    # ── Tab 2: Demographics ────────────────────────────────────────────────────
    nav_panel(
      title = tagList(bs_icon("person-lines-fill"), " Demographics"),
      tagList(
        textOutput(ns("n_filtered")) |> tagAppendAttributes(class = "text-muted mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Age Distribution"),
               card_body(plotlyOutput(ns("age_plot"), height = "280px"))),
          card(card_header("Sex Distribution"),
               card_body(plotlyOutput(ns("sex_plot"), height = "280px"))),
          card(card_header("Race / Ethnicity"),
               card_body(plotlyOutput(ns("race_plot"), height = "280px"))),
          card(card_header("ICU Length of Stay"),
               card_body(plotlyOutput(ns("icu_los_plot"), height = "280px"))),
          card(card_header("Ventilation Duration"),
               card_body(plotlyOutput(ns("vent_plot"), height = "280px"))),
          card(card_header("Hospital Mortality"),
               card_body(plotlyOutput(ns("mort_plot"), height = "280px")))
        )
      )
    ),

    # Tab 3: AKI Staging 
    nav_panel(
      title = tagList(bs_icon("droplet-fill"), " AKI Staging"),
      tagList(
        card(
          class = "border-warning mb-3",
          card_body(
            class = "py-2",
            tags$div(
              class = "d-flex align-items-start gap-2",
              bs_icon("info-circle-fill", class = "text-warning mt-1", size = "1.1rem"),
              tags$div(
                tags$strong("Note: "),
                "The AKI Staging tab shows the AKI Stages of patients upon peri-intubation (12,799 ICU stays). The final study cohort (10,089 stays) contains only No AKI and Stage 1 patients at intubation. Of these, 10,085 have complete feature matrices available for ML modeling."
              )
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("AKI Stage Distribution (Pre-exclusion)"),
            card_body(plotlyOutput(ns("stage_bar"), height = "320px"))
          ),
          card(
            card_header("Creatinine Fold-Increase"),
            card_body(
              sliderInput(ns("fold_max"), "Max fold-increase shown:",
                          min = 2, max = 10, value = 5, step = 0.5, width = "100%"),
              plotlyOutput(ns("fold_hist"), height = "260px")
            )
          ),
          card(
            card_header("Baseline vs Peri-Intubation Creatinine"),
            card_body(
              checkboxGroupInput(ns("scatter_stages"), "Show stages:",
                                 choices  = c("No AKI", "Stage 1", "Stage 2", "Stage 3"),
                                 selected = c("No AKI", "Stage 1", "Stage 2", "Stage 3"),
                                 inline   = TRUE),
              plotlyOutput(ns("creat_scatter"), height = "280px")
            )
          ),
          card(
            card_header("Creatinine by Timepoint"),
            card_body(plotlyOutput(ns("creat_box"), height = "320px"))
          )
        )
      )
    ),

    # Tab 4: Lab Explorer 
    nav_panel(
      title = tagList(bs_icon("eyedropper"), " Lab Explorer"),
      tagList(
        uiOutput(ns("lab_load_ui")),
        uiOutput(ns("lab_explorer_ui"))
      )
    )
  )
}


cohortExplorerServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Reactive data (teal filter-aware) 
    filtered <- reactive(data()[["aki_cohort"]])
    aki_full <- reactive({
      data()[["aki_staging"]] %>%
        filter(!is.na(aki_stage)) %>%
        mutate(aki_stage = factor(aki_stage,
                                  levels = c("No AKI", "Stage 1", "Stage 2", "Stage 3")))
    })

    # Overview outputs 
    output$n_stays <- renderText(format(nrow(aki_cohort), big.mark = ","))
    output$n_patients <- renderText(format(n_distinct(aki_cohort$subject_id),
                                           big.mark = ","))
    output$mortality <- renderText(sprintf("%.1f%%",
                           mean(aki_cohort$hospital_expire_flag == 1, na.rm = TRUE) * 100))
    output$aki_rate <- renderText(sprintf("%.1f%%", AKI_PREVALENCE))
    output$icu_los <- renderText(sprintf("%.1f days",
                           median(aki_cohort$icu_los_days, na.rm = TRUE)))
    output$vent_dur <- renderText(sprintf("%.0f hrs",
                           median(aki_cohort$vent_duration_hours, na.rm = TRUE)))

    # Demographics outputs
    output$n_filtered <- renderText({
      sprintf("Showing %s ICU stays", format(nrow(filtered()), big.mark = ","))
    })

    output$age_plot <- renderPlotly({
      med_age <- median(filtered()$age, na.rm = TRUE)
      p <- ggplot(filtered(), aes(x = age)) +
        geom_histogram(bins = 40, fill = "#3498db", alpha = 0.85, color = "white") +
        geom_vline(xintercept = med_age,
                   color = "red", linetype = "dashed", linewidth = 0.8) +
        labs(x = "Age (years)", y = "Count") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      plt <- ggplotly(p) %>% plotly::layout(margin = list(l = 50))
      n <- length(plt$x$data)
      plotly::style(plt,
        hovertemplate = paste0("Median: ", round(med_age, 0), "<extra></extra>"),
        traces = n)
    })

    output$sex_plot <- renderPlotly({
      d <- filtered() %>%
        count(gender) %>%
        filter(!is.na(gender)) %>%
        mutate(
          label = case_when(gender == "F" ~ "Female", gender == "M" ~ "Male", TRUE ~ gender),
          pct = round(n / sum(n) * 100, 1)
        )
      plot_ly(d, labels = ~label, values = ~n,
              textinfo = "label+percent", hoverinfo = "label+value+percent",
              marker = list(colors = c("#e74c3c", "#3498db"),
                            line   = list(color = "white", width = 1))) %>%
        add_pie(hole = 0.4) %>%
        plotly::layout(showlegend = TRUE, margin = list(t = 10, b = 10, l = 10, r = 10))
    })

    output$race_plot <- renderPlotly({
      req("race" %in% names(filtered()))
      d <- filtered() %>%
        count(race) %>%
        filter(!is.na(race)) %>%
        mutate(pct  = round(n / sum(n) * 100, 1),
               race = fct_reorder(race, n))
      p <- ggplot(d, aes(x = race, y = n, fill = race,
                         text = paste0(race, ": ", format(n, big.mark = ","), " (", pct, "%)"))) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_fill_brewer(palette = "Set2") +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = "Count") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.y = element_blank())
      ggplotly(p, tooltip = "text") %>% plotly::layout(margin = list(l = 130))
    })

    output$icu_los_plot <- renderPlotly({
      med_los <- median(filtered()$icu_los_days, na.rm = TRUE)
      p <- ggplot(filtered() %>% filter(icu_los_days <= 30), aes(x = icu_los_days)) +
        geom_histogram(bins = 50, fill = "#9b59b6", alpha = 0.85, color = "white") +
        geom_vline(xintercept = med_los,
                   color = "red", linetype = "dashed", linewidth = 0.8) +
        labs(x = "Days (capped at 30)", y = "Count") +
        theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
      plt <- ggplotly(p) %>% plotly::layout(margin = list(l = 50))
      n <- length(plt$x$data)
      plotly::style(plt,
        hovertemplate = paste0("Median: ", round(med_los, 1), " days<extra></extra>"),
        traces = n)
    })

    output$vent_plot <- renderPlotly({
      med_vent <- median(filtered()$vent_duration_hours, na.rm = TRUE)
      p <- ggplot(filtered() %>% filter(vent_duration_hours <= 500),
                  aes(x = vent_duration_hours)) +
        geom_histogram(bins = 50, fill = "#2ecc71", alpha = 0.85, color = "white") +
        geom_vline(xintercept = med_vent,
                   color = "red", linetype = "dashed", linewidth = 0.8) +
        labs(x = "Hours (capped at 500)", y = "Count") +
        theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
      plt <- ggplotly(p) %>% plotly::layout(margin = list(l = 50))
      n <- length(plt$x$data)
      plotly::style(plt,
        hovertemplate = paste0("Median: ", round(med_vent, 1), " hrs<extra></extra>"),
        traces = n)
    })

    output$mort_plot <- renderPlotly({
      d <- filtered() %>%
        mutate(outcome = ifelse(hospital_expire_flag == 1, "Died", "Survived")) %>%
        count(outcome) %>%
        mutate(pct = round(n / sum(n) * 100, 1))
      p <- ggplot(d, aes(x = outcome, y = n, fill = outcome,
                         text = paste0(outcome, ": ", format(n, big.mark = ","),
                                       " (", pct, "%)"))) +
        geom_col(width = 0.6, show.legend = FALSE) +
        scale_fill_manual(values = c("Died" = "#e74c3c", "Survived" = "#2ecc71")) +
        scale_y_continuous(labels = comma) +
        labs(x = NULL, y = "Count") +
        theme_minimal(base_size = 11) + theme(panel.grid.major.x = element_blank())
      ggplotly(p, tooltip = "text") %>% plotly::layout(margin = list(b = 10))
    })

    # AKI Staging outputs
    output$stage_bar <- renderPlotly({
      d <- aki_full() %>% count(aki_stage) %>% mutate(pct = round(n / sum(n) * 100, 1))
      p <- ggplot(d, aes(x = aki_stage, y = n, fill = aki_stage,
                          text = paste0(aki_stage, "<br>n = ", format(n, big.mark = ","),
                                        "<br>", pct, "%"))) +
        geom_col(width = 0.65, show.legend = FALSE) +
        geom_text(aes(label = paste0(format(n, big.mark = ","), "\n(", pct, "%)")),
                  vjust = -0.3, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = aki_colors) +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
        labs(x = "AKI Stage", y = "ICU Stays") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
      ggplotly(p, tooltip = "text") %>% plotly::layout(margin = list(b = 10))
    })

    output$fold_hist <- renderPlotly({
      req(input$fold_max)
      d <- aki_full() %>%
        filter(!is.na(creat_fold_increase), creat_fold_increase <= input$fold_max)
      p <- ggplot(d, aes(x = creat_fold_increase, fill = aki_stage)) +
        geom_histogram(bins = 60, color = "white", alpha = 0.85) +
        geom_vline(xintercept = c(1.5, 2.0, 3.0),
                   linetype = "dashed",
                   color    = c("#f39c12", "#e67e22", "#e74c3c"),
                   linewidth = 0.8) +
        scale_fill_manual(values = aki_colors) +
        scale_x_continuous(breaks = seq(0, input$fold_max, 0.5)) +
        labs(x = "Fold Increase (peri-intubation / baseline)",
             y = "Count", fill = "AKI Stage") +
        theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
      ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "v", x = 1.02, y = 0.5),
               margin = list(r = 130, l = 50, b = 40))
    })

    output$creat_scatter <- renderPlotly({
      req(input$scatter_stages)
      d <- aki_full() %>%
        filter(aki_stage %in% input$scatter_stages,
               !is.na(baseline_creat), !is.na(max_creat_at_intubation),
               baseline_creat <= 5, max_creat_at_intubation <= 10)
      p <- ggplot(d, aes(x = baseline_creat, y = max_creat_at_intubation,
                          color = aki_stage,
                          text = paste0("AKI: ", aki_stage,
                                         "<br>Baseline: ", round(baseline_creat, 2),
                                         "<br>Peri-intubation: ",
                                         round(max_creat_at_intubation, 2)))) +
        geom_abline(slope = c(1, 1.5, 2.0, 3.0), intercept = 0,
                    linetype = c("solid", "dashed", "dashed", "dashed"),
                    color = c("gray60", "#f39c12", "#e67e22", "#e74c3c"),
                    linewidth = 0.7) +
        geom_point(alpha = 0.4, size = 1.5) +
        scale_color_manual(values = aki_colors) +
        labs(x = "Baseline Cr (mg/dL)", y = "Max Peri-intubation Cr (mg/dL)",
             color = "AKI Stage") +
        theme_minimal(base_size = 11)
      ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "v", x = 1.02, y = 0.5),
               margin = list(r = 130, l = 50, b = 40))
    })

    output$creat_box <- renderPlotly({
      d <- aki_full() %>%
        filter(!is.na(baseline_creat), !is.na(max_creat_at_intubation)) %>%
        select(aki_stage, baseline_creat, max_creat_at_intubation) %>%
        pivot_longer(cols = c(baseline_creat, max_creat_at_intubation),
                     names_to = "timepoint", values_to = "creatinine") %>%
        mutate(
          timepoint = recode(timepoint,
            baseline_creat = "Baseline (7–365d before)",
            max_creat_at_intubation = "Peri-intubation (−48h to +6h)"
          ),
          creatinine = pmin(creatinine, 6)
        )
      p <- ggplot(d, aes(x = timepoint, y = creatinine, fill = timepoint)) +
        geom_boxplot(outlier.alpha = 0.2, color = "black", linewidth = 0.3) +
        geom_hline(yintercept = 1.2, linetype = "dashed",
                   color = "red", linewidth = 0.8) +
        scale_fill_manual(values = c(
          "Baseline (7–365d before)"      = "#3498db",
          "Peri-intubation (−48h to +6h)" = "#e67e22"
        )) +
        scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
        annotate("text", x = 1.5, y = 1.35, label = "Normal upper limit (1.2 mg/dL)",
                 color = "red", size = 3, fontface = "italic") +
        labs(x = NULL, y = "Creatinine (mg/dL, capped at 6)") +
        theme_minimal(base_size = 11) + theme(legend.position = "none")
      ggplotly(p) %>% plotly::layout(margin = list(b = 10, l = 50))
    })

    # Lab Explorer 
    lab_data <- reactiveVal(NULL)
    loading <- reactiveVal(FALSE)

    output$lab_load_ui <- renderUI({
      if (!is.null(lab_data())) return(NULL)
      ns <- session$ns
      card(
        card_body(
          class = "text-center py-5",
          h4("Lab Explorer uses the MICE-imputed feature matrix"),
          p(class = "text-muted",
            "File: feature_matrix_imputed_stacked.rds (5 imputations × 10,085 stays)",
            br(), "Displays imputation 1 of 5 as a representative dataset."),
          if (loading()) {
            tagList(
              tags$div(class = "spinner-border text-primary", role = "status"),
              p("Loading imputed data, please wait...")
            )
          } else {
            actionButton(ns("load_labs"), "Load Imputed Lab Data",
                         class = "btn-primary btn-lg")
          }
        )
      )
    })

    observeEvent(input$load_labs, {
      loading(TRUE)
      withProgress(message = "Loading imputed feature matrix...", value = 0.1, {
        feat_imp <- readRDS(FEAT_IMPUTED_PATH)
        incProgress(0.5, detail = "Filtering to imputation 1...")
        labs_wide <- feat_imp %>%
          filter(.imp == 1) %>%
          select(-any_of("aki_stage")) %>%
          left_join(aki_cohort %>% select(stay_id, aki_stage), by = "stay_id")
        incProgress(0.4, detail = "Done.")
        lab_data(labs_wide)
      })
      loading(FALSE)
    })

    output$lab_explorer_ui <- renderUI({
      if (is.null(lab_data())) return(NULL)
      ns <- session$ns
      avail_labs <- LAB_CATEGORIES[paste0(LAB_CATEGORIES, "_mean") %in% names(lab_data())]
      tagList(
        card(
          class = "border-info mb-3",
          card_body(
            class = "py-2",
            tags$div(
              class = "d-flex align-items-start gap-2",
              bs_icon("info-circle-fill", class = "text-info mt-1", size = "1.1rem"),
              tags$div(
                tags$strong("Showing Imputation 1 of 5. "),
                "Lab values are drawn from the first MICE imputation of ",
                tags$code("feature_matrix_imputed_stacked.rds"),
                " (5 imputations \u00d7 10,085 stays). ",
                "All 5 imputations were used for model training; imputation 1 is shown here as a representative dataset."
              )
            )
          )
        ),
      layout_sidebar(
        sidebar = sidebar(
          title = "Lab Controls",
          width = 260,
          selectInput(ns("lab_cat"), "Lab Category",
                      choices  = sort(avail_labs),
                      selected = if ("creatinine" %in% avail_labs) "creatinine" else avail_labs[1]),
          selectInput(ns("lab_stat"), "Statistic",
                      choices  = c("Mean" = "mean", "Max" = "max", "Min" = "min"),
                      selected = "mean"),
          hr(),
          checkboxGroupInput(ns("aki_filter"), "AKI Stage at Intubation",
                             choices  = c("No AKI", "Stage 1"),
                             selected = c("No AKI", "Stage 1"))
        ),
        tagList(
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header(textOutput(ns("lab_plot_title"))),
              card_body(plotlyOutput(ns("lab_dist"), height = "350px"))
            ),
            card(
              card_header("Summary Statistics"),
              card_body(DTOutput(ns("lab_summary")))
            )
          ),
          card(
            card_header("Distribution by AKI Stage"),
            card_body(plotlyOutput(ns("lab_by_aki"), height = "320px"))
          )
        )
      )
      ) # close tagList
    })

    filtered_labs <- reactive({
      req(lab_data(), input$lab_cat, input$lab_stat, input$aki_filter)
      col_name <- paste0(input$lab_cat, "_", input$lab_stat)
      req(col_name %in% names(lab_data()))
      lab_data() %>%
        filter(aki_stage %in% input$aki_filter) %>%
        select(stay_id, aki_stage, valuenum = .data[[col_name]]) %>%
        filter(!is.na(valuenum))
    })

    output$lab_plot_title <- renderText({
      req(input$lab_cat, input$lab_stat)
      paste0("Distribution: ", toupper(input$lab_cat), " (", input$lab_stat, ")")
    })

    output$lab_dist <- renderPlotly({
      req(filtered_labs())
      d <- filtered_labs()
      if (nrow(d) == 0) return(plot_ly() %>% add_annotations(text = "No data"))
      p <- ggplot(d, aes(x = valuenum)) +
        geom_histogram(bins = 60, fill = "#7ecbf5", alpha = 0.85, color = "white") +
        labs(x = paste0(toupper(input$lab_cat), " (", input$lab_stat, ")"), y = "Count") +
        theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
      ggplotly(p) %>% plotly::layout(margin = list(l = 50))
    })

    output$lab_summary <- renderDT({
      req(filtered_labs())
      d <- filtered_labs()
      if (nrow(d) == 0) return(datatable(data.frame(Message = "No data")))
      tibble(
        Statistic = c("N stays", "Mean", "Median",
                      "SD", "IQR (25–75%)", "Min", "Max"),
        Value = c(
          format(nrow(d), big.mark = ","),
          round(mean(d$valuenum, na.rm = TRUE), 3),
          round(median(d$valuenum, na.rm = TRUE), 3),
          round(sd(d$valuenum, na.rm = TRUE), 3),
          paste0(round(quantile(d$valuenum, 0.25, na.rm = TRUE), 3), " – ",
                 round(quantile(d$valuenum, 0.75, na.rm = TRUE), 3)),
          round(min(d$valuenum, na.rm = TRUE), 3),
          round(max(d$valuenum, na.rm = TRUE), 3)
        )
      ) %>%
        datatable(rownames = FALSE, options = list(dom = "t", pageLength = 10),
                  class = "compact stripe")
    })

    output$lab_by_aki <- renderPlotly({
      req(filtered_labs())
      d <- filtered_labs() %>%
        filter(!is.na(aki_stage)) %>%
        mutate(aki_stage = factor(aki_stage, levels = c("No AKI", "Stage 1", "Stage 2", "Stage 3")))
      if (nrow(d) == 0) return(plot_ly() %>% add_annotations(text = "No data"))
      p <- ggplot(d, aes(x = aki_stage, y = valuenum, fill = aki_stage)) +
        geom_boxplot(outlier.alpha = 0.2, notch = FALSE, linewidth = 0.3) +
        scale_fill_manual(values = aki_colors) +
        labs(x = "AKI Stage at Intubation",
             y = paste0(toupper(input$lab_cat), " (", input$lab_stat, ")")) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "none", panel.grid.major.x = element_blank())
      ggplotly(p) %>% plotly::layout(margin = list(l = 50, b = 50))
    })
  })
}

cohort_explorer_teal_module <- function(label = "Cohort Explorer") {
  teal::module(
    label     = label,
    ui        = cohortExplorerUI,
    server    = function(id, data) { cohortExplorerServer(id, data) },
    datanames = c("aki_cohort", "aki_staging")
  )
}
