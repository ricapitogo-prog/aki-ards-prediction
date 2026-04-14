# mod_patient_trajectory.R — Patient Trajectory
#   - Population Trends: smoothed lab trajectories by AKI outcome group
#   - Patient Drill-Down: individual creatinine + lab trajectory for one patient

patientTrajectoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("traj_load_ui")),
    uiOutput(ns("traj_content_ui"))
  )
}

patientTrajectoryServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    traj_labs <- reactiveVal(NULL)
    traj_loading <- reactiveVal(FALSE)

    # Load button 
    output$traj_load_ui <- renderUI({
      if (!is.null(traj_labs())) return(NULL)
      ns <- session$ns
      card(
        card_body(
          class = "text-center py-5",
          bs_icon("clock-history", size = "3rem", class = "text-muted mb-3"),
          h4("Patient Trajectory requires loading the full lab dataset"),
          p(class = "text-muted",
            "File: aki_labs_long.rds (~250 MB, 61.5 million measurements)",
            br(), "Loading may take 15–30 seconds."),
          if (traj_loading()) {
            tagList(
              tags$div(class = "spinner-border text-primary", role = "status"),
              p("Loading, please wait...")
            )
          } else {
            actionButton(ns("load_traj"), "Load Trajectory Data",
                         class = "btn-primary btn-lg")
          }
        )
      )
    })

    observeEvent(input$load_traj, {
      traj_loading(TRUE)
      withProgress(message = "Loading lab data (250 MB)...", value = 0.1, {
        labs_raw <- readRDS(LAB_DATA_PATH)
        incProgress(0.4, detail = "Joining with cohort...")
        # Join with cohort to get aki_stage and vent_start
        cohort_info <- aki_cohort %>%
          select(subject_id, hadm_id, stay_id, vent_start, aki_stage,
                 age, gender, hospital_expire_flag, vent_duration_hours,
                 baseline_creat)
        labs_joined <- labs_raw %>%
          inner_join(cohort_info, by = "hadm_id") %>%
          mutate(
            hours_from_vent = as.numeric(
              difftime(charttime, vent_start, units = "hours")
            )
          )
        incProgress(0.4, detail = "Done.")
        traj_labs(labs_joined)
      })
      traj_loading(FALSE)
    })

    # Main content (shown once loaded)
    output$traj_content_ui <- renderUI({
      if (is.null(traj_labs())) return(NULL)
      ns <- session$ns
      tagList(
        card(
          class = "border-info mb-3",
          card_body(
            class = "py-2",
            tags$div(
              class = "d-flex align-items-start gap-2",
              bs_icon("info-circle-fill", class = "text-info mt-1", size = "1.1rem"),
              tags$div(
                tags$strong("Lab categories shown are based on Imputation 1 of 5. "),
                "Lab choices are restricted to those that survived the MICE >50% missingness filter. ",
                "Trajectory measurements are raw MIMIC-IV observations — MICE imputes 24h summary ",
                "statistics only, not individual timestamped values."
              )
            )
          )
        ),
      navset_card_tab(
        id = ns("traj_tabs"),

        # Population Trends
        nav_panel(
          title = tagList(bs_icon("graph-up-arrow"), " Population Trends"),
          layout_sidebar(
            sidebar = sidebar(
              title = "Controls",
              width = 260,
              selectInput(ns("pop_lab"), "Lab Category",
                          choices = sort(IMPUTED_LAB_CATS),
                          selected = "creatinine"),
              sliderInput(ns("pop_window"), "Hours from Intubation",
                          min = -48, max = 168, value = c(-12, 72), step = 6),
              checkboxGroupInput(ns("pop_stages"), "AKI Stage at Intubation",
                                 choices = c("No AKI", "Stage 1"),
                                 selected = c("No AKI", "Stage 1")),
              hr(),
              helpText("Shaded ribbon = IQR (25th–75th percentile).",
                       br(), "Line = median per 4-hour bin.")
            ),
            tagList(
              card(
                card_header(textOutput(ns("pop_title"))),
                card_body(plotlyOutput(ns("pop_trend"), height = "420px"))
              ),
              card(
                card_header("Summary: Median value by group (window shown)"),
                card_body(DTOutput(ns("pop_summary_tbl")))
              )
            )
          )
        ),

        # Patient Drill-Down
        nav_panel(
          title = tagList(bs_icon("person-badge"), " Patient Drill-Down"),
          layout_sidebar(
            sidebar = sidebar(
              title = "Patient Selection",
              width = 280,
              selectInput(ns("pt_stay_id"), "Select Stay ID",
                          choices = sort(unique(traj_labs()$stay_id)),
                          selected = sort(unique(traj_labs()$stay_id))[1]),
              hr(),
              uiOutput(ns("pt_meta_ui")),
              hr(),
              selectizeInput(ns("pt_extra_labs"), "Additional Labs to Overlay",
                             choices = sort(IMPUTED_LAB_CATS),
                             selected = NULL,
                             multiple = TRUE,
                             options = list(maxItems = 3L)),
              helpText("Select up to 3 additional labs to plot alongside creatinine.")
            ),
            tagList(
              uiOutput(ns("pt_info_card")),
              card(
                card_header("Creatinine Trajectory"),
                card_body(plotlyOutput(ns("pt_creat_plot"), height = "380px"))
              ),
              uiOutput(ns("pt_extra_plot_ui"))
            )
          )
        )
      )
      ) # close tagList
    })

    # Population Trends: reactive data
    pop_data <- reactive({
      req(traj_labs(), input$pop_lab, input$pop_stages, input$pop_window)
      d <- traj_labs() %>%
        filter(
          lab_category == input$pop_lab,
          aki_stage %in% input$pop_stages,
          !is.na(valuenum),
          !is.na(hours_from_vent),
          hours_from_vent >= input$pop_window[1],
          hours_from_vent <= input$pop_window[2]
        )
      d
    })

    output$pop_title <- renderText({
      req(input$pop_lab)
      paste(toupper(input$pop_lab), "Trajectory by AKI Stage at Intubation")
    })

    output$pop_trend <- renderPlotly({
      req(pop_data())
      d <- pop_data()
      if (nrow(d) < 10) return(plot_ly() %>% add_annotations(text = "Not enough data"))

      # Bin into 4-hour windows, compute median + IQR
      binned <- d %>%
        mutate(hour_bin = floor(hours_from_vent / 4) * 4) %>%
        group_by(aki_stage, hour_bin) %>%
        summarise(
          med = median(valuenum, na.rm = TRUE),
          q25 = quantile(valuenum, 0.25, na.rm = TRUE),
          q75 = quantile(valuenum, 0.75, na.rm = TRUE),
          n_pts = n_distinct(hadm_id),
          .groups = "drop"
        ) %>%
        filter(n_pts >= 5)  # require ≥5 patients per bin

      stage_colors <- c("No AKI" = "#2ecc71", "Stage 1" = "#f39c12")

      p <- ggplot(binned, aes(x = hour_bin, color = aki_stage, fill = aki_stage)) +
        geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, color = NA) +
        geom_line(aes(y = med), linewidth = 1.1) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
        annotate("text", x = 1, y = Inf, label = "Intubation",
                 hjust = 0, vjust = 1.5, size = 3.2, color = "gray40") +
        scale_color_manual(values = stage_colors, name = "AKI Stage at Intubation") +
        scale_fill_manual(values  = stage_colors, name = "AKI Stage at Intubation") +
        labs(x = "Hours from Intubation", y = toupper(input$pop_lab)) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
      ggplotly(p) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.15),
               margin = list(l = 50, b = 60))
    })

    output$pop_summary_tbl <- renderDT({
      req(pop_data())
      pop_data() %>%
        group_by(`AKI Stage` = aki_stage) %>%
        summarise(
          `N measurements` = format(n(), big.mark = ","),
          `N patients` = format(n_distinct(hadm_id), big.mark = ","),
          `Median` = round(median(valuenum, na.rm = TRUE), 3),
          `IQR` = paste0(
            round(quantile(valuenum, 0.25, na.rm = TRUE), 3), " – ",
            round(quantile(valuenum, 0.75, na.rm = TRUE), 3)
          ),
          .groups = "drop"
        ) %>%
        datatable(rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    })

    # Patient Drill-Down: reactive 
    pt_stay <- reactiveVal(NULL)

    observeEvent(input$pt_stay_id, {
      req(input$pt_stay_id)
      pt_stay(as.integer(input$pt_stay_id))
    })

    pt_cohort_info <- reactive({
      req(pt_stay())
      aki_cohort %>% filter(stay_id == pt_stay()) %>% slice(1)
    })

    output$pt_meta_ui <- renderUI({
      req(pt_cohort_info())
      info <- pt_cohort_info()
      tags$div(
        class = "small text-muted lh-sm",
        tags$span(paste0("Stay ID: ", info$stay_id)), br(),
        tags$span(paste0("Subject ID: ", info$subject_id)), br(),
        tags$span(paste0("Age: ", round(info$age, 0))), br(),
        tags$span(paste0("Sex: ", ifelse(info$gender == "F", "Female", "Male"))), br(),
        tags$span(paste0("Race: ", info$race))
      )
    })

    output$pt_info_card <- renderUI({
      if (is.null(pt_stay())) {
        return(card(card_body(class = "text-center text-muted py-4",
                              "Select a Stay ID from the dropdown to view patient trajectory.")))
      }
      info <- pt_cohort_info()
      card(
        card_header("Patient Summary"),
        card_body(
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            tags$div(
              tags$small("Age", class = "text-muted d-block"),
              tags$strong(paste0(round(info$age, 0), " years"))
            ),
            tags$div(
              tags$small("Sex", class = "text-muted d-block"),
              tags$strong(ifelse(info$gender == "F", "Female", "Male"))
            ),
            tags$div(
              tags$small("AKI Stage at Intubation", class = "text-muted d-block"),
              tags$strong(info$aki_stage)
            ),
            tags$div(
              tags$small("Hospital Outcome", class = "text-muted d-block"),
              if (info$hospital_expire_flag == 1)
                tags$strong(tags$span(class = "text-danger", "Died"))
              else
                tags$strong(tags$span(class = "text-success", "Survived"))
            )
          )
        )
      )
    })

    # Creatinine trajectory for the selected patient
    pt_creat_data <- reactive({
      req(pt_stay(), traj_labs())
      traj_labs() %>%
        filter(stay_id == pt_stay(), lab_category == "creatinine", !is.na(valuenum)) %>%
        arrange(charttime)
    })

    output$pt_creat_plot <- renderPlotly({
      if (is.null(pt_stay())) return(plot_ly())
      d <- pt_creat_data()
      info <- pt_cohort_info()

      if (nrow(d) == 0) {
        return(plot_ly() %>%
                 add_annotations(text = "No creatinine data for this patient."))
      }

      # AKI threshold lines based on baseline creatinine
      base_cr <- info$baseline_creat
      thresholds <- if (!is.na(base_cr)) {
        list(
          stage1 = base_cr * 1.5,
          stage2 = base_cr * 2.0,
          stage3 = base_cr * 3.0
        )
      } else NULL

      p <- ggplot(d, aes(x = hours_from_vent, y = valuenum)) +
        geom_line(color = "#3498db", linewidth = 1.1) +
        geom_point(color = "#3498db", size = 2.5, alpha = 0.7) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = "gray40", linewidth = 0.8)

      if (!is.null(thresholds)) {
        p <- p +
          geom_hline(yintercept = base_cr, color = "#2ecc71",
                     linetype = "dashed", linewidth = 0.8) +
          geom_hline(yintercept = thresholds$stage1, color = "#f39c12",
                     linetype = "dashed", linewidth = 0.8) +
          geom_hline(yintercept = thresholds$stage2, color = "#e67e22",
                     linetype = "dashed", linewidth = 0.8) +
          geom_hline(yintercept = thresholds$stage3, color = "#e74c3c",
                     linetype = "dashed", linewidth = 0.8) +
          annotate("text", x = min(d$hours_from_vent), y = base_cr * 1.03,
                   label = "Baseline", color = "#2ecc71", size = 3, hjust = 0) +
          annotate("text", x = min(d$hours_from_vent), y = thresholds$stage1 * 1.03,
                   label = "Stage 1 (×1.5)", color = "#f39c12", size = 3, hjust = 0) +
          annotate("text", x = min(d$hours_from_vent), y = thresholds$stage2 * 1.03,
                   label = "Stage 2 (×2.0)", color = "#e67e22", size = 3, hjust = 0) +
          annotate("text", x = min(d$hours_from_vent), y = thresholds$stage3 * 1.03,
                   label = "Stage 3 (×3.0)", color = "#e74c3c", size = 3, hjust = 0)
      }

      p <- p +
        labs(x = "Hours from Intubation", y = "Creatinine (mg/dL)",
             title = paste0("Stay ", pt_stay(), ": Creatinine Trajectory")) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank())
      ggplotly(p) %>% plotly::layout(margin = list(l = 55, b = 50))
    })

    # Additional labs overlay
    output$pt_extra_plot_ui <- renderUI({
      if (is.null(pt_stay()) || is.null(input$pt_extra_labs) || length(input$pt_extra_labs) == 0)
        return(NULL)
      ns <- session$ns
      card(
        card_header("Additional Lab Trajectories"),
        card_body(plotlyOutput(ns("pt_extra_plot"), height = "300px"))
      )
    })

    output$pt_extra_plot <- renderPlotly({
      req(pt_stay(), input$pt_extra_labs)
      labs_sel <- head(input$pt_extra_labs, 3)  # max 3
      d <- traj_labs() %>%
        filter(stay_id == pt_stay(),
               lab_category %in% labs_sel,
               !is.na(valuenum)) %>%
        arrange(charttime)
      if (nrow(d) == 0)
        return(plot_ly() %>% add_annotations(text = "No data for selected labs."))

      colors_extra <- c("#e74c3c", "#9b59b6", "#f39c12")
      p <- ggplot(d, aes(x = hours_from_vent, y = valuenum,
                         color = lab_category, group = lab_category)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2, alpha = 0.7) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = "gray40", linewidth = 0.7) +
        scale_color_manual(values = setNames(colors_extra[seq_along(labs_sel)], labs_sel)) +
        labs(x = "Hours from Intubation", y = "Value", color = "Lab") +
        theme_minimal(base_size = 11) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank())
      ggplotly(p) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2),
               margin = list(l = 55, b = 60))
    })
  })
}

patient_trajectory_teal_module <- function(label = "Patient Trajectory") {
  teal::module(
    label = label,
    ui = patientTrajectoryUI,
    server = function(id, data) { patientTrajectoryServer(id, data) },
    datanames = "aki_cohort"
  )
}
