# R/modules/mod_avm.R

# =======================================================#
####  1. UI Definition (Actual vs Model Module)       ####
# =======================================================#

mod_avm_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- Row 1: Configuration Cards ----
    fluidRow(
      # Card 1: File Upload
      column(width = 4,
             div(class = "section-card",
                 div(class = "card-title", "Upload Data"),
                 fileInput(ns('file1'), label = 'Choose .zip file from AWB', 
                           multiple = FALSE, accept = c('application/zip', '.zip'))
             )
      ),
      # Card 2: Model Configuration
      column(width = 4,
             div(class = "section-card",
                 div(class = "card-title", "Model & Window"),
                 
                 # --- CAMBIO 1: Radio Buttons para KPI Transformation ---
                 radioButtons(ns("kpi_transform"), label = "KPI Transformation",
                              choices = c("Log", "Linear"), 
                              selected = "Log", 
                              inline = TRUE),
                 
                 # Radio Buttons + Date Range Condicional
                 div(style = "margin-top: 15px;",
                     radioButtons(ns("report_type"), label = "Report Window",
                                  choices = c("Last 52 Weeks" = "L52", 
                                              "Last 13 Weeks" = "L13", 
                                              "Custom" = "Custom"),
                                  selected = "L52",
                                  inline = TRUE),
                     
                     conditionalPanel(
                       condition = "input.report_type == 'Custom'",
                       ns = ns,
                       div(style = "margin-top: 10px;",
                           dateRangeInput(ns('reporting_window'), label = NULL,
                                          start = NULL, end = NULL, separator = " to ", width = "100%")
                       )
                     )
                 )
             )
      ),
      # Card 3: Dimension Filters
      column(width = 4,
             div(class = "section-card",
                 div(class = "card-title", "Dimensions"),
                 selectizeInput(ns('DimensionFilter'), label = 'Cross-sectional Dimension',
                                choices = c("Geography", "Product", "Campaign", "Outlet", "Creative"), 
                                multiple = TRUE, selected = "Geography", width = "100%"),
                 uiOutput(ns("dynamic_filters_container")),
                 
                 div(style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #eee;",
                     checkboxInput(ns("show_entity_metrics"), "Metrics by Entity", value = FALSE)
                 )
             )
      )
    ),
    
    # ---- Row 2: Main Chart (Actual vs Model) & Download ----
    fluidRow(
      column(width = 12,
             div(class = "section-card",
                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                     div(class = "card-title", style = "margin-bottom: 0; border-bottom: none;", "Actual vs Model"),
                     downloadButton(ns("download_data"), "Download Data", class = "btn-primary btn-sm")
                 ),
                 div(style = "border-bottom: 2px solid #3498db; margin-bottom: 20px;"),
                 
                 conditionalPanel(
                   condition = "input.show_entity_metrics == true",
                   ns = ns,
                   div(style = "margin-bottom: 20px;",
                       DT::DTOutput(ns('entity_table'))
                   ),
                   div(style = "border-bottom: 1px solid #eee; margin-bottom: 20px;")
                 ),
                 
                 plotlyOutput(ns('AVM'), height = "400px"),
                 uiOutput(ns('metrics_ui'))
             )
      )
    ),
    
    # ---- Row 3: Residuals Analysis ----
    fluidRow(
      column(width = 12,
             div(class = "section-card",
                 div(class = "card-title", "Residuals Performance"),
                 plotlyOutput(ns('residuals'), height = "350px")
             )
      )
    ),
    
    # ---- Row 4: Seasonality Analysis ----
    fluidRow(
      column(width = 12,
             div(class = "section-card",
                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                     div(class = "card-title", style = "margin-bottom: 0; border-bottom: none;", "Residuals Seasonality"),
                     checkboxInput(ns("show_trend"), "Show Average Trend", value = FALSE)
                 ),
                 div(style = "border-bottom: 2px solid #3498db; margin-bottom: 20px;"),
                 
                 plotlyOutput(ns('residual_seasonality'), height = "350px")
             )
      )
    )
  )
}

# ===============================================#
#### 2. Server Logic (Actual vs Model Module) ####
# ===============================================#

mod_avm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##### 2.1. Data Ingestion #####
    # ============================#

    # Single reactive that unzips and reads both files at once (unique folder
    # per upload), so there is no dependency on evaluation order.
    awb_files <- reactive({
      req(input$file1)
      tryCatch(
        read_awb_zip(input$file1$datapath),
        error = function(e) validate(need(FALSE, conditionMessage(e)))
      )
    })

    data_upload <- reactive(awb_files()$avm)
    var_stats_upload <- reactive(awb_files()$var_stats)

    observe({
      req(data_upload(), input$report_type)
      df <- data_upload()

      max_date <- max(df$Period, na.rm = TRUE)
      min_date <- min(df$Period, na.rm = TRUE)

      if (input$report_type %in% c("L52", "L13")) {
        # Both ends are inclusive: the last N weeks start (N - 1) weeks before
        # the last period (52 weeks -> 51 * 7 days back).
        n_weeks <- if (input$report_type == "L52") 52 else 13
        start_date <- max(min_date, max_date - (n_weeks - 1) * 7)

        updateDateRangeInput(session, 'reporting_window',
                             start = start_date, end = max_date,
                             min = min_date, max = max_date)
      } else {
        if (is.null(input$reporting_window[1])) {
          updateDateRangeInput(session, 'reporting_window',
                               start = min_date, end = max_date,
                               min = min_date, max = max_date)
        } else {
          updateDateRangeInput(session, 'reporting_window', min = min_date, max = max_date)
        }
      }
    })

    ##### 2.2. Dynamic UI Rendering #####
    #===================================#

    output$dynamic_filters_container <- renderUI({
      req(data_upload())
      data <- data_upload()
      ui_elems <- list()
      if("Geography" %in% input$DimensionFilter) ui_elems[[1]] <- div(style="margin-top:10px", selectInput(ns('Geo'), 'Geography', choices = c("Total", unique(data$Geography)), selected = "Total", width = "100%"))
      if("Product" %in% input$DimensionFilter) ui_elems[[length(ui_elems)+1]] <- div(style="margin-top:10px", selectInput(ns('SelectProd'), 'Product', choices = c("Total", unique(data$Product)), selected = "Total", width = "100%"))
      if("Campaign" %in% input$DimensionFilter) ui_elems[[length(ui_elems)+1]] <- div(style="margin-top:10px", selectInput(ns('SelectCamp'), 'Campaign', choices = c("Total", unique(data$Campaign)), selected = "Total", width = "100%"))
      if("Outlet" %in% input$DimensionFilter) ui_elems[[length(ui_elems)+1]] <- div(style="margin-top:10px", selectInput(ns('SelectOutlet'), 'Outlet', choices = c("Total", unique(data$Outlet)), selected = "Total", width = "100%"))
      if("Creative" %in% input$DimensionFilter) ui_elems[[length(ui_elems)+1]] <- div(style="margin-top:10px", selectInput(ns('SelectCreat'), 'Creative', choices = c("Total", unique(data$Creative)), selected = "Total", width = "100%"))
      do.call(tagList, ui_elems)
    })

    ##### 2.3. Helpers & Cleaning #####
    #=================================#

    kpi_name_clean <- reactive({
      req(var_stats_upload())
      vstats <- var_stats_upload()
      raw_name <- vstats %>% filter(DepVar == TRUE) %>% slice(1) %>% pull(VariableName)
      if(length(raw_name) == 0) return("KPI")
      raw_name %>% str_replace_all("[-_\\.]", " ") %>% str_remove_all("(?i)(total\\s+)+total") %>% str_remove_all("(?i)^(log|ln)\\s+") %>% str_remove_all("(?i)\\s+(log|ln)$") %>% str_squish() %>% str_to_title()
    })

    # Normalisation of the dependent variable (raw value from Variables Statistics)
    dep_norm <- reactive({
      req(var_stats_upload())
      var_stats_upload() %>% filter(DepVar == TRUE) %>% pull(Normalisation) %>% head(1)
    })

    # Number of model parameters for the Adjusted R2
    k_params <- reactive({
      req(var_stats_upload())
      n_model_params(var_stats_upload())
    })

    ##### 2.4. Data Processing (Calc) #####
    #=====================================#

    # Transformed data for ALL entities. Computed once per upload/transformation
    # and shared by the charts, the entity table and the download.
    transformed_data <- reactive({
      req(data_upload(), input$kpi_transform)
      norm <- dep_norm()
      transform_kpi(data_upload(), input$kpi_transform, if (length(norm) == 0) NA else norm)
    })

    # Transformed data filtered by the selected dimension values
    newdata_calc <- reactive({
      data <- transformed_data()

      if (!is.null(input$Geo) && input$Geo != 'Total') data <- filter(data, Geography == input$Geo)
      if (!is.null(input$SelectProd) && input$SelectProd != 'Total') data <- filter(data, Product == input$SelectProd)
      if (!is.null(input$SelectCamp) && input$SelectCamp != 'Total') data <- filter(data, Campaign == input$SelectCamp)
      if (!is.null(input$SelectOutlet) && input$SelectOutlet != 'Total') data <- filter(data, Outlet == input$SelectOutlet)
      if (!is.null(input$SelectCreat) && input$SelectCreat != 'Total') data <- filter(data, Creative == input$SelectCreat)

      data
    })

    # Aggregated by period (Actual, Fitted, Residual). Single source for all
    # charts and screen metrics, so all of them use the same observations.
    agg_data <- reactive({
      aggregate_avm(newdata_calc())
    })

    ##### 2.5. Metrics Calculation #####
    #==================================#

    model_metrics <- reactive({
      req(agg_data(), input$reporting_window)
      graph_data <- agg_data()

      m_full <- fit_metrics(graph_data$Actual, graph_data$Fitted, k_params())

      graph_data_win <- graph_data %>%
        filter(Period >= input$reporting_window[1] & Period <= input$reporting_window[2])
      # Only R2 and MAPE are reported for the window (Adj R2 is not meaningful here)
      m_win <- fit_metrics(graph_data_win$Actual, graph_data_win$Fitted, k_params())

      norm_val <- dep_norm()
      norm_label <- "None"
      if(length(norm_val) > 0 && !is.na(norm_val)) {
        if(norm_val == "AROUND 0") { norm_label <- "SUB" } else if(norm_val == "AROUND 1") { norm_label <- "DIV" }
      }

      list(
        R2       = fmt_pct(m_full$R2),
        R2_Adj   = fmt_pct(m_full$R2_Adj),
        MAPE     = fmt_pct(m_full$MAPE / 100),
        NObs     = scales::comma(m_full$N),
        R2_Win   = fmt_pct(m_win$R2),
        MAPE_Win = fmt_pct(m_win$MAPE / 100),
        NObs_Win = scales::comma(m_win$N),
        Norm     = norm_label
      )
    })

    output$metrics_ui <- renderUI({
      req(model_metrics())
      m <- model_metrics()
      tagList(
        tags$hr(class = "card-divider"),
        div(style = "display: flex; justify-content: space-between;",
            div(style = "width: 48%;",
                div(class = "metrics-title", "Global Performance (Full History)"),
                div(class = "metrics-container",
                    div(class = "metric-item", div(class = "metric-value", m$R2), div(class = "metric-label", "R²")),
                    div(class = "metric-item", div(class = "metric-value", m$R2_Adj), div(class = "metric-label", "Adj R²")),
                    div(class = "metric-item", div(class = "metric-value", m$MAPE), div(class = "metric-label", "MAPE")),
                    div(class = "metric-item", div(class = "metric-value", m$NObs), div(class = "metric-label", "Observations"))
                )
            ),
            div(style = "border-left: 1px solid #e0e0e0; margin: 0 10px;"),
            div(style = "width: 48%;",
                div(class = "metrics-title", style = "border-left: 3px solid #27ae60; color: #27ae60;", "Reporting Window"),
                div(class = "metrics-container",
                    div(class = "metric-item", div(class = "metric-value", style="color: #27ae60;", m$R2_Win), div(class = "metric-label", "R²")),
                    div(class = "metric-item", div(class = "metric-value", style="color: #27ae60;", m$MAPE_Win), div(class = "metric-label", "MAPE")),
                    div(class = "metric-item", div(class = "metric-value", style="color: #27ae60;", m$NObs_Win), div(class = "metric-label", "Observations"))
                )
            )
        ),
        div(style = "margin-top: 15px; text-align: center; color: #7f8c8d; font-size: 0.9em;",
            paste("Normalization:", m$Norm)
        )
      )
    })

    ##### 2.6. Metrics By Entity Table #####
    #======================================#

    output$entity_table <- DT::renderDT({
      req(input$show_entity_metrics, transformed_data())

      dims <- input$DimensionFilter
      if (is.null(dims)) dims <- character(0)

      res <- aggregate_avm(transformed_data(), dims) %>%
        group_by(across(all_of(dims))) %>%
        summarise(m = list(fit_metrics(Actual, Fitted, k_params())), .groups = "drop") %>%
        unnest(m) %>%
        mutate(MAPE = MAPE / 100) %>%
        rename(Observations = N)

      if (length(dims) == 0) res <- bind_cols(tibble(Entity = "Total Data"), res)

      # DT uses 0-based column indexes
      mape_col_idx <- which(names(res) == "MAPE") - 1

      DT::datatable(res,
                    filter = "top",
                    rownames = FALSE,
                    width = "100%",
                    options = list(
                      pageLength = 5,
                      autoWidth = FALSE,
                      scrollX = TRUE,
                      dom = 'ltip',
                      order = list(list(mape_col_idx, 'desc'))
                    )) %>%
        DT::formatPercentage(c("R2", "R2_Adj", "MAPE"), 2)
    })

    ##### 2.7. Data Download Logic #####
    #==================================#

    output$download_data <- downloadHandler(
      filename = function() { paste("AvM_Data_", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
        req(transformed_data())
        k <- k_params()
        dims <- input$DimensionFilter
        if (is.null(dims)) dims <- character(0)

        # Adds the fit metrics of each group as columns next to its data.
        # Same aggregation (Actual != 0 dropped) as the screen metrics.
        add_metrics <- function(agg, group_cols) {
          agg %>%
            group_by(across(all_of(group_cols))) %>%
            mutate(fit_metrics(Actual, Fitted, k)) %>%
            ungroup()
        }

        df_split <- NULL
        if (length(dims) > 0) {
          df_split <- add_metrics(aggregate_avm(transformed_data(), dims), dims)
        }

        df_total <- add_metrics(aggregate_avm(transformed_data()), character(0))
        for (d in dims) df_total[[d]] <- "Total"

        final_df <- bind_rows(df_split, df_total) %>%
          select(all_of(dims), Period, Actual, Model = Fitted,
                 R2, R2_Adjusted = R2_Adj, MAPE, Observations = N) %>%
          arrange(across(all_of(dims)), Period)

        write_csv(final_df, file)
      }
    )

    ##### 2.8. Plot Rendering #####
    #=============================#

    # Chart 1: Actual vs Model
    output$AVM <- renderPlotly({
      req(agg_data(), kpi_name_clean(), input$reporting_window)
      kpi_label <- kpi_name_clean()

      plot_data <- agg_data() %>%
        select(Period, Actual, Fitted) %>%
        pivot_longer(cols = c("Actual", "Fitted"), names_to = "AVM")

      y_vals <- plot_data$value
      y_min <- min(y_vals, na.rm=TRUE) * 0.95
      y_max <- max(y_vals, na.rm=TRUE) * 1.05

      rect_df <- data.frame(
        xmin = input$reporting_window[1],
        xmax = input$reporting_window[2],
        ymin = y_min,
        ymax = y_max
      )

      p <- ggplot() +
        geom_rect(data = rect_df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  fill="#27ae60", alpha=0.1, inherit.aes = FALSE) +

        geom_line(data = plot_data,
                  aes(x = Period, y = value, color = AVM, group = AVM,
                      text = paste0("<b>Period:</b> ", format(Period, "%Y-%m-%d"), "<br>",
                                    "<b>Type:</b> ", AVM, "<br>",
                                    "<b>Value:</b> ", scales::comma(value, big.mark = ",", accuracy = 0.1))),
                  linewidth = 1) +

        scale_color_manual(values = c('Actual' = '#2980b9', 'Fitted' = '#c0392b')) +
        scale_y_continuous(n.breaks = 6, labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
        labs(y = kpi_label, x = '', color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = "text") %>%
        style(hoverinfo = "skip", traces = 1) %>%
        layout(legend = list(orientation = 'h', x = 0.4, y = -0.2), margin = list(t = 20, b=50))
    })

    # Chart 2: Residuals Performance
    output$residuals <- renderPlotly({
      req(agg_data(), input$reporting_window)
      df_agg <- agg_data() %>% mutate(value = Residual)

      sd_err <- sd(df_agg$value); mean_err <- mean(df_agg$value)
      ci_high <- mean_err + 2 * sd_err; ci_low <- mean_err - 2 * sd_err

      all_y <- c(df_agg$value, ci_high, ci_low)
      y_min <- min(all_y, na.rm=TRUE) * 1.1
      y_max <- max(all_y, na.rm=TRUE) * 1.1
      if(y_min > 0) y_min <- y_min * 0.9
      if(y_max < 0) y_max <- y_max * 0.9

      rect_df <- data.frame(
        xmin = input$reporting_window[1],
        xmax = input$reporting_window[2],
        ymin = y_min,
        ymax = y_max
      )

      p <- ggplot() +
        geom_rect(data = rect_df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                  fill="#27ae60", alpha=0.1, inherit.aes = FALSE) +

        geom_hline(yintercept = 0, color = "gray", linetype = "solid", alpha = 0.3) +
        geom_line(data = df_agg, aes(x = Period, y = value), color = "#34495e", linewidth = 0.5) +
        geom_point(data = df_agg, aes(x = Period, y = value,
                                      text = paste0("<b>Period:</b> ", format(Period, "%Y-%m-%d"), "<br>", "<b>Residual:</b> ", scales::comma(value, accuracy = 1))),
                   color = "#e74c3c", size = 1.2) +
        geom_hline(aes(yintercept = ci_high, text = paste0("<b>Upper Band:</b> ", scales::comma(ci_high, accuracy = 1))), color = "#3498db", linetype = "dashed", alpha = 0.8) +
        geom_hline(aes(yintercept = ci_low, text = paste0("<b>Lower Band:</b> ", scales::comma(ci_low, accuracy = 1))), color = "#3498db", linetype = "dashed", alpha = 0.8) +

        scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
        labs(y = "Residuals", x = "") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = "text") %>%
        style(hoverinfo = "skip", traces = 1)
    })

    # Chart 3: Residuals Seasonality
    output$residual_seasonality <- renderPlotly({
      req(agg_data())
      # ISO week and ISO (week-based) year, so weeks at the turn of the year
      # are assigned to the right year (e.g. week 1 dated in late December).
      df_agg <- agg_data() %>%
        mutate(value = Residual,
               Week = lubridate::isoweek(Period),
               Year = as.character(lubridate::isoyear(Period)),
               Type = "Annual")

      plot_data <- df_agg
      if (isTRUE(input$show_trend)) {
        df_trend <- df_agg %>% group_by(Week) %>% summarise(value = mean(value, na.rm = TRUE)) %>% mutate(Year = "Average Trend", Period = as.Date(NA), Type = "Trend")
        plot_data <- bind_rows(df_agg, df_trend)
      }
      unique_years <- sort(unique(df_agg$Year))
      color_map <- setNames(get_palette(length(unique_years)), unique_years)
      size_map <- setNames(rep(0.6, length(unique_years)), unique_years)
      type_map <- setNames(rep("solid", length(unique_years)), unique_years)
      if (isTRUE(input$show_trend)) { color_map["Average Trend"] <- "black"; size_map["Average Trend"]  <- 1.2; type_map["Average Trend"]  <- "dashed" }
      plot_levels <- if(isTRUE(input$show_trend)) c(unique_years, "Average Trend") else unique_years
      plot_data$Year <- factor(plot_data$Year, levels = plot_levels)
      p <- ggplot(plot_data, aes(x = Week, y = value, color = Year, group = Year, linetype = Year, linewidth = Year,
                                 text = paste0("<b>", ifelse(Year == "Average Trend", "AVERAGE TREND", "Data Point"), "</b><br>",
                                               "<b>Date:</b> ", ifelse(is.na(Period), "All Years", format(Period, "%Y-%m-%d")), "<br>",
                                               "<b>Year:</b> ", Year, "<br>",
                                               "<b>Week:</b> ", Week, "<br>",
                                               "<b>Residual:</b> ", scales::comma(value, accuracy = 1)))) +
        geom_line() +
        scale_color_manual(values = color_map) +
        scale_linewidth_manual(values = size_map) +
        scale_linetype_manual(values = type_map) +
        scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        scale_x_continuous(breaks = seq(1, 53, 1)) +
        labs(x = "Week Number", y = "Residuals") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), panel.grid.minor.x = element_blank())
      ggplotly(p, tooltip = "text")
    })
  })
}
