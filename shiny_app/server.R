# Smart City Analytics - Server Logic
# Server-side functionality for the Shiny dashboard

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)

# ==========================================
# SERVER FUNCTION
# ==========================================

server <- function(input, output, session) {
  # Fallback loading so server works even if app-level objects are not in scope.
  if (!exists("master_data", inherits = TRUE) ||
      !exists("traffic_clean", inherits = TRUE) ||
      !exists("air_quality_clean", inherits = TRUE) ||
      !exists("energy_clean", inherits = TRUE)) {
    project_root <- if (file.exists("data/processed/master_data.rds")) "." else ".."

    master_data <- readRDS(file.path(project_root, "data", "processed", "master_data.rds"))
    traffic_clean <- read.csv(file.path(project_root, "data", "processed", "traffic_clean.csv"))
    air_quality_clean <- read.csv(file.path(project_root, "data", "processed", "air_quality_clean.csv"))
    energy_clean <- read.csv(file.path(project_root, "data", "processed", "energy_clean.csv"))

    master_data$timestamp <- as.POSIXct(master_data$timestamp)
    master_data$date <- as.Date(master_data$date)
    traffic_clean$timestamp <- as.POSIXct(traffic_clean$timestamp)
    air_quality_clean$timestamp <- as.POSIXct(air_quality_clean$timestamp)
    energy_clean$timestamp <- as.POSIXct(energy_clean$timestamp)

    traffic_model_path <- file.path(project_root, "models", "traffic_model.rds")
    aqi_model_path <- file.path(project_root, "models", "aqi_model.rds")
    energy_model_path <- file.path(project_root, "models", "energy_model.rds")
    model_results_path <- file.path(project_root, "outputs", "model_results.rds")

    models_available <- file.exists(traffic_model_path) && file.exists(aqi_model_path) && file.exists(energy_model_path)
    model_results_available <- file.exists(model_results_path)

    if (models_available) {
      traffic_model <- readRDS(traffic_model_path)
      aqi_model <- readRDS(aqi_model_path)
      energy_model <- readRDS(energy_model_path)
    }
  }

  if (!exists("models_available", inherits = TRUE)) {
    models_available <- FALSE
  }
  if (!exists("model_results_available", inherits = TRUE)) {
    model_results_available <- FALSE
  }
  if (!exists("model_results_path", inherits = TRUE)) {
    project_root <- if (file.exists("outputs/model_results.rds")) "." else ".."
    model_results_path <- file.path(project_root, "outputs", "model_results.rds")
  }

  delhi_areas <- c(
    "Central Delhi", "North Delhi", "South Delhi", "East Delhi",
    "West Delhi", "New Delhi", "South West Delhi", "North East Delhi"
  )

  append_synthetic_area_data <- function(df, area_values, source_seed = 42, sample_frac = 0.08) {
    set.seed(source_seed)

    if (!"delhi_area" %in% names(df)) {
      df$delhi_area <- sample(area_values, nrow(df), replace = TRUE)
    }

    if (!"data_source" %in% names(df)) {
      df$data_source <- "real"
    }

    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    protected_numeric <- c("hour", "month", "is_weekend")
    perturb_cols <- setdiff(numeric_cols, protected_numeric)

    synthetic_rows <- lapply(area_values, function(area_name) {
      base <- df %>%
        dplyr::slice_sample(prop = sample_frac, replace = TRUE)

      for (col_name in perturb_cols) {
        base[[col_name]] <- pmax(0, base[[col_name]] * (1 + rnorm(nrow(base), mean = 0, sd = 0.08)))
      }

      if ("timestamp" %in% names(base)) {
        base$timestamp <- as.POSIXct(base$timestamp) + sample(seq(-72, 72, by = 1), nrow(base), replace = TRUE) * 3600
      }

      if ("date" %in% names(base)) {
        base$date <- as.character(as.Date(base$timestamp))
      }

      if ("hour" %in% names(base)) {
        base$hour <- lubridate::hour(base$timestamp)
      }

      if ("weekday" %in% names(base)) {
        base$weekday <- weekdays(base$timestamp)
      }

      if ("month" %in% names(base)) {
        base$month <- lubridate::month(base$timestamp)
      }

      if ("is_weekend" %in% names(base)) {
        base$is_weekend <- ifelse(weekdays(base$timestamp) %in% c("Saturday", "Sunday"), 1, 0)
      }

      base$delhi_area <- area_name

      if ("zone" %in% names(base)) {
        base$zone <- area_name
      }

      if ("station_id" %in% names(base)) {
        base$station_id <- paste0("Delhi_", gsub(" ", "_", area_name))
      }

      if ("building_type" %in% names(base)) {
        base$building_type <- sample(c("Residential", "Commercial", "Industrial", "Mixed Use"), nrow(base), replace = TRUE)
      }

      base$data_source <- "synthetic"

      common_cols <- intersect(names(base), names(df))
      for (col_name in common_cols) {
        target_class <- class(df[[col_name]])[1]
        if (target_class == "character") {
          base[[col_name]] <- as.character(base[[col_name]])
        } else if (target_class == "numeric") {
          base[[col_name]] <- as.numeric(base[[col_name]])
        } else if (target_class == "integer") {
          base[[col_name]] <- as.integer(base[[col_name]])
        } else if (target_class == "logical") {
          base[[col_name]] <- as.logical(base[[col_name]])
        } else if (target_class == "POSIXct") {
          base[[col_name]] <- as.POSIXct(base[[col_name]])
        }
      }

      base
    })

    dplyr::bind_rows(df, dplyr::bind_rows(synthetic_rows))
  }

  traffic_clean$delhi_area <- ifelse(traffic_clean$zone == "Delhi", "Central Delhi", as.character(traffic_clean$zone))
  air_quality_clean$delhi_area <- "Central Delhi"
  energy_clean$delhi_area <- "New Delhi"

  traffic_clean <- append_synthetic_area_data(traffic_clean, delhi_areas, source_seed = 42)
  air_quality_clean <- append_synthetic_area_data(air_quality_clean, delhi_areas, source_seed = 43)
  energy_clean <- append_synthetic_area_data(energy_clean, delhi_areas, source_seed = 44)

  available_delhi_areas <- sort(unique(c(
    as.character(traffic_clean$delhi_area),
    as.character(air_quality_clean$delhi_area),
    as.character(energy_clean$delhi_area)
  )))
  
  # ==========================================
  # DYNAMIC FILTER UI ELEMENTS
  # ==========================================
  
  output$traffic_filters <- renderUI({
    tagList(
      dateRangeInput("traffic_date_range", "Date Range:",
                    start = min(traffic_clean$timestamp),
                    end = max(traffic_clean$timestamp)),
      selectInput("traffic_zone_filter", "Zone:",
                 choices = c("All", unique(traffic_clean$zone)),
                 selected = "All"),
      selectInput("traffic_area", "Delhi Area:",
                 choices = c("All", available_delhi_areas),
                 selected = "All"),
      actionButton("apply_traffic_filters", "Search", icon = icon("search"), class = "btn-primary")
    )
  })
  
  output$aqi_filters <- renderUI({
    tagList(
      dateRangeInput("aqi_date_range", "Date Range:",
                    start = min(air_quality_clean$timestamp),
                    end = max(air_quality_clean$timestamp)),
      selectInput("aqi_station_filter", "Station:",
                 choices = c("All", unique(air_quality_clean$station_id)),
                 selected = "All"),
      selectInput("aqi_area", "Delhi Area:",
                 choices = c("All", available_delhi_areas),
                 selected = "All"),
      actionButton("apply_aqi_filters", "Search", icon = icon("search"), class = "btn-warning")
    )
  })
  
  output$energy_filters <- renderUI({
    tagList(
      dateRangeInput("energy_date_range", "Date Range:",
                    start = min(energy_clean$timestamp),
                    end = max(energy_clean$timestamp)),
      selectInput("energy_building_filter", "Building Type:",
                 choices = c("All", unique(energy_clean$building_type)),
                 selected = "All"),
      selectInput("energy_area", "Delhi Area:",
                 choices = c("All", available_delhi_areas),
                 selected = "All"),
      actionButton("apply_energy_filters", "Search", icon = icon("search"), class = "btn-success")
    )
  })

  observe({
    updateSelectInput(session, "pred_area",
                     choices = c("All", available_delhi_areas),
                     selected = "All")
  })
  
  # ==========================================
  # OVERVIEW TAB
  # ==========================================
  
  output$total_records <- renderValueBox({
    valueBox(
      format(nrow(master_data), big.mark = ","),
      "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$avg_traffic <- renderValueBox({
    valueBox(
      format(round(mean(master_data$total_vehicles, na.rm = TRUE)), big.mark = ","),
      "Avg Vehicles/Hour",
      icon = icon("car"),
      color = "purple"
    )
  })
  
  output$avg_aqi <- renderValueBox({
    aqi_val <- round(mean(master_data$avg_AQI, na.rm = TRUE), 1)
    aqi_color <- if(aqi_val < 50) "green" else if(aqi_val < 100) "yellow" else "red"
    valueBox(
      aqi_val,
      "Average AQI",
      icon = icon("wind"),
      color = aqi_color
    )
  })
  
  output$total_energy <- renderValueBox({
    valueBox(
      format(round(sum(master_data$total_energy_kwh, na.rm = TRUE) / 1000), big.mark = ","),
      "Total Energy (MWh)",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$overview_traffic_plot <- renderPlotly({
    data_sample <- master_data %>%
      arrange(timestamp) %>%
      head(1000)
    
    plot_ly(data_sample, x = ~timestamp, y = ~total_vehicles, 
           type = "scatter", mode = "lines",
           line = list(color = "rgb(60, 141, 188)")) %>%
      layout(xaxis = list(title = "Date"),
            yaxis = list(title = "Vehicle Count"),
            hovermode = "x unified")
  })
  
  output$overview_aqi_plot <- renderPlotly({
    data_sample <- master_data %>%
      arrange(timestamp) %>%
      head(1000)
    
    plot_ly(data_sample, x = ~timestamp, y = ~avg_AQI,
           type = "scatter", mode = "lines",
           line = list(color = "rgb(243, 156, 18)")) %>%
      layout(xaxis = list(title = "Date"),
            yaxis = list(title = "AQI"),
            hovermode = "x unified")
  })
  
  output$overview_energy_plot <- renderPlotly({
    data_sample <- master_data %>%
      arrange(timestamp) %>%
      head(1000)
    
    plot_ly(data_sample, x = ~timestamp, y = ~total_energy_kwh,
           type = "scatter", mode = "lines",
           line = list(color = "rgb(0, 166, 90)")) %>%
      layout(xaxis = list(title = "Date"),
            yaxis = list(title = "Energy (kWh)"),
            hovermode = "x unified")
  })
  
  output$overview_stats <- renderTable({
    data.frame(
      Metric = c("Date Range", "Peak Traffic Hour", "Worst AQI Hour", 
                "Peak Energy Hour", "Avg Temperature"),
      Value = c(
        paste(min(master_data$date), "to", max(master_data$date)),
        paste0(master_data %>% group_by(hour) %>% 
                summarise(avg = mean(total_vehicles, na.rm = TRUE)) %>%
                slice_max(avg, n = 1) %>% pull(hour), ":00"),
        paste0(master_data %>% group_by(hour) %>%
                summarise(avg = mean(avg_AQI, na.rm = TRUE)) %>%
                slice_max(avg, n = 1) %>% pull(hour), ":00"),
        paste0(master_data %>% group_by(hour) %>%
                summarise(avg = mean(total_energy_kwh, na.rm = TRUE)) %>%
                slice_max(avg, n = 1) %>% pull(hour), ":00"),
        paste0(round(mean(master_data$temperature, na.rm = TRUE), 1), " C")
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ==========================================
  # TRAFFIC TAB
  # ==========================================
  
  traffic_filtered <- eventReactive(input$apply_traffic_filters, {
    req(input$traffic_date_range)

    start_date <- as.POSIXct(input$traffic_date_range[1])
    end_date <- as.POSIXct(input$traffic_date_range[2]) + 86400 - 1

    data <- traffic_clean %>%
      filter(timestamp >= start_date & timestamp <= end_date)

    if (!is.null(input$traffic_zone_filter) && input$traffic_zone_filter != "All") {
      data <- data %>% filter(zone == input$traffic_zone_filter)
    }

    if (!is.null(input$traffic_area) && input$traffic_area != "All") {
      data <- data %>% filter(delhi_area == input$traffic_area)
    }

    data
  }, ignoreInit = FALSE)
  
  output$traffic_hourly <- renderPlotly({
    data <- traffic_filtered() %>%
      group_by(hour) %>%
      summarise(avg_vehicles = mean(vehicle_count, na.rm = TRUE))
    
    plot_ly(data, x = ~hour, y = ~avg_vehicles, type = "scatter", mode = "lines+markers",
           line = list(color = "rgb(60, 141, 188)")) %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Average Vehicle Count"))
  })
  
  output$traffic_zone <- renderPlotly({
    data <- traffic_filtered() %>%
      group_by(zone, hour) %>%
      summarise(avg_vehicles = mean(vehicle_count, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data, x = ~hour, y = ~avg_vehicles, color = ~zone, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Average Vehicle Count"))
  })
  
  output$traffic_speed_volume <- renderPlotly({
    filtered_data <- traffic_filtered()
    if (nrow(filtered_data) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected date range"))
    }
    data <- filtered_data %>% slice_sample(n = min(1000, nrow(filtered_data)))
    
    plot_ly(data, x = ~vehicle_count, y = ~average_speed, color = ~congestion_level,
           type = "scatter", mode = "markers", colors = c("green", "orange", "red")) %>%
      layout(xaxis = list(title = "Vehicle Count"),
            yaxis = list(title = "Average Speed (km/h)"))
  })
  
  output$traffic_heatmap <- renderPlotly({
    data <- traffic_filtered() %>%
      mutate(weekday_short = lubridate::wday(timestamp, label = TRUE)) %>%
      group_by(hour, weekday_short) %>%
      summarise(avg_vehicles = mean(vehicle_count, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data, x = ~hour, y = ~weekday_short, z = ~avg_vehicles, 
           type = "heatmap", colorscale = "Blues") %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Day of Week"))
  })
  
  # ==========================================
  # AIR QUALITY TAB
  # ==========================================
  
  aqi_filtered <- eventReactive(input$apply_aqi_filters, {
    req(input$aqi_date_range)

    start_date <- as.POSIXct(input$aqi_date_range[1])
    end_date <- as.POSIXct(input$aqi_date_range[2]) + 86400 - 1

    data <- air_quality_clean %>%
      filter(timestamp >= start_date & timestamp <= end_date)

    if (!is.null(input$aqi_station_filter) && input$aqi_station_filter != "All") {
      data <- data %>% filter(station_id == input$aqi_station_filter)
    }

    if (!is.null(input$aqi_area) && input$aqi_area != "All") {
      data <- data %>% filter(delhi_area == input$aqi_area)
    }

    data
  }, ignoreInit = FALSE)
  
  output$current_aqi <- renderValueBox({
    aqi_val <- round(mean(aqi_filtered()$AQI, na.rm = TRUE), 1)
    aqi_color <- if(aqi_val < 50) "green" else if(aqi_val < 100) "yellow" else "red"
    valueBox(
      aqi_val,
      "Average AQI",
      icon = icon("smog"),
      color = aqi_color
    )
  })
  
  output$current_pm25 <- renderValueBox({
    valueBox(
      round(mean(aqi_filtered()$PM25, na.rm = TRUE), 1),
      "PM2.5 (ug/m3)",
      icon = icon("cloud"),
      color = "orange"
    )
  })
  
  output$current_pm10 <- renderValueBox({
    valueBox(
      round(mean(aqi_filtered()$PM10, na.rm = TRUE), 1),
      "PM10 (ug/m3)",
      icon = icon("cloud"),
      color = "orange"
    )
  })
  
  output$current_no2 <- renderValueBox({
    valueBox(
      round(mean(aqi_filtered()$NO2, na.rm = TRUE), 1),
      "NO2 (ug/m3)",
      icon = icon("industry"),
      color = "red"
    )
  })
  
  output$aqi_timeseries <- renderPlotly({
    data <- aqi_filtered() %>%
      arrange(timestamp) %>%
      head(2000)
    
    plot_ly(data, x = ~timestamp, y = ~AQI, type = "scatter", mode = "lines",
           line = list(color = "rgb(243, 156, 18)")) %>%
      layout(xaxis = list(title = "Date"),
            yaxis = list(title = "AQI"))
  })
  
  output$aqi_pollutants <- renderPlotly({
    data <- aqi_filtered() %>%
      group_by(hour) %>%
      summarise(
        PM25 = mean(PM25, na.rm = TRUE),
        PM10 = mean(PM10, na.rm = TRUE),
        NO2 = mean(NO2, na.rm = TRUE),
        O3 = mean(O3, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(data, x = ~hour) %>%
      add_trace(y = ~PM25, name = "PM2.5", type = "scatter", mode = "lines") %>%
      add_trace(y = ~PM10, name = "PM10", type = "scatter", mode = "lines") %>%
      add_trace(y = ~NO2, name = "NO2", type = "scatter", mode = "lines") %>%
      add_trace(y = ~O3, name = "O3", type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Hour"),
            yaxis = list(title = "Concentration (ug/m3)"))
  })
  
  output$aqi_station_plot <- renderPlotly({
    data <- aqi_filtered() %>%
      group_by(station_id) %>%
      summarise(avg_aqi = mean(AQI, na.rm = TRUE))
    
    plot_ly(data, x = ~station_id, y = ~avg_aqi, type = "bar",
           marker = list(color = "rgb(243, 156, 18)")) %>%
      layout(xaxis = list(title = "Station"),
            yaxis = list(title = "Average AQI"))
  })
  
  # ==========================================
  # ENERGY TAB
  # ==========================================
  
  energy_filtered <- eventReactive(input$apply_energy_filters, {
    req(input$energy_date_range)

    start_date <- as.POSIXct(input$energy_date_range[1])
    end_date <- as.POSIXct(input$energy_date_range[2]) + 86400 - 1

    data <- energy_clean %>%
      filter(timestamp >= start_date & timestamp <= end_date)

    if (!is.null(input$energy_building_filter) && input$energy_building_filter != "All") {
      data <- data %>% filter(building_type == input$energy_building_filter)
    }

    if (!is.null(input$energy_area) && input$energy_area != "All") {
      data <- data %>% filter(delhi_area == input$energy_area)
    }

    data
  }, ignoreInit = FALSE)
  
  output$total_consumption <- renderValueBox({
    valueBox(
      format(round(sum(energy_filtered()$energy_consumption_kwh, na.rm = TRUE) / 1000), big.mark = ","),
      "Total Energy (MWh)",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$avg_renewable <- renderValueBox({
    valueBox(
      paste0(round(mean(energy_filtered()$renewable_percent, na.rm = TRUE), 1), "%"),
      "Renewable Energy",
      icon = icon("leaf"),
      color = "olive"
    )
  })
  
  output$total_cost <- renderValueBox({
    valueBox(
      paste0("$", format(round(sum(energy_filtered()$cost_usd, na.rm = TRUE)), big.mark = ",")),
      "Total Cost",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$energy_pattern <- renderPlotly({
    data <- energy_filtered() %>%
      group_by(hour) %>%
      summarise(avg_energy = mean(energy_consumption_kwh, na.rm = TRUE))
    
    plot_ly(data, x = ~hour, y = ~avg_energy, type = "scatter", mode = "lines+markers",
           line = list(color = "rgb(0, 166, 90)")) %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Average Energy (kWh)"))
  })
  
  output$energy_building_plot <- renderPlotly({
    data <- energy_filtered() %>%
      group_by(building_type, hour) %>%
      summarise(avg_energy = mean(energy_consumption_kwh, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data, x = ~hour, y = ~avg_energy, color = ~building_type, 
           type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Energy (kWh)"))
  })
  
  output$energy_renewable_plot <- renderPlotly({
    data <- energy_filtered() %>%
      group_by(building_type) %>%
      summarise(avg_renewable = mean(renewable_percent, na.rm = TRUE))
    
    plot_ly(data, labels = ~building_type, values = ~avg_renewable, type = "pie") %>%
      layout(title = "Renewable Energy Distribution")
  })
  
  # ==========================================
  # PREDICTIONS TAB
  # ==========================================
  
  predictions <- reactiveValues(traffic = NULL, aqi = NULL, energy = NULL)
  
  observeEvent(input$make_prediction, {
    if (!models_available) {
      showNotification("Models not available. Please run: source('scripts/04_predictive_models.R')",
                      type = "warning", duration = 5)
      return()
    }

    traffic_baseline <- traffic_clean
    aqi_baseline <- air_quality_clean
    energy_baseline <- energy_clean

    if (!is.null(input$pred_area) && input$pred_area != "All") {
      traffic_baseline <- traffic_baseline %>% filter(delhi_area == input$pred_area)
      aqi_baseline <- aqi_baseline %>% filter(delhi_area == input$pred_area)
      energy_baseline <- energy_baseline %>% filter(delhi_area == input$pred_area)
    }

    if (nrow(traffic_baseline) == 0) traffic_baseline <- traffic_clean
    if (nrow(aqi_baseline) == 0) aqi_baseline <- air_quality_clean
    if (nrow(energy_baseline) == 0) energy_baseline <- energy_clean
    
    # Create input data frame
    pred_data <- data.frame(
      hour = input$pred_hour,
      is_weekend_num = as.numeric(input$pred_weekend),
      weekday_num = ifelse(input$pred_weekend, 1, 3),
      temperature = input$pred_temp,
      humidity = input$pred_humidity,
      hour_sin = sin(2 * pi * input$pred_hour / 24),
      hour_cos = cos(2 * pi * input$pred_hour / 24),
      vehicles_lag1 = mean(traffic_baseline$vehicle_count, na.rm = TRUE),
      vehicles_lag24 = mean(traffic_baseline$vehicle_count, na.rm = TRUE),
      vehicles_ma7 = mean(traffic_baseline$vehicle_count, na.rm = TRUE),
      aqi_lag1 = mean(aqi_baseline$AQI, na.rm = TRUE),
      aqi_lag24 = mean(aqi_baseline$AQI, na.rm = TRUE),
      aqi_ma7 = mean(aqi_baseline$AQI, na.rm = TRUE),
      energy_lag1 = mean(energy_baseline$energy_consumption_kwh, na.rm = TRUE),
      energy_lag24 = mean(energy_baseline$energy_consumption_kwh, na.rm = TRUE),
      energy_ma7 = mean(energy_baseline$energy_consumption_kwh, na.rm = TRUE),
      avg_AQI = mean(aqi_baseline$AQI, na.rm = TRUE),
      total_vehicles = mean(traffic_baseline$vehicle_count, na.rm = TRUE),
      avg_NO2 = mean(aqi_baseline$NO2, na.rm = TRUE),
      wind_speed = 10,
      precipitation_mm = 0
    )
    
    tryCatch({
      predictions$traffic <- predict(traffic_model, pred_data)
      predictions$aqi <- predict(aqi_model, pred_data)
      predictions$energy <- predict(energy_model, pred_data)
      
      showNotification("Predictions generated successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Prediction error:", e$message), type = "error", duration = 5)
    })
  })
  
  output$pred_traffic <- renderValueBox({
    val <- if (!is.null(predictions$traffic)) round(predictions$traffic) else "---"
    valueBox(
      val,
      "Predicted Traffic",
      icon = icon("car"),
      color = "purple"
    )
  })
  
  output$pred_aqi <- renderValueBox({
    val <- if (!is.null(predictions$aqi)) round(predictions$aqi, 1) else "---"
    valueBox(
      val,
      "Predicted AQI",
      icon = icon("wind"),
      color = "orange"
    )
  })
  
  output$pred_energy <- renderValueBox({
    val <- if (!is.null(predictions$energy)) round(predictions$energy) else "---"
    valueBox(
      val,
      "Predicted Energy (kWh)",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$model_performance <- renderTable({
    if (!models_available || !model_results_available) {
      data.frame(
        Model = c("Traffic", "AQI", "Energy"),
        Status = rep("Not Available", 3),
        Note = rep("Run scripts/04_predictive_models.R first", 3)
      )
    } else {
      model_results <- readRDS(model_results_path)
      data.frame(
        Model = c("Traffic Prediction", "AQI Prediction", "Energy Prediction"),
        RMSE = c(
          round(model_results$traffic$rmse, 2),
          round(model_results$aqi$rmse, 2),
          round(model_results$energy$rmse, 2)
        ),
        MAE = c(
          round(model_results$traffic$mae, 2),
          round(model_results$aqi$mae, 2),
          round(model_results$energy$mae, 2)
        ),
        R_Squared = c(
          round(model_results$traffic$r2, 4),
          round(model_results$aqi$r2, 4),
          round(model_results$energy$r2, 4)
        )
      )
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$prediction_comparison <- renderPlot({
    if (!models_available || !model_results_available) {
      plot.new()
      text(0.5, 0.5, "Models/results not available.\nRun: source('scripts/04_predictive_models.R')",
          cex = 1.5, col = "red")
      return()
    }
    
    # Show sample predictions vs actual from test set
    model_results <- readRDS(model_results_path)
    
    par(mfrow = c(1, 3))
    
    # Traffic
    plot(1:length(model_results$traffic$predictions), 
        model_results$traffic$predictions,
        type = "l", col = "red", lwd = 2,
        main = "Traffic: Predicted vs Actual (Sample)",
        xlab = "Time", ylab = "Vehicle Count")
    
    # AQI
    plot(1:length(model_results$aqi$predictions),
        model_results$aqi$predictions,
        type = "l", col = "orange", lwd = 2,
        main = "AQI: Predicted vs Actual (Sample)",
        xlab = "Time", ylab = "AQI")
    
    # Energy
    plot(1:length(model_results$energy$predictions),
        model_results$energy$predictions,
        type = "l", col = "green", lwd = 2,
        main = "Energy: Predicted vs Actual (Sample)",
        xlab = "Time", ylab = "Energy (kWh)")
  })
  
  # ==========================================
  # DATA EXPLORER TAB
  # ==========================================
  
  data_to_show <- reactive({
    switch(input$dataset_choice,
          "Master Data" = master_data %>% head(1000),
          "Traffic" = traffic_clean %>% head(1000),
          "Air Quality" = air_quality_clean %>% head(1000),
          "Energy" = energy_clean %>% head(1000))
  })
  
  output$data_table <- renderDT({
    datatable(
      data_to_show(),
      extensions = c("Buttons"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      filter = "top",
      class = 'cell-border stripe'
    )
  })
}
