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
  
  # ==========================================
  # DYNAMIC FILTER UI ELEMENTS
  # ==========================================
  
  output$traffic_filters <- renderUI({
    tagList(
      dateRangeInput("traffic_date_range", "Date Range:",
                    start = min(traffic_clean$timestamp),
                    end = max(traffic_clean$timestamp)),
      selectInput("traffic_zone", "Zone:",
                 choices = c("All", unique(traffic_clean$zone)),
                 selected = "All")
    )
  })
  
  output$aqi_filters <- renderUI({
    tagList(
      dateRangeInput("aqi_date_range", "Date Range:",
                    start = min(air_quality_clean$timestamp),
                    end = max(air_quality_clean$timestamp)),
      selectInput("aqi_station", "Station:",
                 choices = c("All", unique(air_quality_clean$station_id)),
                 selected = "All")
    )
  })
  
  output$energy_filters <- renderUI({
    tagList(
      dateRangeInput("energy_date_range", "Date Range:",
                    start = min(energy_clean$timestamp),
                    end = max(energy_clean$timestamp)),
      selectInput("energy_building", "Building Type:",
                 choices = c("All", unique(energy_clean$building_type)),
                 selected = "All")
    )
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
  
  traffic_filtered <- reactive({
    req(input$traffic_date_range)
    data <- traffic_clean %>%
      filter(timestamp >= input$traffic_date_range[1] &
            timestamp <= input$traffic_date_range[2])
    
    if (!is.null(input$traffic_zone) && input$traffic_zone != "All") {
      data <- data %>% filter(zone == input$traffic_zone)
    }
    
    data
  })
  
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
    data <- traffic_clean %>%
      group_by(zone, hour) %>%
      summarise(avg_vehicles = mean(vehicle_count, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data, x = ~hour, y = ~avg_vehicles, color = ~zone, type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Average Vehicle Count"))
  })
  
  output$traffic_speed_volume <- renderPlotly({
    data <- traffic_filtered() %>% sample_n(min(1000, nrow(traffic_filtered())))
    
    plot_ly(data, x = ~vehicle_count, y = ~average_speed, color = ~congestion_level,
           type = "scatter", mode = "markers", colors = c("green", "orange", "red")) %>%
      layout(xaxis = list(title = "Vehicle Count"),
            yaxis = list(title = "Average Speed (km/h)"))
  })
  
  output$traffic_heatmap <- renderPlotly({
    data <- traffic_clean %>%
      mutate(weekday_short = wday(timestamp, label = TRUE)) %>%
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
  
  aqi_filtered <- reactive({
    req(input$aqi_date_range)
    data <- air_quality_clean %>%
      filter(timestamp >= input$aqi_date_range[1] &
            timestamp <= input$aqi_date_range[2])
    
    if (!is.null(input$aqi_station) && input$aqi_station != "All") {
      data <- data %>% filter(station_id == input$aqi_station)
    }
    
    data
  })
  
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
    data <- air_quality_clean %>%
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
  
  energy_filtered <- reactive({
    req(input$energy_date_range)
    data <- energy_clean %>%
      filter(timestamp >= input$energy_date_range[1] &
            timestamp <= input$energy_date_range[2])
    
    if (!is.null(input$energy_building) && input$energy_building != "All") {
      data <- data %>% filter(building_type == input$energy_building)
    }
    
    data
  })
  
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
    data <- energy_clean %>%
      group_by(building_type, hour) %>%
      summarise(avg_energy = mean(energy_consumption_kwh, na.rm = TRUE), .groups = "drop")
    
    plot_ly(data, x = ~hour, y = ~avg_energy, color = ~building_type, 
           type = "scatter", mode = "lines") %>%
      layout(xaxis = list(title = "Hour of Day"),
            yaxis = list(title = "Energy (kWh)"))
  })
  
  output$energy_renewable_plot <- renderPlotly({
    data <- energy_clean %>%
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
    
    # Create input data frame
    pred_data <- data.frame(
      hour = input$pred_hour,
      is_weekend_num = as.numeric(input$pred_weekend),
      weekday_num = ifelse(input$pred_weekend, 1, 3),
      temperature = input$pred_temp,
      humidity = input$pred_humidity,
      hour_sin = sin(2 * pi * input$pred_hour / 24),
      hour_cos = cos(2 * pi * input$pred_hour / 24),
      vehicles_lag1 = mean(master_data$total_vehicles, na.rm = TRUE),
      vehicles_lag24 = mean(master_data$total_vehicles, na.rm = TRUE),
      vehicles_ma7 = mean(master_data$total_vehicles, na.rm = TRUE),
      aqi_lag1 = mean(master_data$avg_AQI, na.rm = TRUE),
      aqi_lag24 = mean(master_data$avg_AQI, na.rm = TRUE),
      aqi_ma7 = mean(master_data$avg_AQI, na.rm = TRUE),
      energy_lag1 = mean(master_data$total_energy_kwh, na.rm = TRUE),
      energy_lag24 = mean(master_data$total_energy_kwh, na.rm = TRUE),
      energy_ma7 = mean(master_data$total_energy_kwh, na.rm = TRUE),
      avg_AQI = mean(master_data$avg_AQI, na.rm = TRUE),
      total_vehicles = mean(master_data$total_vehicles, na.rm = TRUE),
      avg_NO2 = mean(master_data$avg_NO2, na.rm = TRUE),
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
    if (!models_available) {
      data.frame(
        Model = c("Traffic", "AQI", "Energy"),
        Status = rep("Not Available", 3),
        Note = rep("Run predictive models script first", 3)
      )
    } else {
      model_results <- readRDS("../outputs/model_results.rds")
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
    if (!models_available) {
      plot.new()
      text(0.5, 0.5, "Models not available.\nRun: source('scripts/04_predictive_models.R')",
          cex = 1.5, col = "red")
      return()
    }
    
    # Show sample predictions vs actual from test set
    model_results <- readRDS("../outputs/model_results.rds")
    
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
