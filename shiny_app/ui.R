# Smart City Analytics - UI Components
# User interface definition for the Shiny dashboard

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)

# ==========================================
# UI DEFINITION
# ==========================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      class = "brand-wrap",
      tags$span(class = "brand-main", "SmartCity"),
      tags$span(class = "brand-accent", "Analytics")
    ),
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 260,
    tags$div(
      class = "side-brand-mini",
      tags$div(class = "mini-title", "Delhi City Analytics"),
      tags$div(class = "mini-sub", "Traffic, Air Quality, Energy")
    ),
    tags$div(class = "sidebar-section-label", "Navigation"),
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Traffic Analysis", tabName = "traffic", icon = icon("car")),
      menuItem("Air Quality", tabName = "air_quality", icon = icon("wind")),
      menuItem("Energy", tabName = "energy", icon = icon("bolt")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700;800&family=JetBrains+Mono:wght@500&display=swap"
      ),
      tags$style(HTML("
        :root {
          --bg-main: #f7f8fa;
          --bg-elevated: #ffffff;
          --text-primary: #101828;
          --text-muted: #4b5565;
          --border-soft: #e5e7eb;
          --accent: #0f766e;
          --sidebar-bg: #fcfcfd;
          --sidebar-hover: #f3f4f6;
          --sidebar-active-bg: #e6f4f1;
          --sidebar-active-border: #7ed7ca;
        }

        body, h1, h2, h3, h4, h5, p, .box-title, .main-header .logo, .sidebar-menu > li > a {
          font-family: 'Manrope', sans-serif;
        }

        .content-wrapper,
        .right-side {
          background:
            radial-gradient(circle at 12% 8%, rgba(15, 118, 110, 0.08), transparent 34%),
            radial-gradient(circle at 90% 15%, rgba(2, 6, 23, 0.05), transparent 38%),
            var(--bg-main);
          color: var(--text-primary);
        }

        .wrapper {
          min-height: 100vh;
        }

        .main-header .navbar {
          background: #ffffff !important;
          border-bottom: 1px solid #e5e7eb;
        }

        .main-header .logo {
          background: #ffffff !important;
          border-bottom: 1px solid #e5e7eb;
          border-right: 1px solid #e5e7eb;
          font-weight: 800;
          letter-spacing: 0.2px;
          text-align: left;
          padding-left: 18px;
        }

        .brand-wrap {
          display: flex;
          align-items: baseline;
          gap: 6px;
        }

        .brand-main {
          color: #111827;
        }

        .brand-accent {
          color: #0f766e;
          font-weight: 800;
        }

        .main-sidebar,
        .left-side {
          background: #ffffff !important;
          border-right: 1px solid #e6e9ef;
        }

        .sidebar {
          padding-top: 12px;
        }

        .side-brand-mini {
          margin: 4px 14px 14px 14px;
          padding: 2px 4px 12px 4px;
          border-bottom: 1px solid #edf0f4;
        }

        .mini-title {
          color: #0f1720;
          font-size: 17px;
          font-weight: 800;
          letter-spacing: 0.1px;
          margin-bottom: 2px;
        }

        .mini-sub {
          color: #667689;
          font-size: 12px;
          font-weight: 600;
        }

        .sidebar-section-label {
          color: #8593a3;
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 1px;
          font-weight: 700;
          margin: 8px 16px 10px 16px;
        }

        .sidebar-menu > li > a {
          color: #273444 !important;
          border-radius: 10px;
          margin: 4px 10px;
          padding: 11px 14px;
          font-weight: 700;
          border: 1px solid transparent;
          background: transparent;
          transition: all 0.2s ease;
        }

        .sidebar-menu > li > a > .fa {
          width: 18px;
          text-align: center;
          margin-right: 10px;
          color: #7a8da3;
        }

        .sidebar-menu > li:hover > a {
          background: #f7f9fb !important;
          color: #1f2a37 !important;
          border-color: #e6ebf2;
        }

        .sidebar-menu > li.active > a {
          background: #e9f7f5 !important;
          color: #0e5f59 !important;
          border-color: #b7e6e0;
          box-shadow: 0 1px 0 rgba(255, 255, 255, 0.7);
        }

        .sidebar-menu > li.active > a > .fa,
        .sidebar-menu > li:hover > a > .fa {
          color: #0f766e;
        }

        .content {
          padding: 20px;
        }

        .page-title {
          margin: 0;
          font-size: 30px;
          font-weight: 800;
          color: var(--text-primary);
          letter-spacing: -0.5px;
        }

        .page-subtitle {
          margin: 4px 0 18px 0;
          color: var(--text-muted);
          font-size: 14px;
          font-weight: 500;
        }

        .box {
          border-top: 0 !important;
          border-radius: 16px;
          background: var(--bg-elevated);
          border: 1px solid var(--border-soft);
          box-shadow: 0 10px 24px rgba(15, 23, 42, 0.045);
          overflow: hidden;
        }

        .box-header {
          border-bottom: 1px solid #eef2f6;
          padding: 14px 16px;
          background: linear-gradient(180deg, #ffffff, #fbfcfd);
        }

        .box-title {
          font-size: 15px;
          font-weight: 700;
          color: #132030;
        }

        .box-body {
          padding: 14px 16px 16px 16px;
        }

        .small-box {
          border-radius: 16px;
          border: 1px solid var(--border-soft);
          box-shadow: 0 8px 20px rgba(9, 30, 66, 0.06);
          overflow: hidden;
        }

        .small-box h3,
        .small-box p {
          color: #ffffff;
        }

        .small-box .inner h3 {
          font-size: 26px;
          font-weight: 800;
          letter-spacing: -0.4px;
        }

        .small-box .inner p {
          font-weight: 600;
        }

        .btn {
          border-radius: 10px !important;
          font-weight: 700;
          letter-spacing: 0.1px;
        }

        .form-control,
        .selectize-input {
          border-radius: 10px !important;
          border-color: #d5dfe9 !important;
          min-height: 42px;
          font-weight: 500;
        }

        .irs--shiny .irs-bar,
        .irs--shiny .irs-single,
        .irs--shiny .irs-handle > i:first-child {
          background: var(--accent);
          border-top-color: var(--accent);
          border-bottom-color: var(--accent);
        }

        .dataTables_wrapper .dataTables_filter input {
          border-radius: 8px;
          border: 1px solid #d5dfe9;
          padding: 6px 10px;
        }

        @media (max-width: 992px) {
          .page-title {
            font-size: 24px;
          }

          .content {
            padding: 12px;
          }
        }
      "))
    ),
    
    tabItems(
      # ==========================================
      # OVERVIEW TAB
      # ==========================================
      tabItem(
        tabName = "overview",
        tags$h2(class = "page-title", "Smart City Dashboard Overview"),
        tags$p(class = "page-subtitle", "Live city intelligence across traffic, air quality, and energy systems."),
        
        fluidRow(
          valueBoxOutput("total_records", width = 3),
          valueBoxOutput("avg_traffic", width = 3),
          valueBoxOutput("avg_aqi", width = 3),
          valueBoxOutput("total_energy", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Traffic Trends", status = "primary", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("overview_traffic_plot", height = 350)
          ),
          box(
            title = "Air Quality Trends", status = "warning", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("overview_aqi_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Energy Consumption", status = "success", solidHeader = TRUE,
            width = 6, height = 400,
            plotlyOutput("overview_energy_plot", height = 350)
          ),
          box(
            title = "Quick Statistics", status = "info", solidHeader = TRUE,
            width = 6, height = 400,
            tableOutput("overview_stats")
          )
        )
      ),
      
      # ==========================================
      # TRAFFIC TAB
      # ==========================================
      tabItem(
        tabName = "traffic",
        tags$h2(class = "page-title", "Traffic Analysis"),
        tags$p(class = "page-subtitle", "Analyze congestion, speed, and flow patterns by time window and Delhi area."),
        
        fluidRow(
          box(
            title = "Filters", status = "primary", solidHeader = TRUE,
            width = 12,
            uiOutput("traffic_filters")
          )
        ),
        
        fluidRow(
          box(
            title = "Hourly Traffic Pattern", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("traffic_hourly", height = 400)
          ),
          box(
            title = "Traffic by Zone", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("traffic_zone", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Speed vs Volume", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("traffic_speed_volume", height = 400)
          ),
          box(
            title = "Congestion Heatmap", status = "primary", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("traffic_heatmap", height = 400)
          )
        )
      ),
      
      # ==========================================
      # AIR QUALITY TAB
      # ==========================================
      tabItem(
        tabName = "air_quality",
        tags$h2(class = "page-title", "Air Quality Monitoring"),
        tags$p(class = "page-subtitle", "Track AQI behavior, pollutant mix, and station-level distribution."),
        
        fluidRow(
          box(
            title = "Filters", status = "warning", solidHeader = TRUE,
            width = 12,
            uiOutput("aqi_filters")
          )
        ),
        
        fluidRow(
          valueBoxOutput("current_aqi", width = 3),
          valueBoxOutput("current_pm25", width = 3),
          valueBoxOutput("current_pm10", width = 3),
          valueBoxOutput("current_no2", width = 3)
        ),
        
        fluidRow(
          box(
            title = "AQI Time Series", status = "warning", solidHeader = TRUE,
            width = 12, height = 450,
            plotlyOutput("aqi_timeseries", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Pollutant Levels", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("aqi_pollutants", height = 400)
          ),
          box(
            title = "AQI by Station", status = "warning", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("aqi_station_plot", height = 400)
          )
        )
      ),
      
      # ==========================================
      # ENERGY TAB
      # ==========================================
      tabItem(
        tabName = "energy",
        tags$h2(class = "page-title", "Energy Management"),
        tags$p(class = "page-subtitle", "Monitor load patterns, renewable adoption, and demand by building profile."),
        
        fluidRow(
          box(
            title = "Filters", status = "success", solidHeader = TRUE,
            width = 12,
            uiOutput("energy_filters")
          )
        ),
        
        fluidRow(
          valueBoxOutput("total_consumption", width = 4),
          valueBoxOutput("avg_renewable", width = 4),
          valueBoxOutput("total_cost", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Energy Consumption Pattern", status = "success", solidHeader = TRUE,
            width = 12, height = 450,
            plotlyOutput("energy_pattern", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Consumption by Building Type", status = "success", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("energy_building_plot", height = 400)
          ),
          box(
            title = "Renewable Energy Usage", status = "success", solidHeader = TRUE,
            width = 6, height = 450,
            plotlyOutput("energy_renewable_plot", height = 400)
          )
        )
      ),
      
      # ==========================================
      # PREDICTIONS TAB
      # ==========================================
      tabItem(
        tabName = "predictions",
        tags$h2(class = "page-title", "Predictive Analytics"),
        tags$p(class = "page-subtitle", "Generate scenario-based forecasts for selected Delhi areas."),
        
        fluidRow(
          box(
            title = "Prediction Input", status = "info", solidHeader = TRUE,
            width = 12,
            selectInput("pred_area", "Delhi Area:",
                       choices = c("All"), selected = "All"),
            sliderInput("pred_hour", "Hour of Day:", min = 0, max = 23, value = 12),
            checkboxInput("pred_weekend", "Weekend", value = FALSE),
            sliderInput("pred_temp", "Temperature (C):", min = -10, max = 40, value = 20),
            sliderInput("pred_humidity", "Humidity (%):", min = 0, max = 100, value = 60),
            actionButton("make_prediction", "Make Prediction", 
                        class = "btn-primary", icon = icon("play"))
          )
        ),
        
        fluidRow(
          valueBoxOutput("pred_traffic", width = 4),
          valueBoxOutput("pred_aqi", width = 4),
          valueBoxOutput("pred_energy", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Model Performance", status = "info", solidHeader = TRUE,
            width = 12,
            tableOutput("model_performance")
          )
        ),
        
        fluidRow(
          box(
            title = "Prediction Visualization", status = "info", solidHeader = TRUE,
            width = 12, height = 450,
            plotOutput("prediction_comparison", height = 400)
          )
        )
      ),
      
      # ==========================================
      # DATA EXPLORER TAB
      # ==========================================
      tabItem(
        tabName = "data",
        tags$h2(class = "page-title", "Data Explorer"),
        tags$p(class = "page-subtitle", "Inspect source datasets and validate analytical assumptions."),

        fluidRow(
          box(
            title = "Upload Your Datasets", status = "primary", solidHeader = TRUE,
            width = 12,
            p("Upload CSV files matching the project schema. You can upload one, many, or all datasets."),
            fluidRow(
              column(6,
                fileInput("upload_master_csv", "Master Data CSV",
                          accept = c(".csv"),
                          placeholder = "Choose master_data CSV"),
                tags$small("Required: timestamp, date, total_vehicles, avg_AQI, total_energy_kwh")
              ),
              column(6,
                fileInput("upload_traffic_csv", "Traffic CSV",
                          accept = c(".csv"),
                          placeholder = "Choose traffic CSV"),
                tags$small("Required: timestamp, zone, hour, vehicle_count, average_speed, congestion_level")
              )
            ),
            br(),
            fluidRow(
              column(6,
                fileInput("upload_aqi_csv", "Air Quality CSV",
                          accept = c(".csv"),
                          placeholder = "Choose air_quality CSV"),
                tags$small("Required: timestamp, station_id, hour, AQI, PM25, PM10, NO2, O3")
              ),
              column(6,
                fileInput("upload_energy_csv", "Energy CSV",
                          accept = c(".csv"),
                          placeholder = "Choose energy CSV"),
                tags$small("Required: timestamp, building_type, hour, energy_consumption_kwh, renewable_percent, cost_usd")
              )
            ),
            br(),
            actionButton("apply_uploaded_data", "Apply Uploaded Dataset(s)",
                         class = "btn-success", icon = icon("upload")),
            tags$span(" "),
            textOutput("upload_status", inline = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = "Select Dataset", status = "primary", solidHeader = TRUE,
            width = 12,
            selectInput("dataset_choice", "Choose Dataset:",
                       choices = c("Master Data", "Traffic", "Air Quality", "Energy"),
                       selected = "Master Data")
          )
        ),
        
        fluidRow(
          box(
            title = "Data Table", status = "primary", solidHeader = TRUE,
            width = 12,
            DTOutput("data_table")
          )
        )
      ),
      
      # ==========================================
      # ABOUT TAB
      # ==========================================
      tabItem(
        tabName = "about",
        tags$h2(class = "page-title", "About Smart City Analytics"),
        tags$p(class = "page-subtitle", "Project context, methodology, and interpretation guidelines."),
        
        fluidRow(
          box(
            title = "Project Overview", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("Smart City Data Analytics System"),
            p("A comprehensive R-based data analytics system for smart city monitoring, 
              featuring traffic analysis, air quality monitoring, and energy consumption patterns."),
            h4("Features:"),
            tags$ul(
              tags$li("Real-time traffic flow and congestion analysis"),
              tags$li("Air quality index (AQI) monitoring across multiple stations"),
              tags$li("Energy consumption tracking by building type"),
              tags$li("Predictive models for traffic, AQI, and energy demand"),
              tags$li("Interactive visualizations and dashboards")
            ),
            h4("Technology Stack:"),
            p("Built with R, Shiny, ggplot2, plotly, and machine learning libraries."),
            h4("Data Sources:"),
            p("This dashboard is designed for real city datasets (traffic, air quality, energy, and weather). 
              For academic submission, document the city name, time period, and official data sources used.")
          )
        ),
        
        fluidRow(
          box(
            title = "Key Metrics Explained", status = "info", solidHeader = TRUE,
            width = 12,
            h4("Air Quality Index (AQI):"),
            tags$ul(
              tags$li("0-50: Good (Green)"),
              tags$li("51-100: Moderate (Yellow)"),
              tags$li("101-150: Unhealthy for Sensitive Groups (Orange)"),
              tags$li("151-200: Unhealthy (Red)"),
              tags$li("201-300: Very Unhealthy (Purple)"),
              tags$li("301+: Hazardous (Maroon)")
            ),
            h4("Traffic Congestion Levels:"),
            tags$ul(
              tags$li("Low: Free-flowing traffic"),
              tags$li("Medium: Moderate delays"),
              tags$li("High: Significant congestion")
            )
          )
        )
      )
    )
  )
)
