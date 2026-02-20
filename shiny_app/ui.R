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
    title = "Smart City Analytics",
    titleWidth = 280
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
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
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .small-box { border-radius: 5px; }
      "))
    ),
    
    tabItems(
      # ==========================================
      # OVERVIEW TAB
      # ==========================================
      tabItem(
        tabName = "overview",
        h2("Smart City Dashboard Overview"),
        
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
        h2("Traffic Analysis"),
        
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
        h2("Air Quality Monitoring"),
        
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
        h2("Energy Management"),
        
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
        h2("Predictive Analytics"),
        
        fluidRow(
          box(
            title = "Prediction Input", status = "info", solidHeader = TRUE,
            width = 12,
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
        h2("Data Explorer"),
        
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
        h2("About Smart City Analytics"),
        
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
            p("This dashboard uses synthetic data generated for demonstration purposes. 
              In a production environment, it would connect to real-time city sensors and APIs.")
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
