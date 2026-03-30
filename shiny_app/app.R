# Smart City Analytics - Shiny Dashboard
# Interactive dashboard for smart city monitoring and analysis
# 
# This is the main entry point for the Shiny application.
# UI components are defined in ui.R
# Server logic is defined in server.R

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(lubridate)

# ==========================================
# PATHS
# ==========================================

app_dir <- if (basename(getwd()) == "shiny_app") getwd() else file.path(getwd(), "shiny_app")
project_root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = FALSE)

if (!dir.exists(app_dir) || !file.exists(file.path(app_dir, "ui.R"))) {
  stop("Could not locate shiny_app directory. Launch with shiny::runApp('shiny_app') from the project root.")
}

project_path <- function(...) {
  file.path(project_root, ...)
}

# ==========================================
# LOAD DATA
# ==========================================

# Check if data exists, if not run preprocessing
if (!file.exists(project_path("data", "processed", "master_data.rds"))) {
  message("Processed data not found. Running preprocessing...")
  source(project_path("scripts", "01_data_preprocessing.R"))
}

# Load datasets
master_data <- readRDS(project_path("data", "processed", "master_data.rds"))
traffic_clean <- read.csv(project_path("data", "processed", "traffic_clean.csv"))
air_quality_clean <- read.csv(project_path("data", "processed", "air_quality_clean.csv"))
energy_clean <- read.csv(project_path("data", "processed", "energy_clean.csv"))

# Convert timestamps
master_data$timestamp <- as.POSIXct(master_data$timestamp)
master_data$date <- as.Date(master_data$date)
traffic_clean$timestamp <- as.POSIXct(traffic_clean$timestamp)
air_quality_clean$timestamp <- as.POSIXct(air_quality_clean$timestamp)
energy_clean$timestamp <- as.POSIXct(energy_clean$timestamp)

# Load models if available
traffic_model_path <- project_path("models", "traffic_model.rds")
aqi_model_path <- project_path("models", "aqi_model.rds")
energy_model_path <- project_path("models", "energy_model.rds")
model_results_path <- project_path("outputs", "model_results.rds")

models_available <- file.exists(traffic_model_path) && file.exists(aqi_model_path) && file.exists(energy_model_path)
model_results_available <- file.exists(model_results_path)

if (models_available) {
  traffic_model <- readRDS(traffic_model_path)
  aqi_model <- readRDS(aqi_model_path)
  energy_model <- readRDS(energy_model_path)
}

# ==========================================
# SOURCE UI AND SERVER COMPONENTS
# ==========================================

source(file.path(app_dir, "ui.R"), local = environment())

server <- local({
  master_data <- master_data
  traffic_clean <- traffic_clean
  air_quality_clean <- air_quality_clean
  energy_clean <- energy_clean

  models_available <- models_available
  model_results_available <- model_results_available
  model_results_path <- model_results_path

  traffic_model <- if (exists("traffic_model", inherits = FALSE)) traffic_model else NULL
  aqi_model <- if (exists("aqi_model", inherits = FALSE)) aqi_model else NULL
  energy_model <- if (exists("energy_model", inherits = FALSE)) energy_model else NULL

  source(file.path(app_dir, "server.R"), local = environment())
  server
})

# ==========================================
# RUN APP
# ==========================================

shinyApp(ui = ui, server = server)
