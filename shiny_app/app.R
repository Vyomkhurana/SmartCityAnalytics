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
# LOAD DATA
# ==========================================

# Check if data exists, if not run preprocessing
if (!file.exists("../data/processed/master_data.rds")) {
  message("Processed data not found. Running preprocessing...")
  source("../scripts/01_data_preprocessing.R")
}

# Load datasets
master_data <- readRDS("../data/processed/master_data.rds")
traffic_clean <- read.csv("../data/processed/traffic_clean.csv")
air_quality_clean <- read.csv("../data/processed/air_quality_clean.csv")
energy_clean <- read.csv("../data/processed/energy_clean.csv")

# Convert timestamps
master_data$timestamp <- as.POSIXct(master_data$timestamp)
master_data$date <- as.Date(master_data$date)
traffic_clean$timestamp <- as.POSIXct(traffic_clean$timestamp)
air_quality_clean$timestamp <- as.POSIXct(air_quality_clean$timestamp)
energy_clean$timestamp <- as.POSIXct(energy_clean$timestamp)

# Load models if available
models_available <- file.exists("../models/traffic_model.rds")
if (models_available) {
  traffic_model <- readRDS("../models/traffic_model.rds")
  aqi_model <- readRDS("../models/aqi_model.rds")
  energy_model <- readRDS("../models/energy_model.rds")
}

# ==========================================
# SOURCE UI AND SERVER COMPONENTS
# ==========================================

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

# ==========================================
# RUN APP
# ==========================================

shinyApp(ui = ui, server = server)
