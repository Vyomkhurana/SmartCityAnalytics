# Smart City Data Analytics System

A comprehensive R-based data analytics system for smart city monitoring and prediction, featuring traffic analysis, air quality monitoring, and energy consumption patterns.

## Project Structure

```
SmartCityAnalytics/
├── data/
│   ├── raw/              # Original datasets
│   └── processed/        # Cleaned and merged data
├── scripts/
│   ├── 01_data_preprocessing.R
│   ├── 02_exploratory_analysis.R
│   ├── 03_visualization.R
│   ├── 04_predictive_models.R
│   ├── 05_anomaly_detection.R
│   └── 06_clustering_analysis.R
├── models/               # Saved prediction models
├── shiny_app/
│   ├── app.R            # Main Shiny dashboard
│   ├── ui.R             # UI components
│   └── server.R         # Server logic
├── outputs/              # Generated reports and plots
└── setup.R              # Install required packages
```

## Features

### Data Analysis
- Traffic flow and congestion analysis
- Air quality index (AQI) monitoring
- Energy consumption patterns
- Multi-dataset integration

### Visualizations
- Interactive plotly charts
- Leaflet city maps
- ggplot2 statistical graphs
- Heatmaps for congestion zones

### Predictive Models
- Traffic congestion forecasting
- AQI prediction
- Energy demand estimation

### Anomaly Detection
- Z-score based anomaly detection
- IQR-based outlier identification
- Time-based pattern anomalies
- Automated anomaly reports

### Clustering Analysis
- Hourly pattern clustering (night, rush hours, midday)
- Daily activity level classification
- Weekday hierarchical clustering
- Principal Component Analysis (PCA)
- Multi-metric correlation heatmaps

### Shiny Dashboard
- Interactive web interface
- Multiple analysis tabs
- Real-time predictions
- Downloadable reports

## Installation

1. Install R (version 4.0 or higher)
2. Open R or RStudio
3. Run the setup script:

```r
source("setup.R")
```

## Usage

### Run Data Pipeline

Run the complete workflow in one command:

```r
source("run_pipeline.R")
```

On Windows PowerShell, use the launcher script for setup + pipeline + optional dashboard:

```powershell
.\run_project.ps1
```

Common options:

```powershell
# Run setup + pipeline + launch dashboard
.\run_project.ps1 -LaunchDashboard

# Skip package installation (faster reruns)
.\run_project.ps1 -SkipSetup

# Launch dashboard only (after artifacts already exist)
.\run_project.ps1 -SkipSetup -SkipPipeline -LaunchDashboard
```

Or run each step manually:

```r
# 1. Preprocess data
source("scripts/01_data_preprocessing.R")

# 2. Exploratory analysis
source("scripts/02_exploratory_analysis.R")

# 3. Generate visualizations
source("scripts/03_visualization.R")

# 4. Train predictive models
source("scripts/04_predictive_models.R")

# 5. Run anomaly detection
source("scripts/05_anomaly_detection.R")

# 6. Clustering analysis
source("scripts/06_clustering_analysis.R")
```

### Launch Shiny Dashboard

```r
library(shiny)
runApp("shiny_app")
```

Or from terminal:
```bash
R -e "shiny::runApp('shiny_app')"
```

## Datasets

This project is intended for real city data. Place the following files in `data/raw/`:
- `traffic_data.csv`
- `air_quality_data.csv`
- `energy_data.csv`
- `weather_data.csv`

Recommended submission metadata (for teacher review):
- City name
- Data collection period
- Data source links (municipal portal, national open data, sensor APIs)

Expected content by file:
- `traffic_data.csv`: timestamp, zone, vehicle_count, average_speed, congestion_level
- `air_quality_data.csv`: timestamp, station_id, PM25, PM10, NO2, O3, AQI, AQI_category, temperature, humidity
- `energy_data.csv`: timestamp, building_type, energy_consumption_kwh, peak_demand_kw, renewable_percent, cost_usd
- `weather_data.csv`: timestamp, temperature, humidity, wind_speed, precipitation_mm, pressure_hpa, condition

## Requirements

- R >= 4.0
- See `setup.R` for complete package list

## Teacher Submission Guidance

Use `CITY_REPORT_TEMPLATE.md` to document:
- which real city you analyzed,
- where the data came from,
- and how each recommendation is supported by output evidence.

## Author

Data Science Project - Smart City Analytics
Date: February 2026
