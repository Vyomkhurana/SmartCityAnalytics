# Smart City Data Analytics System

A comprehensive R-based data analytics system for smart city monitoring and prediction, featuring traffic analysis, air quality monitoring, and energy consumption patterns. Now updated with **real government data** from Delhi NCR!

## Project Overview

This project analyzes real-world smart city data from Delhi NCR (Jan 2023 - Dec 2025 projections) including:
- 🚗 **Traffic Data** - Hourly congestion patterns (% utilization by time-of-day)
- 💨 **Air Quality** - Pollutant levels from CPCB monitoring stations
- ⚡ **Energy** - Regional grid demand and renewable energy mix
- 🌡️ **Weather** - Temperature, humidity, wind speed from NOAA weather stations

## Project Structure

```
SmartCityAnalytics/
├── data/
│   ├── raw/              # Real government datasets
│   │   ├── traffic_data.csv          # Time × Weekday congestion matrix
│   │   ├── air_quality_data.xlsx     # CPCB Excel workbook
│   │   ├── energy_data.csv           # Grid-India regional data
│   │   └── weather_data.csv          # NOAA ISD station data
│   └── processed/        # Cleaned and standardized data
├── scripts/
│   ├── 00_preprocess_real_data.py    # [NEW] Real data format adapter
│   ├── 01_data_preprocessing.R       # [UPDATED] Enhanced for real data
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
├── setup.R              # Install required packages
└── REAL_DATA_ENHANCEMENT_SUMMARY.md  # [NEW] Complete enhancement guide
```

## 🚀 Quick Start

### 1. Setup Environment
```bash
# Install R packages
R
source('setup.R')
```

### 2. Preprocess Real Data
```bash
# Convert real government data to standard schema
python scripts/00_preprocess_real_data.py
```

This script:
- ✅ Expands traffic time-of-day × weekday matrix to hourly timestamps
- ✅ Parses CPCB Excel air quality workbooks
- ✅ Extracts Delhi-specific data from regional grid data
- ✅ Decodes NOAA ISD FM-12 weather format

### 3. Run Analysis Pipeline
```bash
R
source('scripts/02_exploratory_analysis.R')  # Explore patterns
source('scripts/03_visualization.R')         # Generate charts
source('scripts/04_predictive_models.R')     # Build models
```

### 4. Launch Interactive Dashboard
```bash
R
shiny::runApp('shiny_app/app.R')
```

Opens at `http://localhost:3838`

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

## Data Sources

All data is sourced from **official government/scientific repositories**:

| Dataset | Source | Format | Time Range | Update Frequency |
|---------|--------|--------|-----------|-----------------|
| **Traffic** | Delhi Traffic Smart-City Analytics | Time × Weekday Matrix | Aggregated patterns | Baseline data |
| **Air Quality** | CPCB (Central Pollution Control Board) | Excel workbooks (.xlsx) | Varies by download | Hourly readings |
| **Energy** | Grid-India / NLDC | CSV exports | 2013-2023 (historical) | Daily aggregates |
| **Weather** | NOAA ISD Station (Safdarjung, Delhi) | FM-12 encoded CSV | 2023 + ongoing | Every 3 hours |

### Data Files
- ✅ **traffic_data.csv**: Time-of-day × Days-of-week congestion percentages
- ✅ **air_quality_data.xlsx**: Station-level PM2.5, PM10, NO2, O3, AQI readings
- ✅ **energy_data.csv**: Regional grid: Northern (NR), Western (WR), Southern (SR), Eastern (ER) regions
- ✅ **weather_data.csv**: NOAA weather observations with encoded fields (temperature, wind, pressure)

**Note on Data Dates:**
- Traffic: Expanded to 2025-2027 time periods
- Weather: Real data from 2023
- Energy: Currently 2013-2023; newer 2023-2025 data recommended
- Air Quality: Date range depends on downloaded workbook

See `REAL_DATA_ENHANCEMENT_SUMMARY.md` for detailed data transformation documentation.

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
