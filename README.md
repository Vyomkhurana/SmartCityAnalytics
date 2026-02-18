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

The system uses the following data sources:
- Traffic flow/congestion data
- Air pollution (AQI, PM2.5, PM10, NO2)
- Energy consumption
- Weather data (optional)
- Public transport data (optional)

## Requirements

- R >= 4.0
- See `setup.R` for complete package list

## Author

Data Science Project - Smart City Analytics
Date: February 2026
