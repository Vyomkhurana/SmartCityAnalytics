# Smart City Data Analytics System

An R-based analytics and dashboard project for city-scale monitoring of traffic, air quality, and energy demand.

## Overview

The project provides:
1. Data preprocessing for real-world city datasets
2. Exploratory analysis and visualization
3. Predictive modeling for traffic, AQI, and energy
4. Interactive Shiny dashboard with filtering and scenario prediction
5. Dataset upload workflow to analyze user-provided CSVs

## Repository Structure

```text
SmartCityAnalytics/
	data/
		raw/
			traffic_data.csv
			air_quality_data.csv
			energy_data.csv
			weather_data.csv
		processed/
	scripts/
		00_preprocess_real_data.py
		01_data_preprocessing.R
		02_exploratory_analysis.R
		03_visualization.R
		04_predictive_models.R
		05_anomaly_detection.R
		06_clustering_analysis.R
	models/
	outputs/
	shiny_app/
		app.R
		ui.R
		server.R
		settings.R
	run_pipeline.R
	run_project.ps1
	setup.R
	README.md
```

## Prerequisites

1. R 4.0+
2. Python 3.9+ (for `scripts/00_preprocess_real_data.py`)
3. Required R packages installed through `setup.R`

## Quick Start

1. Install R dependencies

```r
source("setup.R")
```

2. (Optional) Normalize source files into the expected schema

```bash
python scripts/00_preprocess_real_data.py
```

3. Run end-to-end R pipeline

```r
source("run_pipeline.R")
```

4. Launch dashboard

```r
shiny::runApp("shiny_app")
```

## PowerShell Runner

Use the launcher for setup, pipeline, and optional dashboard:

```powershell
.\run_project.ps1
```

Common options:

```powershell
.\run_project.ps1 -LaunchDashboard
.\run_project.ps1 -SkipSetup
.\run_project.ps1 -SkipSetup -SkipPipeline -LaunchDashboard
```

## Dashboard Capabilities

1. Modular tabs for Overview, Traffic, Air Quality, Energy, Predictions, and Data Explorer
2. Search-button based filtering by date range, zone/station/building type, and Delhi area
3. Configurable synthetic augmentation (via `shiny_app/settings.R`)
4. Prediction panel with model-safe input handling and fallbacks
5. Data Explorer with exportable tables

## New Feature: Upload Your Own Datasets

In the Data Explorer tab, you can upload one or more CSV files and apply them directly to the dashboard.

Flow:
1. Upload CSV file(s)
2. Click `Apply Uploaded Dataset(s)`
3. Dashboard validates schema and reloads
4. All analytics use the uploaded data after reload

Required columns by dataset:

1. Master Data CSV
	 - `timestamp`, `date`, `total_vehicles`, `avg_AQI`, `total_energy_kwh`
2. Traffic CSV
	 - `timestamp`, `zone`, `hour`, `vehicle_count`, `average_speed`, `congestion_level`
3. Air Quality CSV
	 - `timestamp`, `station_id`, `hour`, `AQI`, `PM25`, `PM10`, `NO2`, `O3`
4. Energy CSV
	 - `timestamp`, `building_type`, `hour`, `energy_consumption_kwh`, `renewable_percent`, `cost_usd`

Validation behavior:
1. Missing required columns are reported with a clear error message
2. Valid files are persisted into `data/processed/`
3. App reload is automatic after successful apply

## Data and Outputs

Generated artifacts are saved under:
1. `data/processed/` for cleaned datasets
2. `outputs/plots/` for generated figures
3. `models/` for trained model files
4. `outputs/model_results.rds` for evaluation summaries

## Configuration

Project-level app assumptions are centralized in:
1. `shiny_app/settings.R`

You can tune:
1. Delhi area list
2. Synthetic data toggle and parameters (sample fraction, noise, time shift, seeds)
3. Prediction fallback settings

## Notes for Academic Submission

1. Keep dataset provenance documented (source, city, period)
2. Include key visual evidence from `outputs/plots/`
3. Use `CITY_REPORT_TEMPLATE.md` to structure final reporting

## License and Ownership

Use according to your institution/project requirements.
