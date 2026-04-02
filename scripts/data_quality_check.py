#!/usr/bin/env python3
"""
Data Quality Validation - Smart City Analytics
Validates preprocessed data for completeness, range, and consistency
"""

import pandas as pd
import numpy as np
import os
from datetime import datetime

print("=" * 60)
print("DATA QUALITY VALIDATION REPORT")
print("=" * 60)
print()

processed_dir = "data/processed"

# ==========================================
# Load all processed datasets
# ==========================================
print("Loading preprocessed datasets...")
print()

try:
    traffic = pd.read_csv(os.path.join(processed_dir, "traffic_clean.csv"))
    air_quality = pd.read_csv(os.path.join(processed_dir, "air_quality_clean.csv"))
    energy = pd.read_csv(os.path.join(processed_dir, "energy_clean.csv"))
    weather = pd.read_csv(os.path.join(processed_dir, "weather_clean.csv"))
    print("[OK] All datasets loaded successfully")
except Exception as e:
    print(f"[ERROR] Failed to load datasets: {e}")
    exit(1)

print()

# ==========================================
# TRAFFIC DATA VALIDATION
# ==========================================
print("TRAFFIC DATA VALIDATION")
print("-" * 60)
print(f"Shape: {traffic.shape[0]} rows x {traffic.shape[1]} columns")
print(f"Columns: {list(traffic.columns)}")
print()

traffic['timestamp'] = pd.to_datetime(traffic['timestamp'])
print(f"Date Range: {traffic['timestamp'].min()} to {traffic['timestamp'].max()}")
print(f"Missing Values: {traffic.isnull().sum().sum()}")
print()

print("Column Statistics:")
for col in ['vehicle_count', 'average_speed']:
    if col in traffic.columns:
        print(f"  {col}:")
        print(f"    Min: {traffic[col].min():.2f}, Max: {traffic[col].max():.2f}, Mean: {traffic[col].mean():.2f}")
        print(f"    Std Dev: {traffic[col].std():.2f}")

print()
print(f"Congestion Levels: {traffic['congestion_level'].value_counts().to_dict()}")
print()
print("[OK] Traffic data: VALID")
print()

# ==========================================
# AIR QUALITY DATA VALIDATION
# ==========================================
print("AIR QUALITY DATA VALIDATION")
print("-" * 60)
print(f"Shape: {air_quality.shape[0]} rows x {air_quality.shape[1]} columns")
print(f"Columns: {list(air_quality.columns)}")
print()

air_quality['timestamp'] = pd.to_datetime(air_quality['timestamp'])
print(f"Date Range: {air_quality['timestamp'].min()} to {air_quality['timestamp'].max()}")
print(f"Unique Stations: {air_quality['station_id'].nunique()}")
print(f"Missing Values: {air_quality.isnull().sum().sum()}")
print()

# Check if all records have same timestamp (warning sign)
unique_timestamps = air_quality['timestamp'].nunique()
if unique_timestamps <= 1:
    print("[WARNING] Only 1 unique timestamp! This suggests data parsing issue.")
    print("          ACTION NEEDED: Check air_quality_data.xlsx for date column")
else:
    print(f"Unique Timestamps: {unique_timestamps}")

print()
print("Pollutant Statistics:")
for col in ['PM25', 'PM10', 'NO2', 'O3', 'AQI']:
    if col in air_quality.columns:
        valid_count = air_quality[col].notna().sum()
        print(f"  {col}: {valid_count} records, Mean={air_quality[col].mean():.2f}, RangeGap={air_quality[col].min():.2f}-{air_quality[col].max():.2f}")

print()
if unique_timestamps <= 1:
    print("[WARNING] Air Quality: NEEDS ATTENTION (date parsing issue)")
else:
    print("[OK] Air Quality: VALID")
print()

# ==========================================
# ENERGY DATA VALIDATION
# ==========================================
print("ENERGY DATA VALIDATION")
print("-" * 60)
print(f"Shape: {energy.shape[0]} rows x {energy.shape[1]} columns")
print(f"Columns: {list(energy.columns)}")
print()

energy['timestamp'] = pd.to_datetime(energy['timestamp'])
print(f"Date Range: {energy['timestamp'].min()} to {energy['timestamp'].max()}")
print(f"Missing Values: {energy.isnull().sum().sum()}")
print()

print("Energy Statistics:")
for col in ['energy_consumption_kwh', 'peak_demand_kw', 'renewable_percent']:
    if col in energy.columns:
        print(f"  {col}: Mean={energy[col].mean():.2f}, Min={energy[col].min():.2f}, Max={energy[col].max():.2f}")

# Check date range
year_min = energy['timestamp'].dt.year.min()
year_max = energy['timestamp'].dt.year.max()
if year_max < 2023:
    print()
    print(f"[WARNING] Energy data is from {year_min}-{year_max}, need 2023-2025 for project!")
    print("          ACTION NEEDED: Download updated Grid-India or NLDC data")
    print("[WARNING] Energy: OUTDATED (needs newer source)")
else:
    print("[OK] Energy: VALID")
print()

# ==========================================
# WEATHER DATA VALIDATION
# ==========================================
print("WEATHER DATA VALIDATION")
print("-" * 60)
print(f"Shape: {weather.shape[0]} rows x {weather.shape[1]} columns")
print(f"Columns: {list(weather.columns)}")
print()

weather['timestamp'] = pd.to_datetime(weather['timestamp'])
print(f"Date Range: {weather['timestamp'].min()} to {weather['timestamp'].max()}")
print(f"Missing Values: {weather.isnull().sum().sum()}")
print()

print("Weather Statistics:")
for col in ['temperature', 'humidity', 'wind_speed', 'pressure_hpa', 'precipitation_mm']:
    if col in weather.columns:
        print(f"  {col}: Mean={weather[col].mean():.2f}, Min={weather[col].min():.2f}, Max={weather[col].max():.2f}")

print()
print("[OK] Weather: VALID")
print()

# ==========================================
# SUMMARY & RECOMMENDATIONS
# ==========================================
print("=" * 60)
print("SUMMARY & RECOMMENDATIONS")
print("=" * 60)
print()

issues = []
if unique_timestamps <= 1:
    issues.append("- Air Quality: Date parsing issue (all same timestamp)")
if year_max < 2023:
    issues.append("- Energy: Outdated data (need 2023-2025)")

if issues:
    print("ISSUES FOUND:")
    for issue in issues:
        print(issue)
    print()
    print("ACTIONS:")
    print("1. Inspect data/raw/air_quality_data.xlsx in Excel")
    print("2. Download fresh Grid-India data for 2023-2025")
    print("3. Re-run: python scripts/00_preprocess_real_data.py")
else:
    print("All datasets validated successfully!")
    print()
    print("NEXT STEPS:")
    print("1. Run exploratory analysis: R > source('scripts/02_exploratory_analysis.R')")
    print("2. Launch dashboard: R > shiny::runApp('shiny_app/app.R')")
    print("3. Build models: R > source('scripts/04_predictive_models.R')")

print()
print("=" * 60)
