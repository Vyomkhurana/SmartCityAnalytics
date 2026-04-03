#!/usr/bin/env python3
"""
Smart City Analytics - Real Data Preprocessing Adapter
Converts real government data formats to standard pipeline schema
Handles: Traffic time-aggregation, Excel air quality, Regional energy, NOAA ISD weather
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import os
import sys

print("=" * 60)
print("PREPROCESSING: Real Data Format Adapter")
print("=" * 60)
print()

# ==========================================
# 1. CONFIGURATION
# ==========================================
raw_data_dir = "data/raw"
processed_data_dir = "data/processed"
os.makedirs(processed_data_dir, exist_ok=True)

# ==========================================
# 2. TRAFFIC DATA PARSER: Time-of-day × Weekday → Hourly timestamps
# ==========================================
def parse_traffic_time_weekday(filepath):
    """Expand traffic time × weekday matrix to hourly timestamps"""
    print("Parsing traffic data (time × weekday format)...")
    
    traffic = pd.read_csv(filepath)
    
    # Verify format: "Time" column + 7 weekday columns
    if traffic.shape[1] != 8 or "Time" not in traffic.columns:
        raise ValueError(f"Traffic file format error. Expected 'Time' and 7 weekdays, got {traffic.columns.tolist()}")
    
    weekdays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
    
    # Convert weekday columns: remove '%' and convert to float
    for day in weekdays:
        traffic[day] = traffic[day].astype(str).str.rstrip('%').astype(float)
    
    # Reshape: each row becomes 7 rows (one per weekday)
    traffic_long = traffic.melt(
        id_vars=["Time"],
        value_vars=weekdays,
        var_name="weekday",
        value_name="congestion_pct"
    )
    
    # Map times to hours: "12:00 AM" = 0, "02:00 AM" = 2, etc.
    time_map = {
        "12:00 AM": 0, "02:00 AM": 2, "04:00 AM": 4, "06:00 AM": 6,
        "08:00 AM": 8, "10:00 AM": 10, "12:00 PM": 12, "02:00 PM": 14,
        "04:00 PM": 16, "06:00 PM": 18, "08:00 PM": 20, "10:00 PM": 22
    }
    traffic_long["hour"] = traffic_long["Time"].map(time_map)
    
    # Generate date range: 2025-2027 (3 years)
    start_date = pd.Timestamp("2025-01-01")
    end_date = pd.Timestamp("2027-12-31")
    all_dates = pd.date_range(start=start_date, end=end_date, freq="D")
    
    # Create date-weekday DF with proper day name
    dates_df = pd.DataFrame({
        "date": all_dates,
        "day_of_week": all_dates.day_name()
    })
    
    # Expand: create all date_hour combinations
    traffic_expanded_list = []
    for day_idx, day_name in enumerate(weekdays):
        # Get all dates matching this weekday
        matching_dates = dates_df[dates_df["day_of_week"] == day_name]["date"].values
        # Get traffic data for this weekday
        day_traffic = traffic_long[traffic_long["weekday"] == day_name].copy()
        
        for date in matching_dates:
            for _, row in day_traffic.iterrows():
                traffic_expanded_list.append({
                    "timestamp": pd.Timestamp(date).replace(hour=int(row["hour"])),
                    "zone": "Delhi",
                    "vehicle_count": row["congestion_pct"] * 100,  # Scale % to count
                    "average_speed": 30 - (row["congestion_pct"] * 0.2),  # Inverse relationship
                    "congestion_level": "Low" if row["congestion_pct"] < 20 else ("Medium" if row["congestion_pct"] < 50 else "High")
                })
    
    traffic_df = pd.DataFrame(traffic_expanded_list)
    print(f"  [OK] Expanded to {len(traffic_df)} hourly records")
    return traffic_df

# ==========================================
# 3. AIR QUALITY PARSER: Excel (CPCB format) or CSV
# ==========================================
def parse_air_quality(filepath):
    """Read air quality from Excel (CPCB workbook) or CSV"""
    print(f"Parsing air quality data ({filepath})...")
    
    if filepath.endswith('.xlsx') or filepath.endswith('.xls'):
        # Excel: read, skip metadata rows
        aqi_df = pd.read_excel(filepath, sheet_name=0, skiprows=5)
    else:
        # CSV - CPCB format has metadata rows, find actual header row
        with open(filepath, 'r') as f:
            for i, line in enumerate(f):
                if 'From Date' in line:
                    skiprows = i
                    break
        aqi_df = pd.read_csv(filepath, skiprows=skiprows)
    
    # Standardize column names
    aqi_df.columns = aqi_df.columns.str.lower().str.strip()
    
    # Map common column names (handle CPCB variants)
    col_map = {
        "from date": "timestamp",
        "date time": "timestamp",
        "datetime": "timestamp",
        "date": "timestamp",
        "pm2.5": "PM25",
        "pm 2.5": "PM25",
        "pm25": "PM25",
        "pm 10": "PM10",
        "pm10": "PM10",
        "no2": "NO2",
        "no 2": "NO2",
        "o3": "O3",
        "ozone": "O3",
        "aqi": "AQI",
        "station": "station_id",
        "stationname": "station_id",
        "temperature": "temperature",
        "humidity": "humidity"
    }
    
    for old_name, new_name in col_map.items():
        for col in aqi_df.columns:
            if col.lower() == old_name:
                aqi_df.rename(columns={col: new_name}, inplace=True)
                break
    
    # Ensure timestamp is datetime
    if "timestamp" in aqi_df.columns:
        aqi_df["timestamp"] = pd.to_datetime(aqi_df["timestamp"], errors="coerce")
    else:
        aqi_df["timestamp"] = datetime.now()
    
    # Fill missing columns with defaults
    if "station_id" not in aqi_df.columns:
        aqi_df["station_id"] = "Delhi_CPCB_01"
    
    required_cols = ["PM25", "PM10", "NO2", "O3", "temperature", "humidity"]
    for col in required_cols:
        if col not in aqi_df.columns:
            aqi_df[col] = np.nan
    
    # Calculate AQI if not present (simple average of pollutants)
    if "AQI" not in aqi_df.columns:
        pollutant_cols = [c for c in ["PM25", "PM10", "NO2", "O3"] if c in aqi_df.columns]
        aqi_df["AQI"] = aqi_df[pollutant_cols].mean(axis=1)
    
    # Create AQI category
    def get_aqi_category(aqi):
        if pd.isna(aqi):
            return "Unknown"
        elif aqi <= 50:
            return "Good"
        elif aqi <= 100:
            return "Satisfactory"
        elif aqi <= 150:
            return "Moderately Polluted"
        elif aqi <= 200:
            return "Poor"
        elif aqi <= 300:
            return "Very Poor"
        else:
            return "Severe"
    
    aqi_df["AQI_category"] = aqi_df["AQI"].apply(get_aqi_category)
    
    # Select final columns
    final_cols = ["timestamp", "station_id", "PM25", "PM10", "NO2", "O3", 
                  "temperature", "humidity", "AQI", "AQI_category"]
    aqi_df = aqi_df[[c for c in final_cols if c in aqi_df.columns]]
    
    # Remove rows with null timestamps
    aqi_df = aqi_df[aqi_df["timestamp"].notna()]
    
    print(f"  [OK] Parsed {len(aqi_df)} air quality records")
    return aqi_df

# ==========================================
# 4. ENERGY PARSER: Regional Grid Data (Grid-India format)
# ==========================================
def parse_energy_regional(filepath):
    """Parse regional grid energy data (Northern Region for Delhi)"""
    print("Parsing energy data (regional grid format)...")
    
    energy_raw = pd.read_csv(filepath)
    
    # Standardize column names
    energy_raw.columns = energy_raw.columns.str.strip()
    
    # Ensure date/timestamp column
    if "date" in energy_raw.columns:
        energy_raw["timestamp"] = pd.to_datetime(energy_raw["date"], errors="coerce")
    elif "Date" in energy_raw.columns:
        energy_raw["timestamp"] = pd.to_datetime(energy_raw["Date"], errors="coerce")
    else:
        energy_raw["timestamp"] = datetime.now()
    
    # Filter for meaningful data (Total or regional aggregate)
    energy_clean = energy_raw[
        energy_raw["source"].str.lower().isin(["total", "hydro", "wind gen(mu)"])
    ].copy()
    
    # Extract Northern Region (Delhi) energy
    # Use "NR" column if available, else "All India" with scaling
    if "NR" in energy_clean.columns:
        energy_clean["energy_kwh"] = energy_clean["NR"] * 1000  # Convert MU to kWh
    else:
        energy_clean["energy_kwh"] = energy_clean["All India"] * 100  # Scale down
    
    # Create standard schema
    energy_processed = pd.DataFrame({
        "timestamp": energy_clean["timestamp"],
        "building_type": "Regional_Grid",
        "energy_consumption_kwh": energy_clean["energy_kwh"],
        "peak_demand_kw": energy_clean["energy_kwh"] * 0.7,  # Estimate peak
        "renewable_percent": 15,  # Estimated for India grid
        "cost_usd": energy_clean["energy_kwh"] * 0.08  # $/kWh estimate
    })
    
    # Remove null timestamps
    energy_processed = energy_processed[energy_processed["timestamp"].notna()]
    
    print(f"  [OK] Parsed {len(energy_processed)} energy records (Delhi/NR)")
    return energy_processed

# ==========================================
# 5. WEATHER PARSER: NOAA ISD FM-12 Encoded Format
# ==========================================
def parse_noaa_isd(filepath):
    """Parse NOAA ISD FM-12 encoded weather data"""
    print("Parsing weather data (NOAA ISD FM-12 format)...")
    
    weather_raw = pd.read_csv(filepath)
    
    # Extract datetime
    weather_raw["timestamp"] = pd.to_datetime(weather_raw["DATE"], errors="coerce")
    
    # Parse ISD encoded fields
    def extract_isd_value(field, decimals=0):
        """Extract numeric value from ISD encoded field (e.g., '+0074,1' → 7.4)"""
        if pd.isna(field) or field == "":
            return np.nan
        
        parts = str(field).split(",")
        if len(parts) == 0:
            return np.nan
        
        try:
            value = float(parts[0])
            # Check for missing value codes
            if value >= 9999 or value <= -9999:
                return np.nan
            # Apply decimals
            if decimals > 0:
                value = value / (10 ** decimals)
            return value
        except:
            return np.nan
    
    # Extract temperature (TMP column: "+0074,1" format)
    weather_raw["temperature"] = weather_raw["TMP"].apply(lambda x: extract_isd_value(x, decimals=1))
    
    # Extract wind speed from WND column (direction,quality,type,speed,...)
    def extract_wind_speed(wnd_field):
        if pd.isna(wnd_field):
            return np.nan
        parts = str(wnd_field).split(",")
        if len(parts) >= 4:
            try:
                speed_ms = float(parts[3]) / 10  # In m/s
                return speed_ms * 3.6  # Convert to km/h
            except:
                return np.nan
        return np.nan
    
    weather_raw["wind_speed"] = weather_raw["WND"].apply(extract_wind_speed)
    
    # Extract pressure (SLP column)
    weather_raw["pressure_hpa"] = weather_raw["SLP"].apply(lambda x: extract_isd_value(x, decimals=1))
    
    # Default values for missing/complex fields
    weather_raw["humidity"] = 50  # DEW point available but RH requires more data
    weather_raw["precipitation_mm"] = 0  # Requires additional parsing of AA1 field
    weather_raw["condition"] = "Varied"
    
    # Select and clean final columns
    weather_processed = weather_raw[[
        "timestamp", "temperature", "humidity", "wind_speed", 
        "precipitation_mm", "pressure_hpa", "condition"
    ]].copy()
    
    # Remove null timestamps
    weather_processed = weather_processed[weather_processed["timestamp"].notna()]
    
    # Fill missing values with defaults
    weather_processed["temperature"] = weather_processed["temperature"].fillna(
        weather_processed["temperature"].mean()
    )
    weather_processed["pressure_hpa"] = weather_processed["pressure_hpa"].fillna(1013)
    weather_processed["wind_speed"] = weather_processed["wind_speed"].fillna(0)
    weather_processed["humidity"] = weather_processed["humidity"].fillna(50)
    
    # Validate ranges
    weather_processed = weather_processed[
        (weather_processed["temperature"] >= -50) & 
        (weather_processed["temperature"] <= 60) &
        (weather_processed["humidity"] >= 0) & 
        (weather_processed["humidity"] <= 100)
    ]
    
    print(f"  [OK] Parsed {len(weather_processed)} weather records (NOAA Delhi)")
    return weather_processed

# ==========================================
# 6. MAIN: Load, Process, and Save
# ==========================================
def main():
    try:
        print()
        print("Loading and processing real data files...")
        print()
        
        # Detect air quality file format (CSV or Excel)
        aqi_csv = os.path.join(raw_data_dir, "air_quality_data.csv")
        aqi_xlsx = os.path.join(raw_data_dir, "air_quality_data.xlsx")
        aqi_file = aqi_csv if os.path.exists(aqi_csv) else aqi_xlsx
        
        # Load all datasets
        traffic_df = parse_traffic_time_weekday(os.path.join(raw_data_dir, "traffic_data.csv"))
        air_quality_df = parse_air_quality(aqi_file)
        energy_df = parse_energy_regional(os.path.join(raw_data_dir, "energy_data.csv"))
        weather_df = parse_noaa_isd(os.path.join(raw_data_dir, "weather_data.csv"))
        
        print()
        print("=" * 60)
        print("Saving processed datasets...")
        print("=" * 60)
        print()
        
        # Save individual datasets
        traffic_df.to_csv(os.path.join(processed_data_dir, "traffic_clean.csv"), index=False)
        print(f"[OK] traffic_clean.csv ({len(traffic_df)} rows)")
        
        air_quality_df.to_csv(os.path.join(processed_data_dir, "air_quality_clean.csv"), index=False)
        print(f"[OK] air_quality_clean.csv ({len(air_quality_df)} rows)")
        
        energy_df.to_csv(os.path.join(processed_data_dir, "energy_clean.csv"), index=False)
        print(f"[OK] energy_clean.csv ({len(energy_df)} rows)")
        
        weather_df.to_csv(os.path.join(processed_data_dir, "weather_clean.csv"), index=False)
        print(f"[OK] weather_clean.csv ({len(weather_df)} rows)")
        
        print()
        print("=" * 60)
        print("PREPROCESSING COMPLETE")
        print("=" * 60)
        print()
        print("Data Summary:")
        print(f"  Traffic:      {len(traffic_df)} records | {traffic_df['timestamp'].min()} to {traffic_df['timestamp'].max()}")
        print(f"  Air Quality:  {len(air_quality_df)} records | {air_quality_df['timestamp'].min()} to {air_quality_df['timestamp'].max()}")
        print(f"  Energy:       {len(energy_df)} records | {energy_df['timestamp'].min()} to {energy_df['timestamp'].max()}")
        print(f"  Weather:      {len(weather_df)} records | {weather_df['timestamp'].min()} to {weather_df['timestamp'].max()}")
        print()
        print("Next steps:")
        print("  1. Review cleaned data in data/processed/")
        print("  2. Run exploratory analysis: scripts/02_exploratory_analysis.R")
        print("  3. View dashboard: shiny_app/app.R")
        print()
        
    except Exception as e:
        print(f"\n[ERROR] {str(e)}") 
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()
