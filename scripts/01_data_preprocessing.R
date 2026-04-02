# Smart City Analytics - Data Preprocessing Script
# Flexible preprocessing for REAL DATA with various formats
# Handles: Traffic time-aggregation, Excel air quality, Regional energy, NOAA ISD weather

cat("==================================================\n")
cat("STEP 1: Data Preprocessing (Real Data Adapter)\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(readxl)

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# ==========================================
# 1. VALIDATE REAL INPUT DATA
# ==========================================
required_raw_files <- c(
  "data/raw/traffic_data.csv",
  "data/raw/energy_data.csv",
  "data/raw/weather_data.csv"
)

# Air quality can be CSV or Excel
air_quality_file <- if (file.exists("data/raw/air_quality_data.xlsx")) {
  "data/raw/air_quality_data.xlsx"
} else if (file.exists("data/raw/air_quality_data.csv")) {
  "data/raw/air_quality_data.csv"
} else {
  NA
}

if (is.na(air_quality_file)) {
  required_raw_files <- c(required_raw_files, "data/raw/air_quality_data.xlsx OR data/raw/air_quality_data.csv")
}

missing_raw_files <- required_raw_files[!file.exists(required_raw_files) & !is.na(required_raw_files)]
if (length(missing_raw_files) > 0) {
  stop(
    paste0(
      "Missing required real-city input files:\n- ",
      paste(missing_raw_files, collapse = "\n- "),
      "\n\nPlace real datasets in data/raw before running preprocessing."
    )
  )
}

# ==========================================
# 2. HELPER FUNCTIONS FOR REAL DATA PARSING
# ==========================================

# Parse traffic data: Time-of-day × Weekday format → Hourly timestamps
parse_traffic_timeweekday <- function(file_path) {
  traffic <- fread(file_path)
  
  # Verify format: first column "Time", then 7 weekday columns
  if (ncol(traffic) != 8 || names(traffic)[1] != "Time") {
    # Try as-is if already in expected format
    if ("timestamp" %in% names(traffic)) return(traffic)
    stop("Traffic file format not recognized. Expected 'Time' column + 7 weekday columns or 'timestamp' column")
  }
  
  weekdays_cols <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  
  # Expand to long format: each row becomes 7 rows (one per weekday)
  traffic_long <- traffic %>%
    pivot_longer(
      cols = all_of(weekdays_cols),
      names_to = "weekday",
      values_to = "congestion_pct"
    ) %>%
    mutate(
      weekday = factor(weekday, levels = weekdays_cols),
      congestion_pct = as.numeric(gsub("%", "", congestion_pct))
    )
  
  # Create timestamps for future dates (generate 3 years: 2025-2027 starting from Jan 1)
  start_date <- as.Date("2025-01-01")
  end_date <- as.Date("2027-12-31")
  
  # Create date sequence matching weekdays
  all_dates <- seq(start_date, end_date, by = "day")
  dates_by_weekday <- data.frame(
    date = all_dates,
    weekday = factor(lubridate::wday(all_dates, label = TRUE, abbr = FALSE), levels = weekdays_cols)
  )
  
  # Match times to hours (12:00 AM = 0, 02:00 AM = 2, etc.)
  time_to_hour_map <- c(
    "12:00 AM" = 0, "02:00 AM" = 2, "04:00 AM" = 4, "06:00 AM" = 6,
    "08:00 AM" = 8, "10:00 AM" = 10, "12:00 PM" = 12, "02:00 PM" = 14,
    "04:00 PM" = 16, "06:00 PM" = 18, "08:00 PM" = 20, "10:00 PM" = 22
  )
  
  traffic_expanded <- crossing(dates_by_weekday, time_data = traffic_long %>% distinct(Time, weekday)) %>%
    left_join(traffic_long %>% rename(time_data = Time), by = "weekday") %>%
    mutate(
      hour = time_to_hour_map[time_data],
      timestamp = as.POSIXct(paste(date, hour, "00:00"), format = "%Y-%m-%d %H %M:%S"),
      zone = "Delhi",
      vehicle_count = congestion_pct * 100,  # Scale percentage to approx vehicle count
      average_speed = 30 - (congestion_pct * 0.2),  # Inverse relationship with congestion
      congestion_level = cut(congestion_pct, 
                             breaks = c(0, 20, 50, 100), 
                             labels = c("Low", "Medium", "High")),
      congestion_pct = NULL,
      time_data = NULL
    ) %>%
    select(timestamp, zone, vehicle_count, average_speed, congestion_level)
  
  return(traffic_expanded)
}

# Parse air quality data: Excel (CPCB format) or CSV
parse_air_quality <- function(file_path) {
  if (grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
    # Excel: Skip metadata rows, read as data
    tryCatch({
      aqi <- read_excel(file_path, sheet = 1, skip = 5, col_names = TRUE)
      
      # Standardize column names if needed
      names(aqi) <- tolower(names(aqi))
      
      # Map common CPCB column names
      if ("date time" %in% names(aqi)) {
        aqi <- aqi %>% rename(timestamp = "date time")
      } else if ("datetime" %in% names(aqi)) {
        aqi <- aqi %>% rename(timestamp = datetime)
      }
      
      # Ensure timestamp is POSIXct
      if (!inherits(aqi$timestamp, "POSIXct")) {
        aqi$timestamp <- as.POSIXct(aqi$timestamp, format = "%Y-%m-%d %H:%M:%S", 
                                     tz = "UTC")
      }
      
      aqi <- aqi %>%
        mutate(
          station_id = if ("station" %in% names(aqi)) station else "Delhi_CPCB_01",
          PM25 = if ("pm2.5" %in% names(aqi)) `pm2.5` else if ("pm25" %in% names(aqi)) pm25 else NA,
          PM10 = if ("pm10" %in% names(aqi)) pm10 else NA,
          NO2 = if ("no2" %in% names(aqi)) no2 else NA,
          O3 = if ("o3" %in% names(aqi)) o3 else NA,
          temperature = if ("temperature" %in% names(aqi)) temperature else 25,
          humidity = if ("humidity" %in% names(aqi)) humidity else 60,
          AQI = if ("aqi" %in% names(aqi)) aqi else rowMeans(aqi %>% select(all_of(c("PM25", "PM10", "NO2", "O3"))), 
                                                               na.rm = TRUE),
          AQI_category = cut(AQI, breaks = c(0, 50, 100, 150, 200, 300, Inf),
                             labels = c("Good", "Satisfactory", "Moderately Polluted", "Poor", "Very Poor", "Severe"))
        ) %>%
        select(timestamp, station_id, PM25, PM10, NO2, O3, temperature, humidity, AQI, AQI_category)
      
      return(aqi)
    }, error = function(e) {
      stop(paste("Error reading Excel air quality file:", e$message))
    })
  } else {
    # CSV format
    aqi <- fread(file_path)
    
    # Standardize format
    if ("timestamp" %in% names(aqi)) {
      aqi$timestamp <- as.POSIXct(aqi$timestamp)
    } else if ("date" %in% names(aqi)) {
      aqi$timestamp <- as.POSIXct(aqi$date)
      aqi$date <- NULL
    }
    
    return(aqi)
  }
}

# Parse energy data: Regional grid data (Grid-India format)
parse_energy_regional <- function(file_path) {
  energy_raw <- fread(file_path)
  
  # Expected format: source, NR, WR, SR, ER, NER, All India, date
  # Delhi is in Northern Region (NR)
  
  if ("date" %in% names(energy_raw)) {
    energy_raw$timestamp <- as.POSIXct(energy_raw$date, format = "%Y-%m-%d")
  } else if ("Date" %in% names(energy_raw)) {
    energy_raw$timestamp <- as.POSIXct(energy_raw$Date, format = "%Y-%m-%d")
  }
  
  # Filter to Delhi region (Northern Region "NR" or "Total" rows)
  energy <- energy_raw %>%
    filter(tolower(source) %in% c("total", "hydro", "wind gen(mu)")) %>%
    mutate(
      building_type = "Regional_Grid",
      energy_consumption_kwh = if ("NR" %in% names(energy_raw)) NR * 1000 else `All India` * 100,  # Scale to kWh
      peak_demand_kw = energy_consumption_kwh * 0.7,  # Estimate peak as 70% of total
      renewable_percent = if ("Wind Gen(MU)" %in% names(energy_raw)) 15 else 5,
      cost_usd = energy_consumption_kwh * 0.08  # Estimate cost
    ) %>%
    select(timestamp, building_type, energy_consumption_kwh, peak_demand_kw, renewable_percent, cost_usd)
  
  return(energy)
}

# Parse weather data: NOAA ISD FM-12 encoded format
parse_noaa_isd <- function(file_path) {
  weather_raw <- fread(file_path)
  
  # Extract and parse FM-12 encoded fields
  extract_isd_value <- function(isd_field, position, decimals = 0) {
    # ISD format: "value,quality[,additional]"
    # Example: "+0074,1" for temperature +7.4°C
    
    if (is.na(isd_field) || isd_field == "") return(NA)
    
    parts <- strsplit(as.character(isd_field), ",")[[1]]
    if (length(parts) == 0) return(NA)
    
    value <- as.numeric(parts[1])
    if (is.na(value) || value == 9999 || value == 99999) return(NA)  # Missing value codes
    
    # Decode: most values are multiplied by 10^decimals
    if (decimals > 0) value <- value / (10 ^ decimals)
    return(value)
  }
  
  weather <- weather_raw %>%
    mutate(
      timestamp = as.POSIXct(DATE, format = "%Y-%m-%dT%H:%M:%S"),
      temperature = sapply(TMP, function(x) extract_isd_value(x, 1, decimals = 1)),
      humidity = 50,  # DEW point available but calculating RH requires more complex formula
      wind_speed = sapply(WND, function(x) {
        # WND format: "direction,quality,type,speed"  
        parts <- strsplit(as.character(x), ",")[[1]]
        if (length(parts) >= 4) {
          speed <- as.numeric(parts[4]) / 10  # Wind speed in m/s (divide by 10)
          if (!is.na(speed) && speed < 999) speed * 3.6 else NA  # Convert m/s to km/h
        } else NA
      }),
      precipitation_mm = 0,  # ISD data requires additional parsing for precipitation
      pressure_hpa = sapply(SLP, function(x) extract_isd_value(x, 1, decimals = 1)),
      condition = if_else(!is.na(TMP), "Varied", "Unknown")
    ) %>%
    select(timestamp, temperature, humidity, wind_speed, precipitation_mm, pressure_hpa, condition)
  
  # Handle missing values
  weather$temperature <- replace_na(weather$temperature, mean(weather$temperature, na.rm = TRUE))
  weather$pressure_hpa <- replace_na(weather$pressure_hpa, 1013)  # Standard pressure
  weather$wind_speed <- replace_na(weather$wind_speed, 0)
  
  return(weather)
}

# ==========================================
# 2. LOAD AND ADAPT RAW DATA
# ==========================================
cat("Loading and adapting real data...\n")

# Load datasets with format detection
traffic_raw <- parse_traffic_timeweekday("data/raw/traffic_data.csv")
air_quality_raw <- parse_air_quality(air_quality_file)
energy_raw <- parse_energy_regional("data/raw/energy_data.csv")
weather_raw <- parse_noaa_isd("data/raw/weather_data.csv")

cat("✓ Traffic data (expanded to hourly):", nrow(traffic_raw), "records\n")
cat("✓ Air quality data:", nrow(air_quality_raw), "records\n")
cat("✓ Energy data:", nrow(energy_raw), "records\n")
cat("✓ Weather data:", nrow(weather_raw), "records\n\n")


# ==========================================
# 3. DATA CLEANING - TRAFFIC
# ==========================================
cat("Cleaning traffic data...\n")

traffic_clean <- traffic_raw %>%
  filter(!is.na(vehicle_count) & !is.na(average_speed)) %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    zone = as.factor(zone),
    congestion_level = as.factor(congestion_level),
    # Remove outliers
    vehicle_count = ifelse(vehicle_count < 0, NA, vehicle_count),
    average_speed = ifelse(average_speed < 0 | average_speed > 120, NA, average_speed)
  ) %>%
  filter(!is.na(vehicle_count) & !is.na(average_speed)) %>%
  # Add time features
  mutate(
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    weekday = lubridate::wday(timestamp, label = TRUE),
    date = as.Date(timestamp),
    is_weekend = weekday %in% c("Sat", "Sun"),
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 18 ~ "Afternoon",
      hour >= 18 & hour < 22 ~ "Evening",
      TRUE ~ "Night"
    )
  ) %>%
  arrange(timestamp, zone)

cat("✓ Cleaned", nrow(traffic_clean), "records\n\n")

# ==========================================
# 4. DATA CLEANING - AIR QUALITY
# ==========================================
cat("Cleaning air quality data...\n")

air_quality_clean <- air_quality_raw %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    station_id = as.factor(station_id),
    AQI_category = as.factor(AQI_category),
    # Remove negative values
    across(c(PM25, PM10, NO2, O3, AQI), ~ifelse(. < 0, NA, .)),
    # Temperature and humidity ranges
    temperature = ifelse(temperature < -50 | temperature > 60, NA, temperature),
    humidity = ifelse(humidity < 0 | humidity > 100, NA, humidity)
  ) %>%
  filter(!is.na(PM25) & !is.na(PM10) & !is.na(AQI)) %>%
  # Add time features
  mutate(
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    weekday = lubridate::wday(timestamp, label = TRUE),
    date = as.Date(timestamp),
    is_weekend = weekday %in% c("Sat", "Sun")
  ) %>%
  arrange(timestamp, station_id)

cat("✓ Cleaned", nrow(air_quality_clean), "records\n\n")

# ==========================================
# 5. DATA CLEANING - ENERGY
# ==========================================
cat("Cleaning energy data...\n")

energy_clean <- energy_raw %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    building_type = as.factor(building_type),
    # Remove negative values
    energy_consumption_kwh = ifelse(energy_consumption_kwh < 0, NA, energy_consumption_kwh),
    peak_demand_kw = ifelse(peak_demand_kw < 0, NA, peak_demand_kw),
    renewable_percent = ifelse(renewable_percent < 0 | renewable_percent > 100, NA, renewable_percent)
  ) %>%
  filter(!is.na(energy_consumption_kwh)) %>%
  # Add time features
  mutate(
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    weekday = lubridate::wday(timestamp, label = TRUE),
    date = as.Date(timestamp),
    is_weekend = weekday %in% c("Sat", "Sun"),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  arrange(timestamp, building_type)

cat("✓ Cleaned", nrow(energy_clean), "records\n\n")

# ==========================================
# 6. DATA CLEANING - WEATHER
# ==========================================
cat("Cleaning weather data...\n")

weather_clean <- weather_raw %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    condition = as.factor(condition),
    # Validate ranges
    temperature = ifelse(temperature < -50 | temperature > 60, NA, temperature),
    humidity = ifelse(humidity < 0 | humidity > 100, NA, humidity),
    wind_speed = ifelse(wind_speed < 0 | wind_speed > 150, NA, wind_speed),
    precipitation_mm = ifelse(precipitation_mm < 0, 0, precipitation_mm)
  ) %>%
  filter(!is.na(temperature) & !is.na(humidity)) %>%
  mutate(
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    date = as.Date(timestamp)
  ) %>%
  arrange(timestamp)

cat("✓ Cleaned", nrow(weather_clean), "records\n\n")

# ==========================================
# 7. AGGREGATE AND MERGE DATA
# ==========================================
cat("Creating aggregated datasets...\n")

# Aggregate traffic by hour
traffic_hourly <- traffic_clean %>%
  group_by(timestamp, hour, date, weekday, is_weekend) %>%
  summarise(
    total_vehicles = sum(vehicle_count, na.rm = TRUE),
    avg_speed = mean(average_speed, na.rm = TRUE),
    high_congestion_zones = sum(congestion_level == "High"),
    .groups = "drop"
  )

# Aggregate air quality by hour
air_quality_hourly <- air_quality_clean %>%
  group_by(timestamp, hour, date, weekday, is_weekend) %>%
  summarise(
    avg_PM25 = mean(PM25, na.rm = TRUE),
    avg_PM10 = mean(PM10, na.rm = TRUE),
    avg_NO2 = mean(NO2, na.rm = TRUE),
    avg_O3 = mean(O3, na.rm = TRUE),
    avg_AQI = mean(AQI, na.rm = TRUE),
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_humidity = mean(humidity, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate energy by hour
energy_hourly <- energy_clean %>%
  group_by(timestamp, hour, date, weekday, is_weekend) %>%
  summarise(
    total_energy_kwh = sum(energy_consumption_kwh, na.rm = TRUE),
    avg_peak_demand = mean(peak_demand_kw, na.rm = TRUE),
    avg_renewable_percent = mean(renewable_percent, na.rm = TRUE),
    total_cost = sum(cost_usd, na.rm = TRUE),
    .groups = "drop"
  )

# Create master dataset
master_data <- traffic_hourly %>%
  left_join(air_quality_hourly, by = c("timestamp", "hour", "date", "weekday", "is_weekend")) %>%
  left_join(energy_hourly, by = c("timestamp", "hour", "date", "weekday", "is_weekend")) %>%
  left_join(weather_clean %>% select(timestamp, temperature, humidity, wind_speed, 
                                     precipitation_mm, pressure_hpa, condition),
           by = "timestamp")

cat("✓ Master dataset created:", nrow(master_data), "records\n\n")

# ==========================================
# 8. SAVE PROCESSED DATA
# ==========================================
cat("Saving processed data...\n")

# Save cleaned individual datasets
write.csv(traffic_clean, "data/processed/traffic_clean.csv", row.names = FALSE)
write.csv(air_quality_clean, "data/processed/air_quality_clean.csv", row.names = FALSE)
write.csv(energy_clean, "data/processed/energy_clean.csv", row.names = FALSE)
write.csv(weather_clean, "data/processed/weather_clean.csv", row.names = FALSE)

# Save aggregated datasets
write.csv(traffic_hourly, "data/processed/traffic_hourly.csv", row.names = FALSE)
write.csv(air_quality_hourly, "data/processed/air_quality_hourly.csv", row.names = FALSE)
write.csv(energy_hourly, "data/processed/energy_hourly.csv", row.names = FALSE)

# Save master dataset
write.csv(master_data, "data/processed/master_data.csv", row.names = FALSE)
saveRDS(master_data, "data/processed/master_data.rds")

cat("✓ All processed data saved\n\n")

# ==========================================
# 9. DATA SUMMARY
# ==========================================
cat("==================================================\n")
cat("DATA PREPROCESSING COMPLETE\n")
cat("==================================================\n\n")

cat("Processed files saved to data/processed/:\n")
cat("  - traffic_clean.csv\n")
cat("  - air_quality_clean.csv\n")
cat("  - energy_clean.csv\n")
cat("  - weather_clean.csv\n")
cat("  - master_data.csv/rds\n\n")

cat("Date range:", as.character(min(master_data$date)), "to", 
    as.character(max(master_data$date)), "\n")
cat("Total records in master dataset:", nrow(master_data), "\n\n")

cat("Summary statistics:\n")
print(summary(master_data %>% select(total_vehicles, avg_speed, avg_AQI, 
                                    total_energy_kwh, temperature)))

cat("\n==================================================\n")
cat("Next step: source('scripts/02_exploratory_analysis.R')\n")
cat("==================================================\n")
