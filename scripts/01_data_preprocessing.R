# Smart City Analytics - Data Preprocessing Script
# This script loads, cleans, and prepares data for analysis

cat("==================================================\n")
cat("STEP 1: Data Preprocessing\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# ==========================================
# 1. GENERATE SAMPLE DATA (if not exists)
# ==========================================
if (!file.exists("data/raw/traffic_data.csv")) {
  cat("Sample data not found. Generating...\n")
  source("data/raw/generate_sample_data.R")
  cat("\n")
}

# ==========================================
# 2. LOAD RAW DATA
# ==========================================
cat("Loading raw data...\n")

# Load datasets
traffic_raw <- fread("data/raw/traffic_data.csv")
air_quality_raw <- fread("data/raw/air_quality_data.csv")
energy_raw <- fread("data/raw/energy_data.csv")
weather_raw <- fread("data/raw/weather_data.csv")

cat("✓ Traffic data:", nrow(traffic_raw), "records\n")
cat("✓ Air quality data:", nrow(air_quality_raw), "records\n")
cat("✓ Energy data:", nrow(energy_raw), "records\n")
cat("✓ Weather data:", nrow(weather_raw), "records\n\n")

# ==========================================
# 3. DATA CLEANING - TRAFFIC
# ==========================================
cat("Cleaning traffic data...\n")

traffic_clean <- traffic_raw %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    zone = as.factor(zone),
    congestion_level = as.factor(congestion_level),
    # Remove outliers
    vehicle_count = ifelse(vehicle_count < 0, NA, vehicle_count),
    average_speed = ifelse(average_speed < 0 | average_speed > 120, NA, average_speed)
  ) %>%
  # Remove NA values
  filter(!is.na(vehicle_count) & !is.na(average_speed)) %>%
  # Add time features
  mutate(
    hour = hour(timestamp),
    day = day(timestamp),
    month = month(timestamp),
    weekday = wday(timestamp, label = TRUE),
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
    weekday = wday(timestamp, label = TRUE),
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
    weekday = wday(timestamp, label = TRUE),
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
