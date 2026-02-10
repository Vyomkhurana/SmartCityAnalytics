# Generate Sample Data for Smart City Analytics
# This script creates realistic sample datasets for demonstration

library(dplyr)
library(lubridate)

set.seed(42)

# Generate date-time sequence for one year
dates <- seq(from = as.POSIXct("2025-01-01 00:00:00"),
             to = as.POSIXct("2025-12-31 23:00:00"),
             by = "hour")

n_records <- length(dates)

# ==========================================
# 1. TRAFFIC DATA
# ==========================================
cat("Generating traffic data...\n")

# Define 5 major city zones
zones <- c("Downtown", "Industrial", "Residential", "Commercial", "Suburbs")

traffic_data <- data.frame(
  timestamp = rep(dates, each = length(zones)),
  zone = rep(zones, times = n_records),
  vehicle_count = as.integer(
    50 + 200 * sin(2 * pi * rep(1:n_records, each = length(zones)) / 24) + # Daily pattern
    100 * sin(2 * pi * rep(1:n_records, each = length(zones)) / (24 * 7)) + # Weekly pattern
    rnorm(n_records * length(zones), 0, 30)
  ),
  average_speed = pmin(pmax(
    45 + 15 * sin(2 * pi * rep(1:n_records, each = length(zones)) / 24) +
    rnorm(n_records * length(zones), 0, 8),
    10), 80),
  congestion_level = sample(c("Low", "Medium", "High"), 
                           n_records * length(zones), 
                           replace = TRUE,
                           prob = c(0.5, 0.3, 0.2))
)

# Make vehicle counts positive
traffic_data$vehicle_count <- pmax(traffic_data$vehicle_count, 20)

write.csv(traffic_data, "traffic_data.csv", row.names = FALSE)
cat("✓ traffic_data.csv created\n")

# ==========================================
# 2. AIR QUALITY DATA
# ==========================================
cat("Generating air quality data...\n")

# Create monitoring stations
stations <- c("Station_North", "Station_South", "Station_East", "Station_West", "Station_Central")

air_quality_data <- data.frame(
  timestamp = rep(dates, each = length(stations)),
  station_id = rep(stations, times = n_records),
  PM25 = pmax(
    25 + 15 * sin(2 * pi * rep(1:n_records, each = length(stations)) / (24 * 30)) + # Monthly pattern
    10 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) + # Daily pattern
    rnorm(n_records * length(stations), 0, 8),
    5),
  PM10 = pmax(
    50 + 25 * sin(2 * pi * rep(1:n_records, each = length(stations)) / (24 * 30)) +
    15 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) +
    rnorm(n_records * length(stations), 0, 12),
    10),
  NO2 = pmax(
    30 + 20 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) +
    rnorm(n_records * length(stations), 0, 10),
    5),
  O3 = pmax(
    40 + 15 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) +
    rnorm(n_records * length(stations), 0, 8),
    10),
  temperature = 15 + 10 * sin(2 * pi * rep(1:n_records, each = length(stations)) / (24 * 365)) + # Yearly
                5 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) + # Daily
                rnorm(n_records * length(stations), 0, 3),
  humidity = pmin(pmax(
    60 + 20 * sin(2 * pi * rep(1:n_records, each = length(stations)) / 24) +
    rnorm(n_records * length(stations), 0, 10),
    20), 100)
)

# Calculate AQI (simplified)
air_quality_data$AQI <- as.integer(
  pmax(
    air_quality_data$PM25 * 2,
    air_quality_data$PM10 * 1.5,
    air_quality_data$NO2 * 1.8
  ) + rnorm(nrow(air_quality_data), 0, 5)
)

# Categorize AQI
air_quality_data$AQI_category <- cut(
  air_quality_data$AQI,
  breaks = c(0, 50, 100, 150, 200, 300, Inf),
  labels = c("Good", "Moderate", "Unhealthy_Sensitive", "Unhealthy", "Very_Unhealthy", "Hazardous"),
  right = FALSE
)

write.csv(air_quality_data, "air_quality_data.csv", row.names = FALSE)
cat("✓ air_quality_data.csv created\n")

# ==========================================
# 3. ENERGY CONSUMPTION DATA
# ==========================================
cat("Generating energy consumption data...\n")

# Building types
building_types <- c("Residential", "Commercial", "Industrial", "Public")

energy_data <- data.frame(
  timestamp = rep(dates, each = length(building_types)),
  building_type = rep(building_types, times = n_records),
  energy_consumption_kwh = pmax(
    100 + 150 * sin(2 * pi * rep(1:n_records, each = length(building_types)) / 24) + # Daily pattern
    80 * sin(2 * pi * rep(1:n_records, each = length(building_types)) / (24 * 365)) + # Seasonal
    rnorm(n_records * length(building_types), 0, 30),
    20),
  peak_demand_kw = pmax(
    50 + 75 * sin(2 * pi * rep(1:n_records, each = length(building_types)) / 24) +
    rnorm(n_records * length(building_types), 0, 15),
    10),
  renewable_percent = pmin(pmax(
    25 + 10 * sin(2 * pi * rep(1:n_records, each = length(building_types)) / (24 * 30)) +
    rnorm(n_records * length(building_types), 0, 8),
    0), 60)
)

# Add cost calculation
energy_data$cost_usd <- energy_data$energy_consumption_kwh * 0.12

write.csv(energy_data, "energy_data.csv", row.names = FALSE)
cat("✓ energy_data.csv created\n")

# ==========================================
# 4. WEATHER DATA (Supporting)
# ==========================================
cat("Generating weather data...\n")

weather_data <- data.frame(
  timestamp = dates,
  temperature = 15 + 10 * sin(2 * pi * (1:n_records) / (24 * 365)) + # Seasonal
                5 * sin(2 * pi * (1:n_records) / 24) + # Daily
                rnorm(n_records, 0, 3),
  humidity = pmin(pmax(
    60 + 20 * sin(2 * pi * (1:n_records) / (24 * 30)) +
    rnorm(n_records, 0, 10),
    20), 100),
  wind_speed = pmax(
    8 + 4 * sin(2 * pi * (1:n_records) / 24) +
    rnorm(n_records, 0, 2),
    0),
  precipitation_mm = pmax(
    rnorm(n_records, 0.5, 2),
    0),
  pressure_hpa = 1013 + 10 * sin(2 * pi * (1:n_records) / (24 * 7)) +
                 rnorm(n_records, 0, 5)
)

# Add weather condition
weather_data$condition <- sample(
  c("Clear", "Cloudy", "Rainy", "Foggy"),
  n_records,
  replace = TRUE,
  prob = c(0.5, 0.3, 0.15, 0.05)
)

write.csv(weather_data, "weather_data.csv", row.names = FALSE)
cat("✓ weather_data.csv created\n")

cat("\n==================================================\n")
cat("Sample data generation complete!\n")
cat("Generated files:\n")
cat("  - traffic_data.csv (", nrow(traffic_data), "records )\n")
cat("  - air_quality_data.csv (", nrow(air_quality_data), "records )\n")
cat("  - energy_data.csv (", nrow(energy_data), "records )\n")
cat("  - weather_data.csv (", nrow(weather_data), "records )\n")
cat("==================================================\n")
