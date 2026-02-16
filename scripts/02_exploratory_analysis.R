# Smart City Analytics - Exploratory Data Analysis
# This script performs comprehensive exploratory analysis

cat("==================================================\n")
cat("STEP 2: Exploratory Data Analysis\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(lubridate)

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Create outputs directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# ==========================================
# 1. LOAD PROCESSED DATA
# ==========================================
cat("Loading processed data...\n")

# Check if processed data exists
if (!file.exists("data/processed/master_data.rds")) {
  cat("Processed data not found. Running preprocessing...\n")
  source("scripts/01_data_preprocessing.R")
}

master_data <- readRDS("data/processed/master_data.rds")
traffic_clean <- read.csv("data/processed/traffic_clean.csv")
air_quality_clean <- read.csv("data/processed/air_quality_clean.csv")
energy_clean <- read.csv("data/processed/energy_clean.csv")

cat("✓ Data loaded successfully\n\n")

# ==========================================
# 2. BASIC STATISTICS
# ==========================================
cat("==================================================\n")
cat("BASIC STATISTICS\n")
cat("==================================================\n\n")

cat("Dataset Overview:\n")
cat("  Total records:", nrow(master_data), "\n")
cat("  Variables:", ncol(master_data), "\n")
cat("  Date range:", min(master_data$date), "to", max(master_data$date), "\n\n")

cat("Traffic Statistics:\n")
cat("  Average daily vehicles:", round(mean(master_data$total_vehicles, na.rm = TRUE)), "\n")
cat("  Average speed (km/h):", round(mean(master_data$avg_speed, na.rm = TRUE), 1), "\n")
cat("  Peak traffic hour:", master_data %>% 
      group_by(hour) %>% 
      summarise(avg_vehicles = mean(total_vehicles, na.rm = TRUE)) %>%
      slice_max(avg_vehicles, n = 1) %>% 
      pull(hour), ":00\n\n")

cat("Air Quality Statistics:\n")
cat("  Average AQI:", round(mean(master_data$avg_AQI, na.rm = TRUE), 1), "\n")
cat("  Average PM2.5:", round(mean(master_data$avg_PM25, na.rm = TRUE), 1), "μg/m³\n")
cat("  Average PM10:", round(mean(master_data$avg_PM10, na.rm = TRUE), 1), "μg/m³\n\n")

cat("Energy Statistics:\n")
cat("  Daily energy consumption:", round(mean(master_data$total_energy_kwh, na.rm = TRUE)), "kWh\n")
cat("  Average renewable usage:", round(mean(master_data$avg_renewable_percent, na.rm = TRUE), 1), "%\n")
cat("  Daily energy cost: $", round(mean(master_data$total_cost, na.rm = TRUE), 2), "\n\n")

# ==========================================
# 3. MISSING DATA ANALYSIS
# ==========================================
cat("==================================================\n")
cat("MISSING DATA ANALYSIS\n")
cat("==================================================\n\n")

missing_summary <- master_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(missing_count / nrow(master_data) * 100, 2)) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

if (nrow(missing_summary) > 0) {
  print(missing_summary)
} else {
  cat("✓ No missing data detected\n")
}
cat("\n")

# ==========================================
# 4. CORRELATION ANALYSIS
# ==========================================
cat("==================================================\n")
cat("CORRELATION ANALYSIS\n")
cat("==================================================\n\n")

# Select numeric variables for correlation
numeric_vars <- master_data %>%
  select(total_vehicles, avg_speed, avg_PM25, avg_PM10, avg_NO2, 
         avg_AQI, total_energy_kwh, avg_renewable_percent,
         temperature, humidity, wind_speed) %>%
  na.omit()

# Calculate correlation matrix
cor_matrix <- cor(numeric_vars)

cat("Top correlations:\n")
# Get top correlations
cor_melted <- melt(cor_matrix) %>%
  filter(Var1 != Var2) %>%
  arrange(desc(abs(value))) %>%
  distinct(value, .keep_all = TRUE) %>%
  head(10)

print(cor_melted)
cat("\n")

# Save correlation plot
png("outputs/correlation_matrix.png", width = 1200, height = 1000, res = 120)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix - Smart City Variables",
         mar = c(0, 0, 2, 0))
dev.off()
cat("✓ Correlation plot saved to outputs/correlation_matrix.png\n\n")

# ==========================================
# 5. TEMPORAL PATTERNS
# ==========================================
cat("==================================================\n")
cat("TEMPORAL PATTERNS\n")
cat("==================================================\n\n")

# Hourly patterns
hourly_patterns <- master_data %>%
  group_by(hour) %>%
  summarise(
    avg_vehicles = mean(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    avg_energy = mean(total_energy_kwh, na.rm = TRUE),
    .groups = "drop"
  )

cat("Peak Hours Analysis:\n")
cat("  Peak traffic:", hourly_patterns %>% slice_max(avg_vehicles, n = 1) %>% pull(hour), ":00\n")
cat("  Worst air quality:", hourly_patterns %>% slice_max(avg_aqi, n = 1) %>% pull(hour), ":00\n")
cat("  Peak energy usage:", hourly_patterns %>% slice_max(avg_energy, n = 1) %>% pull(hour), ":00\n\n")

# Day of week patterns
weekday_patterns <- master_data %>%
  group_by(weekday) %>%
  summarise(
    avg_vehicles = mean(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    avg_energy = mean(total_energy_kwh, na.rm = TRUE),
    .groups = "drop"
  )

cat("Weekday vs Weekend:\n")
weekend_comparison <- master_data %>%
  group_by(is_weekend) %>%
  summarise(
    avg_vehicles = mean(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    avg_energy = mean(total_energy_kwh, na.rm = TRUE),
    .groups = "drop"
  )

print(weekend_comparison)
cat("\n")

# ==========================================
# 6. ZONE/STATION ANALYSIS
# ==========================================
cat("==================================================\n")
cat("ZONE & STATION ANALYSIS\n")
cat("==================================================\n\n")

# Convert timestamp to POSIXct
traffic_clean$timestamp <- as.POSIXct(traffic_clean$timestamp)
air_quality_clean$timestamp <- as.POSIXct(air_quality_clean$timestamp)

# Traffic by zone
zone_stats <- traffic_clean %>%
  group_by(zone) %>%
  summarise(
    avg_vehicles = mean(vehicle_count, na.rm = TRUE),
    avg_speed = mean(average_speed, na.rm = TRUE),
    congestion_rate = mean(congestion_level == "High") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(avg_vehicles))

cat("Traffic by Zone:\n")
print(zone_stats)
cat("\n")

# Air quality by station
station_stats <- air_quality_clean %>%
  group_by(station_id) %>%
  summarise(
    avg_aqi = mean(AQI, na.rm = TRUE),
    avg_pm25 = mean(PM25, na.rm = TRUE),
    days_unhealthy = sum(AQI > 100) / n() * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(avg_aqi))

cat("Air Quality by Station:\n")
print(station_stats)
cat("\n")

# ==========================================
# 7. BUILDING TYPE ANALYSIS
# ==========================================
cat("==================================================\n")
cat("BUILDING TYPE ENERGY ANALYSIS\n")
cat("==================================================\n\n")

energy_clean$timestamp <- as.POSIXct(energy_clean$timestamp)

building_stats <- energy_clean %>%
  group_by(building_type) %>%
  summarise(
    avg_consumption = mean(energy_consumption_kwh, na.rm = TRUE),
    total_consumption = sum(energy_consumption_kwh, na.rm = TRUE),
    avg_renewable = mean(renewable_percent, na.rm = TRUE),
    total_cost = sum(cost_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_consumption))

print(building_stats)
cat("\n")

# ==========================================
# 8. OUTLIER DETECTION
# ==========================================
cat("==================================================\n")
cat("OUTLIER DETECTION\n")
cat("==================================================\n\n")

detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
}

outliers <- master_data %>%
  summarise(
    vehicles_outliers = detect_outliers(total_vehicles),
    aqi_outliers = detect_outliers(avg_AQI),
    energy_outliers = detect_outliers(total_energy_kwh),
    temp_outliers = detect_outliers(temperature)
  )

cat("Outliers detected:\n")
print(outliers)
cat("\n")

# ==========================================
# 9. SAVE ANALYSIS RESULTS
# ==========================================
cat("Saving analysis results...\n")

analysis_results <- list(
  hourly_patterns = hourly_patterns,
  weekday_patterns = weekday_patterns,
  zone_stats = zone_stats,
  station_stats = station_stats,
  building_stats = building_stats,
  correlation_matrix = cor_matrix
)

saveRDS(analysis_results, "outputs/exploratory_analysis_results.rds")

# Save summary report
sink("outputs/analysis_summary.txt")
cat("SMART CITY ANALYTICS - EXPLORATORY ANALYSIS SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
cat("Generated:", Sys.time(), "\n\n")
cat("Dataset Overview:\n")
cat("  Records:", nrow(master_data), "\n")
cat("  Date Range:", min(master_data$date), "to", max(master_data$date), "\n\n")
cat("\nKey Findings:\n")
cat("  Peak Traffic Hour:", hourly_patterns %>% slice_max(avg_vehicles, n = 1) %>% pull(hour), ":00\n")
cat("  Worst Air Quality Hour:", hourly_patterns %>% slice_max(avg_aqi, n = 1) %>% pull(hour), ":00\n")
cat("  Peak Energy Hour:", hourly_patterns %>% slice_max(avg_energy, n = 1) %>% pull(hour), ":00\n")
cat("  Most Congested Zone:", zone_stats %>% slice_max(congestion_rate, n = 1) %>% pull(zone), "\n")
cat("  Worst AQI Station:", station_stats %>% slice_max(avg_aqi, n = 1) %>% pull(station_id), "\n")
sink()

cat("✓ Analysis results saved\n\n")

# ==========================================
# 10. SUMMARY
# ==========================================
cat("==================================================\n")
cat("EXPLORATORY ANALYSIS COMPLETE\n")
cat("==================================================\n\n")

cat("Outputs saved to outputs/:\n")
cat("  - correlation_matrix.png\n")
cat("  - exploratory_analysis_results.rds\n")
cat("  - analysis_summary.txt\n\n")

cat("==================================================\n")
cat("Next step: source('scripts/03_visualization.R')\n")
cat("==================================================\n")
