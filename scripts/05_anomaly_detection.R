# Smart City Analytics - Anomaly Detection
# Detect unusual patterns in traffic, air quality, and energy data

cat("==================================================\n")
cat("STEP 5: Anomaly Detection\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Set working directory
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# ==========================================
# 1. LOAD DATA
# ==========================================
cat("Loading data...\n")

if (!file.exists("data/processed/master_data.rds")) {
  cat("Running preprocessing...\n")
  source("scripts/01_data_preprocessing.R")
}

master_data <- readRDS("data/processed/master_data.rds")
master_data$timestamp <- as.POSIXct(master_data$timestamp)
master_data$date <- as.Date(master_data$date)

cat("✓ Data loaded:", nrow(master_data), "records\n\n")

# ==========================================
# 2. Z-SCORE ANOMALY DETECTION
# ==========================================
cat("Detecting anomalies using Z-score method...\n")

detect_zscore_anomalies <- function(data, column, threshold = 3) {
  values <- data[[column]]
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  z_scores <- abs((values - mean_val) / sd_val)
  return(z_scores > threshold)
}

# Detect anomalies for each metric
master_data <- master_data %>%
  mutate(
    traffic_anomaly = detect_zscore_anomalies(., "total_vehicles"),
    aqi_anomaly = detect_zscore_anomalies(., "avg_AQI"),
    energy_anomaly = detect_zscore_anomalies(., "total_energy_kwh")
  )

# ==========================================
# 3. IQR-BASED ANOMALY DETECTION
# ==========================================
cat("Detecting anomalies using IQR method...\n")

detect_iqr_anomalies <- function(data, column, multiplier = 1.5) {
  values <- data[[column]]
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  return(values < lower_bound | values > upper_bound)
}

master_data <- master_data %>%
  mutate(
    traffic_anomaly_iqr = detect_iqr_anomalies(., "total_vehicles"),
    aqi_anomaly_iqr = detect_iqr_anomalies(., "avg_AQI"),
    energy_anomaly_iqr = detect_iqr_anomalies(., "total_energy_kwh")
  )

# ==========================================
# 4. MOVING AVERAGE ANOMALY DETECTION
# ==========================================
cat("Detecting anomalies using Moving Average method...\n")

detect_ma_anomalies <- function(data, column, window_size = 24, threshold = 2) {
  values <- data[[column]]
  n <- length(values)
  anomalies <- rep(FALSE, n)
  
  for (i in (window_size + 1):n) {
    window_values <- values[(i - window_size):(i - 1)]
    ma <- mean(window_values, na.rm = TRUE)
    ma_sd <- sd(window_values, na.rm = TRUE)
    
    if (!is.na(ma_sd) && ma_sd > 0) {
      anomalies[i] <- abs(values[i] - ma) > threshold * ma_sd
    }
  }
  return(anomalies)
}

# Sort by timestamp for proper moving average calculation
master_data <- master_data %>% arrange(timestamp)

master_data <- master_data %>%
  mutate(
    traffic_anomaly_ma = detect_ma_anomalies(., "total_vehicles"),
    aqi_anomaly_ma = detect_ma_anomalies(., "avg_AQI"),
    energy_anomaly_ma = detect_ma_anomalies(., "total_energy_kwh")
  )

cat("✓ Moving Average anomaly detection complete\n")

# ==========================================
# 5. TIME-BASED ANOMALY DETECTION
# ==========================================
cat("Detecting time-based anomalies...\n")

# Detect anomalies based on hourly patterns
hourly_stats <- master_data %>%
  group_by(hour) %>%
  summarise(
    traffic_mean = mean(total_vehicles, na.rm = TRUE),
    traffic_sd = sd(total_vehicles, na.rm = TRUE),
    aqi_mean = mean(avg_AQI, na.rm = TRUE),
    aqi_sd = sd(avg_AQI, na.rm = TRUE),
    energy_mean = mean(total_energy_kwh, na.rm = TRUE),
    energy_sd = sd(total_energy_kwh, na.rm = TRUE)
  )

master_data <- master_data %>%
  left_join(hourly_stats, by = "hour") %>%
  mutate(
    traffic_time_anomaly = abs(total_vehicles - traffic_mean) > 2 * traffic_sd,
    aqi_time_anomaly = abs(avg_AQI - aqi_mean) > 2 * aqi_sd,
    energy_time_anomaly = abs(total_energy_kwh - energy_mean) > 2 * energy_sd
  )

# ==========================================
# 6. SUMMARIZE ANOMALIES
# ==========================================
cat("\n==================================================\n")
cat("ANOMALY SUMMARY\n")
cat("==================================================\n\n")

anomaly_summary <- data.frame(
  Metric = c("Traffic", "Air Quality", "Energy"),
  ZScore_Anomalies = c(
    sum(master_data$traffic_anomaly, na.rm = TRUE),
    sum(master_data$aqi_anomaly, na.rm = TRUE),
    sum(master_data$energy_anomaly, na.rm = TRUE)
  ),
  IQR_Anomalies = c(
    sum(master_data$traffic_anomaly_iqr, na.rm = TRUE),
    sum(master_data$aqi_anomaly_iqr, na.rm = TRUE),
    sum(master_data$energy_anomaly_iqr, na.rm = TRUE)
  ),
  MovingAvg_Anomalies = c(
    sum(master_data$traffic_anomaly_ma, na.rm = TRUE),
    sum(master_data$aqi_anomaly_ma, na.rm = TRUE),
    sum(master_data$energy_anomaly_ma, na.rm = TRUE)
  ),
  TimeBased_Anomalies = c(
    sum(master_data$traffic_time_anomaly, na.rm = TRUE),
    sum(master_data$aqi_time_anomaly, na.rm = TRUE),
    sum(master_data$energy_time_anomaly, na.rm = TRUE)
  )
)

print(anomaly_summary)

# ==========================================
# 7. VISUALIZE ANOMALIES
# ==========================================
cat("\nGenerating anomaly visualizations...\n")

# Create output directory
if (!dir.exists("outputs")) dir.create("outputs")

# Traffic anomalies plot
traffic_plot <- ggplot(master_data, aes(x = timestamp, y = total_vehicles)) +
  geom_line(color = "steelblue", alpha = 0.5) +
  geom_point(data = filter(master_data, traffic_anomaly), 
             aes(x = timestamp, y = total_vehicles), 
             color = "red", size = 2, alpha = 0.7) +
  labs(title = "Traffic Anomalies (Z-Score Method)",
       subtitle = "Red points indicate anomalies",
       x = "Time", y = "Total Vehicles") +
  theme_minimal()

ggsave("outputs/traffic_anomalies.png", traffic_plot, width = 12, height = 6)

# AQI anomalies plot
aqi_plot <- ggplot(master_data, aes(x = timestamp, y = avg_AQI)) +
  geom_line(color = "orange", alpha = 0.5) +
  geom_point(data = filter(master_data, aqi_anomaly), 
             aes(x = timestamp, y = avg_AQI), 
             color = "red", size = 2, alpha = 0.7) +
  labs(title = "Air Quality Anomalies (Z-Score Method)",
       subtitle = "Red points indicate anomalies",
       x = "Time", y = "Average AQI") +
  theme_minimal()

ggsave("outputs/aqi_anomalies.png", aqi_plot, width = 12, height = 6)

# Energy anomalies plot
energy_plot <- ggplot(master_data, aes(x = timestamp, y = total_energy_kwh)) +
  geom_line(color = "green4", alpha = 0.5) +
  geom_point(data = filter(master_data, energy_anomaly), 
             aes(x = timestamp, y = total_energy_kwh), 
             color = "red", size = 2, alpha = 0.7) +
  labs(title = "Energy Consumption Anomalies (Z-Score Method)",
       subtitle = "Red points indicate anomalies",
       x = "Time", y = "Total Energy (kWh)") +
  theme_minimal()

ggsave("outputs/energy_anomalies.png", energy_plot, width = 12, height = 6)

# ==========================================
# 8. EXPORT ANOMALY DATA
# ==========================================
cat("Exporting anomaly data...\n")

# Save anomaly records
anomaly_records <- master_data %>%
  filter(traffic_anomaly | aqi_anomaly | energy_anomaly) %>%
  select(timestamp, date, hour, total_vehicles, avg_AQI, total_energy_kwh,
         traffic_anomaly, aqi_anomaly, energy_anomaly)

write.csv(anomaly_records, "outputs/anomaly_records.csv", row.names = FALSE)

# Save summary
write.csv(anomaly_summary, "outputs/anomaly_summary.csv", row.names = FALSE)

cat("\n✓ Anomaly detection complete!\n")
cat("  - Anomaly records saved to outputs/anomaly_records.csv\n")
cat("  - Summary saved to outputs/anomaly_summary.csv\n")
cat("  - Visualizations saved to outputs/\n")
