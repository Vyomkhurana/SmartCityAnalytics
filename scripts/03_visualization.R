# Smart City Analytics - Visualization Script
# This script creates comprehensive visualizations

cat("==================================================\n")
cat("STEP 3: Data Visualization\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(lubridate)

# Set working directory to project root
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Create outputs directory
if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots", recursive = TRUE)
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
traffic_clean <- read.csv("data/processed/traffic_clean.csv")
air_quality_clean <- read.csv("data/processed/air_quality_clean.csv")
energy_clean <- read.csv("data/processed/energy_clean.csv")

# Convert timestamps
master_data$timestamp <- as.POSIXct(master_data$timestamp)
master_data$date <- as.Date(master_data$date)
traffic_clean$timestamp <- as.POSIXct(traffic_clean$timestamp)
air_quality_clean$timestamp <- as.POSIXct(air_quality_clean$timestamp)
energy_clean$timestamp <- as.POSIXct(energy_clean$timestamp)

cat("✓ Data loaded\n\n")

# Set theme
theme_set(theme_minimal(base_size = 12))
custom_colors <- brewer.pal(8, "Set2")

# ==========================================
# 2. TRAFFIC VISUALIZATIONS
# ==========================================
cat("Creating traffic visualizations...\n")

# 2.1 Hourly traffic pattern
p1 <- master_data %>%
  group_by(hour) %>%
  summarise(avg_vehicles = mean(total_vehicles, na.rm = TRUE),
            avg_speed = mean(avg_speed, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = avg_vehicles)) +
  geom_line(color = custom_colors[1], size = 1.2) +
  geom_area(alpha = 0.3, fill = custom_colors[1]) +
  labs(title = "Average Hourly Traffic Volume",
       x = "Hour of Day", y = "Number of Vehicles") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal()

ggsave("outputs/plots/traffic_hourly.png", p1, width = 10, height = 6, dpi = 300)

# 2.2 Traffic by zone
p2 <- traffic_clean %>%
  group_by(zone, hour) %>%
  summarise(avg_vehicles = mean(vehicle_count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = hour, y = avg_vehicles, color = zone)) +
  geom_line(size = 1) +
  labs(title = "Traffic Patterns by Zone",
       x = "Hour of Day", y = "Average Vehicle Count",
       color = "Zone") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("outputs/plots/traffic_by_zone.png", p2, width = 12, height = 6, dpi = 300)

# 2.3 Speed vs congestion
p3 <- traffic_clean %>%
  sample_n(min(5000, nrow(traffic_clean))) %>%
  ggplot(aes(x = vehicle_count, y = average_speed, color = congestion_level)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Vehicle Speed vs Traffic Volume",
       x = "Vehicle Count", y = "Average Speed (km/h)",
       color = "Congestion Level") +
  scale_color_manual(values = c("Low" = "green3", "Medium" = "orange", "High" = "red")) +
  theme_minimal()

ggsave("outputs/plots/speed_vs_volume.png", p3, width = 10, height = 6, dpi = 300)

# 2.4 Weekday vs Weekend
p4 <- master_data %>%
  group_by(hour, is_weekend) %>%
  summarise(avg_vehicles = mean(total_vehicles, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = hour, y = avg_vehicles, color = is_weekend, linetype = is_weekend)) +
  geom_line(size = 1.2) +
  labs(title = "Traffic: Weekday vs Weekend",
       x = "Hour of Day", y = "Average Vehicle Count",
       color = "Weekend", linetype = "Weekend") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  scale_color_manual(values = c("TRUE" = custom_colors[2], "FALSE" = custom_colors[1])) +
  theme_minimal()

ggsave("outputs/plots/traffic_weekday_weekend.png", p4, width = 10, height = 6, dpi = 300)

cat("✓ Traffic plots saved\n\n")

# ==========================================
# 3. AIR QUALITY VISUALIZATIONS
# ==========================================
cat("Creating air quality visualizations...\n")

# 3.1 AQI over time
p5 <- master_data %>%
  sample_n(min(2000, nrow(master_data))) %>%
  ggplot(aes(x = timestamp, y = avg_AQI)) +
  geom_line(color = custom_colors[3], alpha = 0.7) +
  geom_smooth(method = "loess", color = "red", size = 1.2) +
  labs(title = "Air Quality Index Over Time",
       x = "Date", y = "AQI") +
  theme_minimal()

ggsave("outputs/plots/aqi_time_series.png", p5, width = 12, height = 6, dpi = 300)

# 3.2 Pollutants comparison
pollutants_data <- air_quality_clean %>%
  group_by(hour) %>%
  summarise(
    PM25 = mean(PM25, na.rm = TRUE),
    PM10 = mean(PM10, na.rm = TRUE),
    NO2 = mean(NO2, na.rm = TRUE),
    O3 = mean(O3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(PM25, PM10, NO2, O3), names_to = "Pollutant", values_to = "Value")

p6 <- ggplot(pollutants_data, aes(x = hour, y = Value, color = Pollutant)) +
  geom_line(size = 1) +
  labs(title = "Air Pollutants Throughout the Day",
       x = "Hour of Day", y = "Concentration (μg/m³)",
       color = "Pollutant") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("outputs/plots/pollutants_hourly.png", p6, width = 10, height = 6, dpi = 300)

# 3.3 AQI by station
p7 <- air_quality_clean %>%
  group_by(station_id) %>%
  summarise(
    mean_aqi = mean(AQI, na.rm = TRUE),
    median_aqi = median(AQI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(station_id, mean_aqi), y = mean_aqi, fill = station_id)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average AQI by Station",
       x = "Station", y = "Average AQI") +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave("outputs/plots/aqi_by_station.png", p7, width = 10, height = 6, dpi = 300)

# 3.4 AQI distribution
p8 <- air_quality_clean %>%
  ggplot(aes(x = AQI, fill = AQI_category)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  labs(title = "Distribution of Air Quality Index",
       x = "AQI", y = "Frequency",
       fill = "AQI Category") +
  scale_fill_manual(values = c("Good" = "green", "Moderate" = "yellow", 
                               "Unhealthy_Sensitive" = "orange",
                               "Unhealthy" = "red", "Very_Unhealthy" = "purple",
                               "Hazardous" = "maroon")) +
  theme_minimal()

ggsave("outputs/plots/aqi_distribution.png", p8, width = 10, height = 6, dpi = 300)

cat("✓ Air quality plots saved\n\n")

# ==========================================
# 4. ENERGY VISUALIZATIONS
# ==========================================
cat("Creating energy visualizations...\n")

# 4.1 Energy consumption by building type
p9 <- energy_clean %>%
  group_by(building_type, hour) %>%
  summarise(avg_energy = mean(energy_consumption_kwh, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = hour, y = avg_energy, color = building_type)) +
  geom_line(size = 1) +
  labs(title = "Energy Consumption Patterns by Building Type",
       x = "Hour of Day", y = "Energy (kWh)",
       color = "Building Type") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("outputs/plots/energy_by_building.png", p9, width = 12, height = 6, dpi = 300)

# 4.2 Total energy consumption by type
p10 <- energy_clean %>%
  group_by(building_type) %>%
  summarise(total_energy = sum(energy_consumption_kwh, na.rm = TRUE) / 1000,
            .groups = "drop") %>%
  ggplot(aes(x = reorder(building_type, total_energy), y = total_energy, fill = building_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Total Energy Consumption by Building Type",
       x = "Building Type", y = "Energy (MWh)") +
  scale_fill_viridis_d() +
  theme_minimal()

ggsave("outputs/plots/energy_total.png", p10, width = 10, height = 6, dpi = 300)

# 4.3 Renewable energy usage
p11 <- energy_clean %>%
  group_by(building_type) %>%
  summarise(avg_renewable = mean(renewable_percent, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(building_type, avg_renewable), y = avg_renewable, fill = building_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Renewable Energy Usage",
       x = "Building Type", y = "Renewable Energy (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

ggsave("outputs/plots/renewable_energy.png", p11, width = 10, height = 6, dpi = 300)

# 4.4 Energy cost over time
p12 <- master_data %>%
  mutate(month = floor_date(timestamp, "month")) %>%
  group_by(month) %>%
  summarise(total_cost = sum(total_cost, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = month, y = total_cost)) +
  geom_line(color = custom_colors[4], size = 1.2) +
  geom_area(alpha = 0.3, fill = custom_colors[4]) +
  labs(title = "Monthly Energy Costs",
       x = "Month", y = "Cost (USD)") +
  theme_minimal()

ggsave("outputs/plots/energy_cost.png", p12, width = 12, height = 6, dpi = 300)

cat("✓ Energy plots saved\n\n")

# ==========================================
# 5. INTEGRATED VISUALIZATIONS
# ==========================================
cat("Creating integrated visualizations...\n")

# 5.1 Traffic vs Air Quality
p13 <- master_data %>%
  sample_n(min(5000, nrow(master_data))) %>%
  ggplot(aes(x = total_vehicles, y = avg_AQI)) +
  geom_point(aes(color = temperature), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "red", size = 1.2) +
  labs(title = "Traffic Volume vs Air Quality",
       x = "Total Vehicles", y = "Average AQI",
       color = "Temperature (°C)") +
  scale_color_viridis_c() +
  theme_minimal()

ggsave("outputs/plots/traffic_vs_aqi.png", p13, width = 10, height = 6, dpi = 300)

# 5.2 Heatmap - Hour vs Day of Week
heatmap_data <- master_data %>%
  group_by(hour, weekday) %>%
  summarise(avg_vehicles = mean(total_vehicles, na.rm = TRUE), .groups = "drop")

p14 <- ggplot(heatmap_data, aes(x = hour, y = weekday, fill = avg_vehicles)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Vehicles") +
  labs(title = "Traffic Heatmap: Hour vs Day of Week",
       x = "Hour of Day", y = "Day of Week") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal()

ggsave("outputs/plots/traffic_heatmap.png", p14, width = 12, height = 6, dpi = 300)

# 5.3 Multi-variable dashboard
p15 <- master_data %>%
  sample_n(min(1000, nrow(master_data))) %>%
  select(hour, total_vehicles, avg_AQI, total_energy_kwh, temperature) %>%
  pivot_longer(cols = c(total_vehicles, avg_AQI, total_energy_kwh, temperature),
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = hour, y = Value, color = Variable)) +
  geom_smooth(se = FALSE, size = 1) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Multi-Variable Patterns Throughout the Day",
       x = "Hour of Day", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/plots/multi_variable_patterns.png", p15, width = 12, height = 8, dpi = 300)

cat("✓ Integrated plots saved\n\n")

# ==========================================
# 6. INTERACTIVE PLOTLY VISUALIZATIONS
# ==========================================
cat("Creating interactive visualizations...\n")

# 6.1 Interactive time series
plot_data <- master_data %>%
  sample_n(min(2000, nrow(master_data)))

fig1 <- plot_ly(plot_data, x = ~timestamp) %>%
  add_trace(y = ~total_vehicles, name = "Traffic", type = "scatter", mode = "lines",
            line = list(color = custom_colors[1])) %>%
  add_trace(y = ~avg_AQI * 50, name = "AQI (scaled)", type = "scatter", mode = "lines",
            line = list(color = custom_colors[3]), yaxis = "y2") %>%
  layout(title = "Traffic and Air Quality Over Time (Interactive)",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Vehicle Count"),
         yaxis2 = list(title = "AQI", overlaying = "y", side = "right"))

htmlwidgets::saveWidget(fig1, "outputs/plots/interactive_timeseries.html")

cat("✓ Interactive plots saved\n\n")

# ==========================================
# 7. SUMMARY
# ==========================================
cat("==================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==================================================\n\n")

cat("Generated visualizations (", list.files("outputs/plots") %>% length(), "files):\n")
cat("  Traffic: 4 plots\n")
cat("  Air Quality: 4 plots\n")
cat("  Energy: 4 plots\n")
cat("  Integrated: 3 plots\n")
cat("  Interactive: 1 HTML file\n\n")

cat("All plots saved to outputs/plots/\n\n")

cat("==================================================\n")
cat("Next step: source('scripts/04_predictive_models.R')\n")
cat("==================================================\n")
