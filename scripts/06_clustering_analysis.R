# Smart City Analytics - Clustering Analysis
# Identify patterns and group similar time periods/zones

cat("==================================================\n")
cat("STEP 6: Clustering Analysis\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(plotly)

# Set working directory
if (basename(getwd()) == "scripts") {
  setwd("..")
}

# Set seed for reproducibility
set.seed(42)

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
# 2. HOURLY PATTERN CLUSTERING
# ==========================================
cat("==================================================\n")
cat("Clustering by Hourly Patterns\n")
cat("==================================================\n\n")

# Aggregate data by hour to find typical patterns
hourly_patterns <- master_data %>%
  group_by(hour) %>%
  summarise(
    avg_traffic = mean(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    avg_energy = mean(total_energy_kwh, na.rm = TRUE),
    traffic_sd = sd(total_vehicles, na.rm = TRUE),
    aqi_sd = sd(avg_AQI, na.rm = TRUE),
    energy_sd = sd(total_energy_kwh, na.rm = TRUE)
  ) %>%
  ungroup()

# Standardize features for clustering
hourly_scaled <- hourly_patterns %>%
  select(avg_traffic, avg_aqi, avg_energy) %>%
  scale()

rownames(hourly_scaled) <- paste0("Hour_", hourly_patterns$hour)

# Determine optimal number of clusters using elbow method
cat("Determining optimal clusters (Elbow method)...\n")
wss <- sapply(1:8, function(k) {
  kmeans(hourly_scaled, centers = k, nstart = 25)$tot.withinss
})

# Create elbow plot
elbow_data <- data.frame(k = 1:8, wss = wss)
elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Within-cluster Sum of Squares") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:8)

if (!dir.exists("outputs")) dir.create("outputs")
ggsave("outputs/hourly_elbow_plot.png", elbow_plot, width = 10, height = 6)

# Use 4 clusters for hourly patterns (typical for city patterns: night, morning rush, midday, evening rush)
k_hours <- 4
hourly_kmeans <- kmeans(hourly_scaled, centers = k_hours, nstart = 25)

hourly_patterns$cluster <- factor(hourly_kmeans$cluster)

cat("Hourly clusters identified:\n\n")

# Analyze clusters
hourly_cluster_summary <- hourly_patterns %>%
  group_by(cluster) %>%
  summarise(
    hours = paste(hour, collapse = ", "),
    avg_traffic = mean(avg_traffic),
    avg_aqi = mean(avg_aqi),
    avg_energy = mean(avg_energy)
  ) %>%
  arrange(avg_traffic)

# Label clusters based on characteristics
cluster_labels <- c()
for (i in 1:nrow(hourly_cluster_summary)) {
  traffic <- hourly_cluster_summary$avg_traffic[i]
  hours <- as.numeric(unlist(strsplit(hourly_cluster_summary$hours[i], ", ")))
  
  if (all(hours %in% c(0, 1, 2, 3, 4, 5, 23))) {
    cluster_labels[i] <- "Night (Low Activity)"
  } else if (all(hours %in% c(6, 7, 8, 9, 10))) {
    cluster_labels[i] <- "Morning Rush"
  } else if (all(hours %in% c(11, 12, 13, 14, 15, 16))) {
    cluster_labels[i] <- "Midday"
  } else if (all(hours %in% c(17, 18, 19, 20, 21, 22))) {
    cluster_labels[i] <- "Evening Rush"
  } else {
    cluster_labels[i] <- paste("Pattern", i)
  }
}

hourly_cluster_summary$label <- cluster_labels
print(hourly_cluster_summary)

# Visualize hourly clusters
hourly_cluster_plot <- ggplot(hourly_patterns, aes(x = hour, y = avg_traffic, color = cluster)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Traffic Patterns by Hour Cluster",
       x = "Hour of Day",
       y = "Average Traffic Volume",
       color = "Cluster") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)

ggsave("outputs/hourly_cluster_plot.png", hourly_cluster_plot, width = 12, height = 6)

# ==========================================
# 3. DAILY PATTERN CLUSTERING
# ==========================================
cat("\n==================================================\n")
cat("Clustering by Daily Patterns\n")
cat("==================================================\n\n")

# Aggregate data by day
daily_patterns <- master_data %>%
  group_by(date) %>%
  summarise(
    total_traffic = sum(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    total_energy = sum(total_energy_kwh, na.rm = TRUE),
    max_traffic = max(total_vehicles, na.rm = TRUE),
    peak_hour = hour[which.max(total_vehicles)],
    is_weekend = first(is_weekend)
  ) %>%
  ungroup()

# Standardize features
daily_scaled <- daily_patterns %>%
  select(total_traffic, avg_aqi, total_energy, max_traffic) %>%
  scale()

# K-means clustering for daily patterns
k_days <- 3
daily_kmeans <- kmeans(daily_scaled, centers = k_days, nstart = 25)

daily_patterns$cluster <- factor(daily_kmeans$cluster)

cat("Daily clusters identified:\n\n")

daily_cluster_summary <- daily_patterns %>%
  group_by(cluster) %>%
  summarise(
    n_days = n(),
    pct_weekend = mean(is_weekend) * 100,
    avg_traffic = mean(total_traffic),
    avg_aqi = mean(avg_aqi),
    avg_energy = mean(total_energy)
  ) %>%
  mutate(
    label = case_when(
      avg_traffic == max(avg_traffic) ~ "High Activity Days",
      avg_traffic == min(avg_traffic) ~ "Low Activity Days",
      TRUE ~ "Moderate Activity Days"
    )
  )

print(daily_cluster_summary)

# Visualize daily clusters
daily_cluster_plot <- ggplot(daily_patterns, aes(x = total_traffic, y = total_energy, color = cluster)) +
  geom_point(size = 3, alpha = 0.6) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Daily Pattern Clusters",
       subtitle = "Traffic vs Energy Consumption",
       x = "Total Daily Traffic",
       y = "Total Daily Energy (kWh)",
       color = "Cluster") +
  theme_minimal()

ggsave("outputs/daily_cluster_plot.png", daily_cluster_plot, width = 10, height = 8)

# ==========================================
# 4. MULTI-METRIC CORRELATION PATTERNS
# ==========================================
cat("\n==================================================\n")
cat("Multi-Metric Correlation Analysis\n")
cat("==================================================\n\n")

# Calculate correlations between key metrics
correlation_data <- master_data %>%
  select(total_vehicles, avg_AQI, total_energy_kwh, temperature, humidity, 
         wind_speed, precipitation_mm)

cor_matrix <- cor(correlation_data, use = "complete.obs")

cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

# Create correlation heatmap
cor_melted <- reshape2::melt(cor_matrix)
cor_plot <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap",
       subtitle = "Smart City Metrics",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/correlation_heatmap.png", cor_plot, width = 10, height = 8)

# ==========================================
# 5. HIERARCHICAL CLUSTERING FOR WEEKDAYS
# ==========================================
cat("\n==================================================\n")
cat("Hierarchical Clustering by Weekday\n")
cat("==================================================\n\n")

# Aggregate by weekday
weekday_patterns <- master_data %>%
  group_by(weekday) %>%
  summarise(
    avg_traffic = mean(total_vehicles, na.rm = TRUE),
    avg_aqi = mean(avg_AQI, na.rm = TRUE),
    avg_energy = mean(total_energy_kwh, na.rm = TRUE),
    traffic_variability = sd(total_vehicles, na.rm = TRUE),
    peak_hour = names(which.max(table(hour[total_vehicles == max(total_vehicles)])))
  ) %>%
  ungroup()

# Standardize
weekday_scaled <- weekday_patterns %>%
  select(avg_traffic, avg_aqi, avg_energy, traffic_variability) %>%
  scale()

rownames(weekday_scaled) <- weekday_patterns$weekday

# Hierarchical clustering
hc <- hclust(dist(weekday_scaled), method = "ward.D2")

# Create dendrogram
png("outputs/weekday_dendrogram.png", width = 800, height = 600)
plot(hc, main = "Weekday Clustering Dendrogram",
     xlab = "Weekday", ylab = "Distance",
     sub = "Based on Traffic, AQI, and Energy Patterns")
rect.hclust(hc, k = 2, border = "red")
dev.off()

cat("Weekday clusters (k=2):\n")
weekday_clusters <- cutree(hc, k = 2)
weekday_patterns$cluster <- factor(weekday_clusters)
print(weekday_patterns %>% select(weekday, cluster, avg_traffic, avg_energy))

# ==========================================
# 6. PCA FOR DIMENSIONALITY INSIGHTS
# ==========================================
cat("\n==================================================\n")
cat("Principal Component Analysis\n")
cat("==================================================\n\n")

# Prepare features for PCA
pca_data <- master_data %>%
  select(total_vehicles, avg_AQI, total_energy_kwh, temperature, 
         humidity, wind_speed, hour, is_weekend) %>%
  mutate(is_weekend = as.numeric(is_weekend)) %>%
  na.omit()

# Perform PCA
pca_result <- prcomp(pca_data, scale. = TRUE)

# Summary
cat("PCA Variance Explained:\n")
var_explained <- summary(pca_result)$importance
print(var_explained[, 1:4])

# Create PCA visualization
pca_df <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  is_weekend = factor(pca_data$is_weekend, labels = c("Weekday", "Weekend"))
)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = is_weekend)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = c("steelblue", "coral")) +
  labs(title = "PCA: Weekday vs Weekend Patterns",
       subtitle = paste0("PC1 explains ", round(var_explained[2, 1] * 100, 1), "%, ",
                        "PC2 explains ", round(var_explained[2, 2] * 100, 1), "%"),
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Day Type") +
  theme_minimal()

ggsave("outputs/pca_plot.png", pca_plot, width = 10, height = 8)

# Loading plot
loadings_df <- data.frame(
  variable = rownames(pca_result$rotation),
  PC1 = pca_result$rotation[, 1],
  PC2 = pca_result$rotation[, 2]
)

loadings_plot <- ggplot(loadings_df, aes(x = PC1, y = PC2)) +
  geom_segment(aes(xend = 0, yend = 0), arrow = arrow(length = unit(0.2, "cm")),
               color = "steelblue", size = 1) +
  geom_text(aes(label = variable), hjust = -0.1, vjust = 0.5, size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "PCA Loadings Plot",
       subtitle = "Feature contributions to principal components",
       x = "PC1 Loading", y = "PC2 Loading") +
  theme_minimal() +
  coord_equal()

ggsave("outputs/pca_loadings.png", loadings_plot, width = 10, height = 8)

# ==========================================
# 6B. DBSCAN CLUSTERING FOR ANOMALY DETECTION
# ==========================================
cat("\n==================================================\n")
cat("DBSCAN Clustering for Anomaly Detection\n")
cat("==================================================\n\n")

library(dbscan)

# Prepare hourly data for DBSCAN (traffic, AQI, energy)
dbscan_data <- master_data %>%
  select(timestamp, hour, total_vehicles, avg_AQI, total_energy_kwh) %>%
  na.omit()

dbscan_features <- dbscan_data %>%
  select(total_vehicles, avg_AQI, total_energy_kwh) %>%
  scale()

# Determine optimal eps using k-nearest neighbor distance plot
knn_dist <- kNNdist(dbscan_features, k = 4)
knn_dist_sorted <- sort(knn_dist)

# Plot k-distance graph
png("outputs/dbscan_knn_distance.png", width = 800, height = 600)
plot(knn_dist_sorted, type = "l", 
     main = "k-NN Distance Plot for DBSCAN eps Selection",
     xlab = "Points (sorted by distance)", 
     ylab = "4-NN Distance",
     col = "steelblue", lwd = 2)
abline(h = 1.5, col = "red", lty = 2)
legend("bottomright", legend = "eps = 1.5", col = "red", lty = 2)
dev.off()

# Run DBSCAN with chosen parameters
eps_val <- 1.5
minPts_val <- 5

dbscan_result <- dbscan(dbscan_features, eps = eps_val, minPts = minPts_val)

dbscan_data$cluster <- factor(dbscan_result$cluster)

# Cluster 0 represents noise/anomalies
n_anomalies <- sum(dbscan_result$cluster == 0)
n_clusters <- length(unique(dbscan_result$cluster)) - 1  # Exclude noise cluster

cat("DBSCAN Results:\n")
cat("  - eps:", eps_val, "\n")
cat("  - minPts:", minPts_val, "\n")
cat("  - Clusters found:", n_clusters, "\n")
cat("  - Anomalies detected:", n_anomalies, "(", 
    round(n_anomalies / nrow(dbscan_data) * 100, 2), "%)\n\n")

# Analyze anomalies
anomalies <- dbscan_data %>%
  filter(cluster == 0) %>%
  mutate(
    traffic_zscore = (total_vehicles - mean(dbscan_data$total_vehicles)) / sd(dbscan_data$total_vehicles),
    aqi_zscore = (avg_AQI - mean(dbscan_data$avg_AQI)) / sd(dbscan_data$avg_AQI),
    energy_zscore = (total_energy_kwh - mean(dbscan_data$total_energy_kwh)) / sd(dbscan_data$total_energy_kwh)
  )

if (nrow(anomalies) > 0) {
  cat("Anomaly characteristics:\n")
  cat("  - Hour distribution:\n")
  print(table(anomalies$hour))
  
  cat("\n  - Extreme anomalies (high traffic + high AQI):\n")
  extreme_anomalies <- anomalies %>%
    filter(traffic_zscore > 2 | aqi_zscore > 2) %>%
    arrange(desc(traffic_zscore))
  
  if (nrow(extreme_anomalies) > 0) {
    print(head(extreme_anomalies %>% select(timestamp, hour, total_vehicles, avg_AQI, total_energy_kwh), 10))
  }
}

# Visualize DBSCAN results
dbscan_plot <- ggplot(dbscan_data, aes(x = total_vehicles, y = avg_AQI, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("0" = "red", "1" = "steelblue", "2" = "forestgreen", 
                                "3" = "orange", "4" = "purple"),
                     labels = function(x) ifelse(x == "0", "Anomaly", paste("Cluster", x))) +
  labs(title = "DBSCAN Clustering: Traffic vs Air Quality",
       subtitle = paste("Anomalies detected:", n_anomalies, "points"),
       x = "Total Vehicles",
       y = "Average AQI",
       color = "Cluster") +
  theme_minimal()

ggsave("outputs/dbscan_clustering.png", dbscan_plot, width = 10, height = 8)

# 3D visualization of DBSCAN results
dbscan_3d <- plot_ly(dbscan_data, 
                     x = ~total_vehicles, 
                     y = ~avg_AQI, 
                     z = ~total_energy_kwh,
                     color = ~cluster,
                     colors = c("red", "steelblue", "forestgreen", "orange"),
                     type = "scatter3d", 
                     mode = "markers",
                     marker = list(size = 3, opacity = 0.6)) %>%
  layout(title = "DBSCAN Clustering (3D)",
         scene = list(
           xaxis = list(title = "Traffic"),
           yaxis = list(title = "AQI"),
           zaxis = list(title = "Energy (kWh)")
         ))

htmlwidgets::saveWidget(dbscan_3d, "outputs/dbscan_3d.html", selfcontained = TRUE)
cat("✓ Interactive 3D DBSCAN plot saved to outputs/dbscan_3d.html\n")

# ==========================================
# 7. SAVE RESULTS
# ==========================================
cat("\n==================================================\n")
cat("Saving Results\n")
cat("==================================================\n\n")

# Save clustering results
clustering_results <- list(
  hourly_patterns = hourly_patterns,
  hourly_cluster_summary = hourly_cluster_summary,
  daily_patterns = daily_patterns,
  daily_cluster_summary = daily_cluster_summary,
  weekday_patterns = weekday_patterns,
  correlation_matrix = cor_matrix,
  pca_result = pca_result,
  pca_variance = var_explained,
  dbscan_result = dbscan_result,
  dbscan_data = dbscan_data,
  anomalies = anomalies
)

saveRDS(clustering_results, "data/processed/clustering_results.rds")
cat("✓ Clustering results saved to data/processed/clustering_results.rds\n")

# ==========================================
# 8. SUMMARY REPORT
# ==========================================
cat("\n==================================================\n")
cat("CLUSTERING ANALYSIS SUMMARY\n")
cat("==================================================\n\n")

cat("HOURLY PATTERNS:\n")
cat("  - Identified", k_hours, "distinct hourly patterns\n")
cat("  - Patterns correspond to: night, morning rush, midday, evening rush\n\n")

cat("DAILY PATTERNS:\n")
cat("  - Identified", k_days, "types of days based on activity levels\n")
cat("  - High activity days have significantly higher traffic and energy use\n\n")

cat("KEY CORRELATIONS:\n")
cat("  - Traffic-Energy correlation:", round(cor_matrix["total_vehicles", "total_energy_kwh"], 3), "\n")
cat("  - Traffic-AQI correlation:", round(cor_matrix["total_vehicles", "avg_AQI"], 3), "\n")
cat("  - Temperature-Energy correlation:", round(cor_matrix["temperature", "total_energy_kwh"], 3), "\n\n")

cat("PCA INSIGHTS:\n")
cat("  - First 2 components explain", round(sum(var_explained[2, 1:2]) * 100, 1), "% of variance\n")
cat("  - Clear separation between weekday and weekend patterns\n\n")

cat("DBSCAN ANOMALY DETECTION:\n")
cat("  - Detected", n_anomalies, "anomalous time periods\n")
cat("  - Anomaly rate:", round(n_anomalies / nrow(dbscan_data) * 100, 2), "%\n")
cat("  - Useful for identifying unusual traffic/pollution events\n\n")

cat("Generated visualizations saved to outputs/:\n")
cat("  - hourly_elbow_plot.png\n")
cat("  - hourly_cluster_plot.png\n")
cat("  - daily_cluster_plot.png\n")
cat("  - correlation_heatmap.png\n")
cat("  - weekday_dendrogram.png\n")
cat("  - pca_plot.png\n")
cat("  - pca_loadings.png\n")
cat("  - dbscan_clustering.png\n")
cat("  - dbscan_knn_distance.png\n")
cat("  - dbscan_3d.html (interactive)\n")

cat("\n==================================================\n")
cat("Clustering analysis complete!\n")
cat("==================================================\n")
