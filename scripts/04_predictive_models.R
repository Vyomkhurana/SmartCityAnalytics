# Smart City Analytics - Predictive Models
# This script builds and evaluates predictive models

cat("==================================================\n")
cat("STEP 4: Predictive Modeling\n")
cat("==================================================\n\n")

# Load required libraries
library(dplyr)
library(caret)
library(xgboost)
library(forecast)
library(Metrics)
library(ggplot2)

# Set working directory
if (basename(getwd()) == "scripts") {
  setwd("..")
}

if (!dir.exists("models")) {
  dir.create("models", recursive = TRUE)
}
if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}
if (!dir.exists("outputs/plots")) {
  dir.create("outputs/plots", recursive = TRUE)
}

# Set seed for reproducibility
set.seed(42)

train_xgb_regressor <- function(train_df, test_df, response_col, feature_cols, model_label) {
  cat("Training XGBoost model for", model_label, "...\n")

  x_train <- data.matrix(train_df[, feature_cols, drop = FALSE])
  x_test <- data.matrix(test_df[, feature_cols, drop = FALSE])
  y_train <- train_df[[response_col]]
  y_test <- test_df[[response_col]]

  dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train, missing = NA)
  dtest <- xgboost::xgb.DMatrix(data = x_test, label = y_test, missing = NA)

  xgb_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    gamma = 0
  )

  model_obj <- xgboost::xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = 200,
    evals = list(train = dtrain, eval = dtest),
    verbose = 0,
    early_stopping_rounds = 20
  )

  predictions <- predict(model_obj, x_test)
  actual <- test_df[[response_col]]

  rmse_val <- rmse(actual, predictions)
  mae_val <- mae(actual, predictions)
  r2_val <- cor(actual, predictions)^2

  importance_raw <- tryCatch(xgboost::xgb.importance(feature_names = feature_cols, model = model_obj), error = function(e) NULL)
  importance_df <- if (!is.null(importance_raw) && nrow(importance_raw) > 0) {
    data.frame(
      feature = importance_raw$Feature,
      importance = importance_raw$Gain
    ) %>%
      arrange(desc(importance)) %>%
      head(5)
  } else {
    data.frame(feature = character(0), importance = numeric(0))
  }

  list(
    model = list(
      booster = model_obj,
      feature_names = feature_cols,
      model_type = "XGBoost"
    ),
    predictions = predictions,
    rmse = rmse_val,
    mae = mae_val,
    r2 = r2_val,
    importance = importance_df
  )
}

# ==========================================
# 1. LOAD AND PREPARE DATA
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
# 2. FEATURE ENGINEERING
# ==========================================
cat("Engineering features...\n")

# Create lag features and additional predictors
master_data <- master_data %>%
  arrange(timestamp) %>%
  mutate(
    # Lag features
    vehicles_lag1 = lag(total_vehicles, 1),
    vehicles_lag24 = lag(total_vehicles, 24),
    aqi_lag1 = lag(avg_AQI, 1),
    aqi_lag24 = lag(avg_AQI, 24),
    energy_lag1 = lag(total_energy_kwh, 1),
    energy_lag24 = lag(total_energy_kwh, 24),
    
    # Rolling averages
    vehicles_ma7 = zoo::rollapply(total_vehicles, 7, mean, fill = NA, align = "right"),
    aqi_ma7 = zoo::rollapply(avg_AQI, 7, mean, fill = NA, align = "right"),
    energy_ma7 = zoo::rollapply(total_energy_kwh, 7, mean, fill = NA, align = "right"),
    
    # Cyclical features
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24),
    
    # Categorical encoding
    is_weekend_num = as.numeric(is_weekend),
    weekday_num = as.numeric(weekday)
  ) %>%
  na.omit()

cat("✓ Features engineered\n\n")

# ==========================================
# 3. TRAIN-TEST SPLIT
# ==========================================
cat("Splitting data...\n")

# 80-20 train-test split (chronological)
split_idx <- floor(0.8 * nrow(master_data))

train_data <- master_data[1:split_idx, ]
test_data <- master_data[(split_idx + 1):nrow(master_data), ]

cat("  Training set:", nrow(train_data), "records\n")
cat("  Test set:", nrow(test_data), "records\n\n")

# ==========================================
# 4. MODEL 1: TRAFFIC PREDICTION
# ==========================================
cat("==================================================\n")
cat("MODEL 1: Traffic Volume Prediction\n")
cat("==================================================\n\n")

# Select features for traffic prediction
traffic_features <- c("hour", "is_weekend_num", "weekday_num", 
                     "vehicles_lag1", "vehicles_lag24", "vehicles_ma7",
                     "hour_sin", "hour_cos", "temperature", "humidity",
                     "avg_AQI", "precipitation_mm")

traffic_formula <- as.formula(paste("total_vehicles ~", paste(traffic_features, collapse = " + ")))

traffic_fit <- train_xgb_regressor(
  train_data,
  test_data,
  "total_vehicles",
  traffic_features,
  "Traffic Volume Prediction"
)

traffic_model <- traffic_fit$model
traffic_pred <- traffic_fit$predictions
traffic_rmse <- traffic_fit$rmse
traffic_mae <- traffic_fit$mae
traffic_r2 <- traffic_fit$r2

cat("\nTraffic Model Performance:\n")
cat("  RMSE:", round(traffic_rmse, 2), "\n")
cat("  MAE:", round(traffic_mae, 2), "\n")
cat("  R²:", round(traffic_r2, 4), "\n\n")

# Feature importance
cat("Top features for traffic prediction:\n")
importance_df <- traffic_fit$importance
print(importance_df)
cat("\n")

# Save model
saveRDS(traffic_model, "models/traffic_model.rds")
cat("✓ Traffic model saved\n\n")

# ==========================================
# 5. MODEL 2: AIR QUALITY PREDICTION
# ==========================================
cat("==================================================\n")
cat("MODEL 2: Air Quality (AQI) Prediction\n")
cat("==================================================\n\n")

# Select features for AQI prediction
aqi_features <- c("hour", "is_weekend_num", "weekday_num",
                 "aqi_lag1", "aqi_lag24", "aqi_ma7",
                 "hour_sin", "hour_cos", "temperature", "humidity",
                 "wind_speed", "total_vehicles", "avg_NO2")

aqi_formula <- as.formula(paste("avg_AQI ~", paste(aqi_features, collapse = " + ")))

aqi_fit <- train_xgb_regressor(
  train_data,
  test_data,
  "avg_AQI",
  aqi_features,
  "Air Quality (AQI) Prediction"
)

aqi_model <- aqi_fit$model
aqi_pred <- aqi_fit$predictions
aqi_rmse <- aqi_fit$rmse
aqi_mae <- aqi_fit$mae
aqi_r2 <- aqi_fit$r2

cat("\nAQI Model Performance:\n")
cat("  RMSE:", round(aqi_rmse, 2), "\n")
cat("  MAE:", round(aqi_mae, 2), "\n")
cat("  R²:", round(aqi_r2, 4), "\n\n")

# Feature importance
cat("Top features for AQI prediction:\n")
importance_df <- aqi_fit$importance
print(importance_df)
cat("\n")

# Save model
saveRDS(aqi_model, "models/aqi_model.rds")
cat("✓ AQI model saved\n\n")

# ==========================================
# 6. MODEL 3: ENERGY CONSUMPTION PREDICTION
# ==========================================
cat("==================================================\n")
cat("MODEL 3: Energy Consumption Prediction\n")
cat("==================================================\n\n")

# Select features for energy prediction
energy_features <- c("hour", "is_weekend_num", "weekday_num",
                    "energy_lag1", "energy_lag24", "energy_ma7",
                    "hour_sin", "hour_cos", "temperature", "humidity",
                    "total_vehicles")

energy_formula <- as.formula(paste("total_energy_kwh ~", paste(energy_features, collapse = " + ")))

energy_fit <- train_xgb_regressor(
  train_data,
  test_data,
  "total_energy_kwh",
  energy_features,
  "Energy Consumption Prediction"
)

energy_model <- energy_fit$model
energy_pred <- energy_fit$predictions
energy_rmse <- energy_fit$rmse
energy_mae <- energy_fit$mae
energy_r2 <- energy_fit$r2

cat("\nEnergy Model Performance:\n")
cat("  RMSE:", round(energy_rmse, 2), "kWh\n")
cat("  MAE:", round(energy_mae, 2), "kWh\n")
cat("  R²:", round(energy_r2, 4), "\n\n")

# Feature importance
cat("Top features for energy prediction:\n")
importance_df <- energy_fit$importance
print(importance_df)
cat("\n")

# Save model
saveRDS(energy_model, "models/energy_model.rds")
cat("✓ Energy model saved\n\n")

# ==========================================
# 7. VISUALIZATION OF PREDICTIONS
# ==========================================
cat("Creating prediction visualizations...\n")

# Create comparison plots
library(gridExtra)

# Traffic predictions
p1 <- data.frame(
  actual = test_data$total_vehicles[1:min(500, nrow(test_data))],
  predicted = traffic_pred[1:min(500, length(traffic_pred))],
  time = 1:min(500, length(traffic_pred))
) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual"), size = 0.8) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 0.8, alpha = 0.7) +
  labs(title = "Traffic Prediction vs Actual",
       x = "Time", y = "Vehicle Count", color = "") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

ggsave("outputs/plots/traffic_predictions.png", p1, width = 12, height = 6, dpi = 300)

# AQI predictions
p2 <- data.frame(
  actual = test_data$avg_AQI[1:min(500, nrow(test_data))],
  predicted = aqi_pred[1:min(500, length(aqi_pred))],
  time = 1:min(500, length(aqi_pred))
) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual"), size = 0.8) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 0.8, alpha = 0.7) +
  labs(title = "AQI Prediction vs Actual",
       x = "Time", y = "AQI", color = "") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

ggsave("outputs/plots/aqi_predictions.png", p2, width = 12, height = 6, dpi = 300)

# Energy predictions
p3 <- data.frame(
  actual = test_data$total_energy_kwh[1:min(500, nrow(test_data))],
  predicted = energy_pred[1:min(500, length(energy_pred))],
  time = 1:min(500, length(energy_pred))
) %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual"), size = 0.8) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 0.8, alpha = 0.7) +
  labs(title = "Energy Prediction vs Actual",
       x = "Time", y = "Energy (kWh)", color = "") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()

ggsave("outputs/plots/energy_predictions.png", p3, width = 12, height = 6, dpi = 300)

cat("✓ Prediction plots saved\n\n")

# ==========================================
# 8. SAVE MODEL RESULTS
# ==========================================
cat("Saving model results...\n")

model_results <- list(
  traffic = list(
    model_name = "XGBoost",
    model = traffic_model,
    rmse = traffic_rmse,
    mae = traffic_mae,
    r2 = traffic_r2,
    predictions = traffic_pred[1:100]
  ),
  aqi = list(
    model_name = "XGBoost",
    model = aqi_model,
    rmse = aqi_rmse,
    mae = aqi_mae,
    r2 = aqi_r2,
    predictions = aqi_pred[1:100]
  ),
  energy = list(
    model_name = "XGBoost",
    model = energy_model,
    rmse = energy_rmse,
    mae = energy_mae,
    r2 = energy_r2,
    predictions = energy_pred[1:100]
  )
)

saveRDS(model_results, "outputs/model_results.rds")

# Save summary report
sink("outputs/model_performance_summary.txt")
cat("SMART CITY ANALYTICS - MODEL PERFORMANCE SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")
cat("Generated:", Sys.time(), "\n\n")
cat("Training Records:", nrow(train_data), "\n")
cat("Test Records:", nrow(test_data), "\n\n")
cat("\n1. TRAFFIC MODEL\n")
cat("  RMSE:", round(traffic_rmse, 2), "\n")
cat("  MAE:", round(traffic_mae, 2), "\n")
cat("  R²:", round(traffic_r2, 4), "\n\n")
cat("2. AQI MODEL\n")
cat("  RMSE:", round(aqi_rmse, 2), "\n")
cat("  MAE:", round(aqi_mae, 2), "\n")
cat("  R²:", round(aqi_r2, 4), "\n\n")
cat("3. ENERGY MODEL\n")
cat("  RMSE:", round(energy_rmse, 2), "kWh\n")
cat("  MAE:", round(energy_mae, 2), "kWh\n")
cat("  R²:", round(energy_r2, 4), "\n")
sink()

cat("✓ Model results saved\n\n")

# ==========================================
# 9. SUMMARY
# ==========================================
cat("==================================================\n")
cat("PREDICTIVE MODELING COMPLETE\n")
cat("==================================================\n\n")

cat("Models trained and saved:\n")
cat("  ✓ Traffic Prediction Model (R² =", round(traffic_r2, 3), ")\n")
cat("  ✓ AQI Prediction Model (R² =", round(aqi_r2, 3), ")\n")
cat("  ✓ Energy Prediction Model (R² =", round(energy_r2, 3), ")\n\n")

cat("Files saved:\n")
cat("  - models/traffic_model.rds\n")
cat("  - models/aqi_model.rds\n")
cat("  - models/energy_model.rds\n")
cat("  - outputs/model_results.rds\n")
cat("  - outputs/model_performance_summary.txt\n")
cat("  - outputs/plots/*_predictions.png\n\n")

cat("==================================================\n")
cat("Next step: Launch Shiny dashboard\n")
cat("Run: shiny::runApp('shiny_app')\n")
cat("==================================================\n")
