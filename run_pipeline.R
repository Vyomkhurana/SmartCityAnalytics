# Smart City Analytics - End-to-End Pipeline Runner
# Runs the complete analytics workflow in the correct order.

cat("==================================================\n")
cat("SMART CITY ANALYTICS - FULL PIPELINE\n")
cat("==================================================\n\n")

if (basename(getwd()) == "scripts") {
  setwd("..")
}

required_paths <- c("scripts", "data", "outputs", "models")
missing_paths <- required_paths[!dir.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop(paste("Project structure not found. Missing:", paste(missing_paths, collapse = ", ")))
}

if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
if (!dir.exists("outputs/plots")) dir.create("outputs/plots", recursive = TRUE)
if (!dir.exists("models")) dir.create("models", recursive = TRUE)

pipeline_scripts <- c(
  "scripts/01_data_preprocessing.R",
  "scripts/02_exploratory_analysis.R",
  "scripts/03_visualization.R",
  "scripts/04_predictive_models.R",
  "scripts/05_anomaly_detection.R",
  "scripts/06_clustering_analysis.R"
)

for (script_path in pipeline_scripts) {
  cat("Running:", script_path, "\n")
  source(script_path)
  cat("Completed:", script_path, "\n\n")
}

cat("==================================================\n")
cat("PIPELINE COMPLETE\n")
cat("==================================================\n")
cat("Run dashboard with: shiny::runApp('shiny_app')\n")
