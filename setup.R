# Smart City Analytics - Package Setup Script
# This script installs all required R packages for the project

cat("==================================================\n")
cat("Smart City Data Analytics System - Setup\n")
cat("==================================================\n\n")

# List of required packages
required_packages <- c(
  # Data manipulation
  "dplyr",
  "tidyr",
  "lubridate",
  "data.table",
  
  # Visualization
  "ggplot2",
  "plotly",
  "leaflet",
  "RColorBrewer",
  "gridExtra",
  "viridis",
  
  # Dashboard
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "DT",
  
  # Modeling
  "caret",
  "forecast",
  "randomForest",
  "e1071",
  "Metrics",
  
  # Statistical analysis
  "corrplot",
  "reshape2",
  
  # Clustering and dimensionality reduction
  "cluster",
  "factoextra",
  "zoo",
  
  # Report generation
  "rmarkdown",
  "knitr"
)

cat("Checking and installing required packages...\n\n")

# Function to install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", package))
    install.packages(package, dependencies = TRUE, repos = "https://cran.r-project.org")
    library(package, character.only = TRUE)
    cat(sprintf("✓ %s installed successfully\n", package))
  } else {
    cat(sprintf("✓ %s already installed\n", package))
  }
}

# Install all packages
for (pkg in required_packages) {
  tryCatch({
    install_if_missing(pkg)
  }, error = function(e) {
    cat(sprintf("✗ Error installing %s: %s\n", pkg, e$message))
  })
}

cat("\n==================================================\n")
cat("Setup complete! All packages are ready.\n")
cat("==================================================\n\n")
cat("Next steps:\n")
cat("1. Run: source('scripts/01_data_preprocessing.R')\n")
cat("2. Or launch dashboard: shiny::runApp('shiny_app')\n")
