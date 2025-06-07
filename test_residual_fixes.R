 # Test script for residual transformation module
# This script will test Task F: Fix Model Residual Issues

cat("=== STARTING RESIDUAL TRANSFORMATION TEST ===\n")

# Load required libraries
cat("Loading configuration...\n")
source("modules/utils/config.R")
load_required_packages(quiet = TRUE)

# Source the new residual transformation module
cat("Loading residual transformation module...\n")
source("modules/analysis/residual_transformation.R")

# Source data loading modules
cat("Loading data modules...\n")
source("modules/data/fetch_dataset.R")

# Load and prepare data
cat("Loading and preparing data...\n")
tryCatch({
  medical_data <- fetch_dataset("dane.csv")
  cat("Data loaded successfully with", nrow(medical_data), "rows and", ncol(medical_data), "columns\n")
  
  # Simple data cleaning (remove missing values for this test)
  complete_data <- medical_data[complete.cases(medical_data), ]
  cat("After removing missing values:", nrow(complete_data), "complete cases\n")
  
  # Get numeric variables
  numeric_vars <- names(complete_data)[sapply(complete_data, is.numeric)]
  cat("Numeric variables found:", paste(numeric_vars, collapse = ", "), "\n")
  
  cat("\nStarting residual analysis...\n")
  
  # Apply residual fixes - test with just a few variables first
  test_vars <- numeric_vars[1:3]  # Test with first 3 variables
  cat("Testing with variables:", paste(test_vars, collapse = ", "), "\n")
  
  residual_analysis_result <- fix_model_residual_issues(complete_data, test_vars, group_column = "grupa")
  
  cat("\n=== TASK F COMPLETED ===\n")
  cat("Analysis completed successfully!\n")
  
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
  traceback()
})