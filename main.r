# Medical Data Statistical Analysis Tool
# Main entry point for command-line execution
# Author: SAD Project Team
# Usage: Rscript main.R --input data.csv --output results/ --groups group_column

# Source all required modules
source("modules/utils/config.R")
source("modules/utils/utils.R")
source("modules/utils/logging.R")

source("modules/data/fetch_dataset.R")
source("modules/data/repair_dataset.R")
source("modules/data/validate_data.R")

source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")
source("modules/analysis/statistical_tests.R")

source("modules/visualization/create_plots.R")
source("modules/visualization/plot_utils.R")

source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")

# Main function to orchestrate the analysis
main <- function(repair_data = TRUE, missing_method = "regression", outlier_method = "iqr") {
  cat("Medical Data Statistical Analysis Tool\n")
  cat("=====================================\n\n")
  
  # For demonstration, use the default data file
  data_file <- "dane.csv"
  
  cat("1. LOADING AND VALIDATING DATA\n")
  cat("------------------------------\n")
  
  # Load the dataset using our fetch_dataset module
  tryCatch({
    medical_data <- fetch_dataset(data_file)
    
    # Store original data for comparison
    original_data <- medical_data
    
    # Step 2: Data Repair (if requested)
    if (repair_data) {
      cat("\n=== DATA REPAIR PHASE ===\n")
      
      repair_result <- repair_dataset(
        data = medical_data,
        missing_threshold = 0.05,  # Repair if >5% missing
        outlier_method = outlier_method,
        missing_method = missing_method,
        outlier_action = "winsorize"
      )
      
      # Use repaired data
      medical_data <- repair_result$data
      repair_log <- repair_result$repair_log
      
      cat("\nData repair completed. Proceeding with analysis on cleaned data.\n")
    } else {
      cat("\n=== SKIPPING DATA REPAIR ===\n")
      cat("Proceeding with original data (may contain missing values and outliers)\n")
      repair_log <- NULL
    }
    
    cat("\n2. DATA OVERVIEW (POST-PROCESSING)\n")
    cat("----------------------------------\n")
    
    # Display first few rows
    cat("First 5 rows of the dataset:\n")
    print(head(medical_data, 5))
    
    cat("\nDataset summary:\n")
    print(summary(medical_data))
    
    # Show group composition
    if ("grupa" %in% names(medical_data)) {
      cat("\nGroup composition:\n")
      group_table <- table(medical_data$grupa)
      print(group_table)
      
      cat("\nGroup percentages:\n")
      print(round(prop.table(group_table) * 100, 1))
    }
    
    # Show gender distribution by group
    if ("plec" %in% names(medical_data) && "grupa" %in% names(medical_data)) {
      cat("\nGender distribution by group:\n")
      print(table(medical_data$grupa, medical_data$plec))
    }
    
    # Basic age statistics by group
    if ("wiek" %in% names(medical_data) && "grupa" %in% names(medical_data)) {
      cat("\nAge statistics by group:\n")
      age_by_group <- aggregate(wiek ~ grupa, data = medical_data, 
                                FUN = function(x) c(
                                  mean = round(mean(x, na.rm = TRUE), 1),
                                  sd = round(sd(x, na.rm = TRUE), 1),
                                  min = min(x, na.rm = TRUE),
                                  max = max(x, na.rm = TRUE)
                                ))
      print(age_by_group)
    }
    
    cat("\n3. DATA QUALITY ASSESSMENT (FINAL)\n")
    cat("----------------------------------\n")
    
    # Check for missing values
    missing_summary <- sapply(medical_data, function(x) sum(is.na(x)))
    missing_summary <- missing_summary[missing_summary > 0]
    
    if (length(missing_summary) > 0) {
      cat("Remaining missing values:\n")
      for (var in names(missing_summary)) {
        pct_missing <- round(100 * missing_summary[var] / nrow(medical_data), 1)
        cat(sprintf("- %s: %d missing values (%.1f%%)\n", 
                    var, missing_summary[var], pct_missing))
      }
    } else {
      cat("No missing values detected in final dataset.\n")
    }
    
    # Compare original vs processed data
    if (repair_data && !is.null(repair_log)) {
      cat("\n4. BEFORE/AFTER COMPARISON\n")
      cat("--------------------------\n")
      
      original_missing <- sum(sapply(original_data, function(x) sum(is.na(x))))
      final_missing <- sum(sapply(medical_data, function(x) sum(is.na(x))))
      
      cat(sprintf("Original missing values: %d\n", original_missing))
      cat(sprintf("Final missing values: %d\n", final_missing))
      cat(sprintf("Missing values reduced by: %d (%.1f%%)\n", 
                  original_missing - final_missing,
                  100 * (original_missing - final_missing) / max(original_missing, 1)))
      
      cat(sprintf("Original dimensions: %d x %d\n", nrow(original_data), ncol(original_data)))
      cat(sprintf("Final dimensions: %d x %d\n", nrow(medical_data), ncol(medical_data)))
    }
    
    cat("\n5. BASIC STATISTICAL OVERVIEW\n")
    cat("-----------------------------\n")
    
    # Show basic statistics for key medical parameters
    medical_params <- c("hsCRP", "ERY", "PLT", "HGB", "HCT", "MCHC", "MON", "LEU")
    available_params <- medical_params[medical_params %in% names(medical_data)]
    
    if (length(available_params) > 0) {
      cat("Key medical parameters overview:\n")
      for (param in available_params) {
        if (is.numeric(medical_data[[param]])) {
          param_stats <- summary(medical_data[[param]])
          cat(sprintf("\n%s:\n", param))
          print(param_stats)
        }
      }
    }
    
    cat("\n6. DATA READINESS ASSESSMENT\n")
    cat("-----------------------------\n")
    
    # Assess if data is ready for statistical analysis
    readiness_issues <- c()
    
    if (sum(sapply(medical_data, function(x) sum(is.na(x)))) > 0) {
      readiness_issues <- c(readiness_issues, "Missing values present")
    }
    
    if (nrow(medical_data) < 30) {
      readiness_issues <- c(readiness_issues, "Small sample size (n < 30)")
    }
    
    numeric_count <- sum(sapply(medical_data, is.numeric))
    if (numeric_count < 3) {
      readiness_issues <- c(readiness_issues, "Few numeric variables for analysis")
    }
    
    if (length(readiness_issues) == 0) {
      cat("✓ Dataset is ready for statistical analysis!\n")
      cat("✓ No major data quality issues detected.\n")
      cat("✓ Sufficient sample size and variables available.\n")
    } else {
      cat("⚠ Data readiness issues detected:\n")
      for (issue in readiness_issues) {
        cat(sprintf("  - %s\n", issue))
      }
    }
    
    cat("\n=====================================\n")
    cat("Data preprocessing pipeline completed!\n")
    cat("Dataset is prepared for statistical analysis.\n")
    cat("=====================================\n")
    
    # Return the processed data and logs
    return(list(
      data = medical_data,
      original_data = original_data,
      repair_log = repair_log,
      readiness_issues = readiness_issues
    ))
    
  }, error = function(e) {
    cat("ERROR: Failed to process dataset\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Function to demonstrate specific repair functionality
demo_data_repair <- function(file_path = "dane.csv", method = "regression") {
  cat("=== DATA REPAIR DEMONSTRATION ===\n\n")
  
  # Load data
  cat("1. Loading data...\n")
  data <- fetch_dataset(file_path)
  
  if (is.null(data)) {
    cat("Failed to load data. Exiting demo.\n")
    return(NULL)
  }
  
  # Show original data issues
  cat("\n2. Original data analysis...\n")
  original_missing <- sum(sapply(data, function(x) sum(is.na(x))))
  cat(sprintf("Original missing values: %d\n", original_missing))
  
  # Perform repair
  cat("\n3. Performing data repair...\n")
  repair_result <- repair_dataset(
    data = data,
    missing_threshold = 0.01,  # Very low threshold for demo
    missing_method = method,
    outlier_method = "iqr",
    outlier_action = "winsorize"
  )
  
  # Show results
  cat("\n4. Repair results...\n")
  final_missing <- sum(sapply(repair_result$data, function(x) sum(is.na(x))))
  cat(sprintf("Final missing values: %d\n", final_missing))
  cat(sprintf("Missing values reduced by: %d\n", original_missing - final_missing))
  
  return(repair_result)
}

# Function to compare different repair methods
compare_repair_methods <- function(file_path = "dane.csv") {
  cat("=== COMPARING REPAIR METHODS ===\n\n")
  
  # Load original data
  original_data <- fetch_dataset(file_path)
  if (is.null(original_data)) return(NULL)
  
  methods <- c("mean_median", "regression", "mice")
  results <- list()
  
  for (method in methods) {
    cat(sprintf("\nTesting method: %s\n", method))
    cat("--------------------------------\n")
    
    tryCatch({
      repair_result <- repair_dataset(
        data = original_data,
        missing_threshold = 0.01,
        missing_method = method,
        outlier_method = "iqr",
        outlier_action = "winsorize"
      )
      
      results[[method]] <- repair_result
      
    }, error = function(e) {
      cat(sprintf("Method %s failed: %s\n", method, e$message))
      results[[method]] <- NULL
    })
  }
  
  # Compare results
  cat("\n=== METHOD COMPARISON SUMMARY ===\n")
  for (method in methods) {
    if (!is.null(results[[method]])) {
      missing_after <- sum(sapply(results[[method]]$data, function(x) sum(is.na(x))))
      cat(sprintf("%-12s: %d missing values remaining\n", method, missing_after))
    } else {
      cat(sprintf("%-12s: FAILED\n", method))
    }
  }
  
  return(results)
}

# Execute main function if script is run directly
if (!interactive()) {
  main()
} else {
  # If running interactively, provide helper message
  cat("Medical Data Analysis Tool loaded.\n")
  cat("Available functions:\n")
  cat("- main(): Run complete analysis pipeline\n")
  cat("- demo_data_repair(): Demonstrate repair functionality\n")
  cat("- compare_repair_methods(): Compare different repair methods\n")
}
