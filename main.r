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
main <- function() {
  cat("Medical Data Statistical Analysis Tool\n")
  cat("=====================================\n\n")
  
  # For demonstration, use the default data file
  data_file <- "dane.csv"
  
  cat("1. LOADING AND VALIDATING DATA\n")
  cat("------------------------------\n")
  
  # Load the dataset using our fetch_dataset module
  tryCatch({
    medical_data <- fetch_dataset(data_file)
    
    cat("\n2. DATA OVERVIEW\n")
    cat("----------------\n")
    
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
    
    cat("\n3. DATA QUALITY ASSESSMENT\n")
    cat("--------------------------\n")
    
    # Check for missing values
    missing_summary <- sapply(medical_data, function(x) sum(is.na(x)))
    missing_summary <- missing_summary[missing_summary > 0]
    
    if (length(missing_summary) > 0) {
      cat("Missing values found:\n")
      for (var in names(missing_summary)) {
        pct_missing <- round(100 * missing_summary[var] / nrow(medical_data), 1)
        cat(sprintf("- %s: %d missing values (%.1f%%)\n", 
                    var, missing_summary[var], pct_missing))
      }
    } else {
      cat("No missing values detected.\n")
    }
    
    cat("\n4. BASIC STATISTICAL OVERVIEW\n")
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
    
    cat("\n=====================================\n")
    cat("Data loading and inspection completed successfully!\n")
    cat("Dataset is ready for statistical analysis.\n")
    cat("=====================================\n")
    
    # Return the loaded data for further analysis
    return(medical_data)
    
  }, error = function(e) {
    cat("ERROR: Failed to load dataset\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Function to demonstrate specific functionality
demo_data_loading <- function(file_path = "dane.csv") {
  cat("=== DATA LOADING DEMONSTRATION ===\n\n")
  
  cat("Testing file validation...\n")
  validation_result <- validate_file_path(file_path)
  cat("Validation result:", validation_result, "\n\n")
  
  if (validation_result) {
    cat("Testing encoding detection...\n")
    detected_encoding <- detect_encoding(file_path)
    cat("Detected encoding:", detected_encoding, "\n\n")
    
    cat("Loading dataset...\n")
    data <- fetch_dataset(file_path)
    
    return(data)
  } else {
    cat("File validation failed. Cannot proceed with data loading.\n")
    return(NULL)
  }
}

# Execute main function if script is run directly
if (!interactive()) {
  main()
} else {
  # If running interactively, provide helper message
  cat("Medical Data Analysis Tool loaded.\n")
  cat("To run the main analysis: main()\n")
  cat("To demo data loading: demo_data_loading()\n")
}
