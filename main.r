# Medical Data Statistical Analysis Tool
# Main entry point for command-line execution
# Author: SAD Project Team
# Usage: Rscript main.R --input data.csv --output results/ --groups group_column

# Source all required modules
source("modules/utils/config.R")
source("modules/utils/utils.R")
source("modules/utils/logging.R")

source("modules/data/fetch_dataset.R")
source("modules/data/inspect_data.R")
source("modules/data/validate_data.R")
source("modules/data/repair_dataset.R")

source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")
source("modules/analysis/statistical_tests.R")

source("modules/visualization/create_plots.R")
source("modules/visualization/plot_utils.R")

source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")

# Main function to orchestrate the analysis
# Helper to suppress output from verbose functions
quiet <- function(expr) {
  capture.output(result <- eval(substitute(expr), envir = parent.frame()))
  result
}

main <- function(repair_data = TRUE, validate_data = TRUE, missing_method = "regression",
                outlier_method = "iqr", missing_threshold = 0.05, zero_missing = FALSE) {
  
  # Use the default data file
  data_file <- "dane.csv"

  tryCatch({
    medical_data <- quiet(fetch_dataset(data_file))
    
    # Store original data for comparison
    original_data <- medical_data
    
    # Step 2: Data Inspection
    inspection_results <- quiet(inspect_data_structure(medical_data))
    
    # Step 3: Data Validation (if requested)
    if (validate_data) {
      validation_results <- quiet(validate_data_for_analysis(
        data = medical_data,
        group_column = "grupa",
        required_columns = c("grupa", "plec", "wiek"),
        min_group_size = 5
      ))
    } else {
      validation_results <- NULL
    }
    
    # Step 4: Data Repair (if requested)
    if (repair_data) {
      
      # Adjust threshold for zero missing tolerance
      effective_threshold <- if (zero_missing) 0.0 else missing_threshold
      
        if (zero_missing) {
          # zero missing mode active
        }
      
      repair_result <- repair_dataset(
        data = medical_data,
        missing_threshold = effective_threshold,  # Use adjusted threshold
        outlier_method = outlier_method,
        missing_method = missing_method,
        outlier_action = "winsorize"
      )
      
      # Use repaired data
      medical_data <- repair_result$data
      repair_log <- repair_result$repair_log
      
    } else {
      repair_log <- NULL
    }
    
    # Basic readiness assessment
    missing_summary <- sapply(medical_data, function(x) sum(is.na(x)))
    
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
    
    # Additional validation-based readiness assessment
    if (!is.null(validation_results) && !validation_results$valid) {
      readiness_issues <- c(readiness_issues, "Failed formal validation checks")
    }
    
    if (length(readiness_issues) == 0) {
      readiness_status <- "ready"
    } else {
      readiness_status <- paste(readiness_issues, collapse = "; ")
    }
    
    # Return the processed data and all results
    return(list(
      data = medical_data,
      original_data = original_data,
      inspection_results = inspection_results,
      validation_results = validation_results,
      repair_log = repair_log,
      readiness_issues = readiness_issues,
      readiness_status = readiness_status
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

