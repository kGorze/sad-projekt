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
main <- function(repair_data = TRUE, validate_data = TRUE, missing_method = "regression", 
                outlier_method = "iqr", missing_threshold = 0.05, zero_missing = FALSE) {
  cat("Medical Data Statistical Analysis Tool\n")
  cat("=====================================\n\n")
  
  # For demonstration, use the default data file
  data_file <- "dane.csv"
  
  cat("1. LOADING AND INSPECTING DATA\n")
  cat("------------------------------\n")
  
  # Load the dataset using our fetch_dataset module
  tryCatch({
    medical_data <- fetch_dataset(data_file)
    
    # Store original data for comparison
    original_data <- medical_data
    
    # Step 2: Data Inspection
    cat("\n=== DATA INSPECTION PHASE ===\n")
    inspection_results <- inspect_data_structure(medical_data)
    
    # Step 3: Data Validation (if requested)
    if (validate_data) {
      cat("\n=== DATA VALIDATION PHASE ===\n")
      validation_results <- validate_data_for_analysis(
        data = medical_data,
        group_column = "grupa",
        required_columns = c("grupa", "plec", "wiek"),
        min_group_size = 5
      )
      
      if (!validation_results$valid) {
        cat("\n⚠ CRITICAL VALIDATION ISSUES DETECTED:\n")
        for (issue in validation_results$issues) {
          cat(sprintf("  - %s\n", issue))
        }
        cat("\nConsider addressing these issues before proceeding with analysis.\n")
      } else {
        cat("\n✓ Data validation passed - ready for statistical analysis!\n")
      }
      
      if (length(validation_results$warnings) > 0) {
        cat("\nValidation warnings:\n")
        for (warning in validation_results$warnings) {
          cat(sprintf("  ⚠ %s\n", warning))
        }
      }
      
      if (length(validation_results$recommendations) > 0) {
        cat("\nRecommendations:\n")
        for (rec in validation_results$recommendations) {
          cat(sprintf("  → %s\n", rec))
        }
      }
    } else {
      cat("\n=== SKIPPING DATA VALIDATION ===\n")
      cat("Proceeding without formal validation checks\n")
      validation_results <- NULL
    }
    
    # Step 4: Data Repair (if requested)
    if (repair_data) {
      cat("\n=== DATA REPAIR PHASE ===\n")
      
      # Adjust threshold for zero missing tolerance
      effective_threshold <- if (zero_missing) 0.0 else missing_threshold
      
      if (zero_missing) {
        cat("ZERO MISSING VALUES MODE: Will impute all missing data regardless of amount\n")
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
      
      cat("\nData repair completed. Proceeding with analysis on cleaned data.\n")
    } else {
      cat("\n=== SKIPPING DATA REPAIR ===\n")
      cat("Proceeding with original data (may contain missing values and outliers)\n")
      repair_log <- NULL
    }
    
    cat("\n2. DATA OVERVIEW (POST-PROCESSING)\n")
    cat("----------------------------------\n")
    
    # Quick overview of final dataset
    quick_data_overview(medical_data)
    
    # Display first few rows
    cat("\nFirst 5 rows of the dataset:\n")
    print(head(medical_data, 5))
    
    cat("\nDataset summary:\n")
    print(summary(medical_data))
    
    # Show detailed group composition if available
    if ("grupa" %in% names(medical_data)) {
      cat("\nGroup composition:\n")
      group_table <- table(medical_data$grupa)
      print(group_table)
      
      cat("\nGroup percentages:\n")
      print(round(prop.table(group_table) * 100, 1))
      
      # Show gender distribution by group if available
      if ("plec" %in% names(medical_data)) {
        cat("\nGender distribution by group:\n")
        print(table(medical_data$grupa, medical_data$plec))
      }
      
      # Basic age statistics by group if available
      if ("wiek" %in% names(medical_data)) {
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
    }
    
    cat("\n3. DATA QUALITY ASSESSMENT (FINAL)\n")
    cat("----------------------------------\n")
    
    # Check for remaining missing values
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
    
    cat("\n6. FINAL READINESS ASSESSMENT\n")
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
    
    # Additional validation-based readiness assessment
    if (!is.null(validation_results) && !validation_results$valid) {
      readiness_issues <- c(readiness_issues, "Failed formal validation checks")
    }
    
    if (length(readiness_issues) == 0) {
      cat("✓ Dataset is ready for statistical analysis!\n")
      cat("✓ No major data quality issues detected.\n")
      cat("✓ Sufficient sample size and variables available.\n")
      if (!is.null(validation_results) && validation_results$valid) {
        cat("✓ Passed formal validation requirements.\n")
      }
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
    
    # Return the processed data and all results
    return(list(
      data = medical_data,
      original_data = original_data,
      inspection_results = inspection_results,
      validation_results = validation_results,
      repair_log = repair_log,
      readiness_issues = readiness_issues
    ))
    
  }, error = function(e) {
    cat("ERROR: Failed to process dataset\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Function to demonstrate data pipeline step-by-step
demo_data_pipeline <- function(file_path = "dane.csv") {
  cat("=== DATA PROCESSING PIPELINE DEMONSTRATION ===\n\n")
  
  # Step 1: Fetch
  cat("STEP 1: FETCHING DATA\n")
  cat("---------------------\n")
  data <- fetch_dataset(file_path)
  if (is.null(data)) return(NULL)
  
  # Step 2: Inspect
  cat("\nSTEP 2: INSPECTING DATA\n")
  cat("-----------------------\n")
  inspection <- inspect_data_structure(data)
  
  # Step 3: Validate
  cat("\nSTEP 3: VALIDATING DATA\n")
  cat("-----------------------\n")
  validation <- validate_data_for_analysis(data, "grupa")
  
  # Step 4: Repair (if needed)
  cat("\nSTEP 4: REPAIRING DATA\n")
  cat("----------------------\n")
  repair <- repair_dataset(data, missing_threshold = 0.01)
  
  return(list(
    original = data,
    cleaned = repair$data,
    inspection = inspection,
    validation = validation,
    repair_log = repair$repair_log
  ))
}

# Function to compare pipeline approaches
compare_pipeline_approaches <- function(file_path = "dane.csv") {
  cat("=== COMPARING PIPELINE APPROACHES ===\n\n")
  
  # Load original data
  original_data <- fetch_dataset(file_path)
  if (is.null(original_data)) return(NULL)
  
  # Approach 1: Minimal processing
  cat("APPROACH 1: MINIMAL PROCESSING\n")
  cat("-------------------------------\n")
  result1 <- main(repair_data = FALSE, validate_data = FALSE)
  
  # Approach 2: With validation only
  cat("\n\nAPPROACH 2: WITH VALIDATION\n")
  cat("---------------------------\n")
  result2 <- main(repair_data = FALSE, validate_data = TRUE)
  
  # Approach 3: Full pipeline
  cat("\n\nAPPROACH 3: FULL PIPELINE\n")
  cat("-------------------------\n")
  result3 <- main(repair_data = TRUE, validate_data = TRUE)
  
  return(list(
    minimal = result1,
    with_validation = result2,
    full_pipeline = result3
  ))
}

# Execute main function if script is run directly
if (!interactive()) {
  main()
} else {
  # If running interactively, provide helper message
  cat("Medical Data Analysis Tool loaded.\n")
  cat("Available functions:\n")
  cat("- main(): Run complete analysis pipeline\n")
  cat("- demo_data_pipeline(): Step-by-step pipeline demonstration\n")
  cat("- compare_pipeline_approaches(): Compare different processing approaches\n")
  cat("- fetch_dataset(): Load data only\n")
  cat("- inspect_data_structure(): Inspect data only\n")
  cat("- validate_data_for_analysis(): Validate data only\n")
  cat("- repair_dataset(): Repair data only\n")
  cat("- main_zero_missing(): Run pipeline with guaranteed zero missing values\n")
}

# Convenience function for zero missing values
main_zero_missing <- function(missing_method = "regression") {
  cat("🔧 ZERO MISSING VALUES MODE\n")
  cat("==========================\n")
  cat("This will eliminate ALL missing values regardless of amount.\n\n")
  
  result <- main(
    repair_data = TRUE,
    validate_data = TRUE,
    missing_method = missing_method,
    zero_missing = TRUE
  )
  
  # Verify zero missing
  missing_count <- sum(sapply(result$data, function(x) sum(is.na(x))))
  
  cat("\n🎯 ZERO MISSING VALUES VERIFICATION\n")
  cat("===================================\n")
  if (missing_count == 0) {
    cat("✅ SUCCESS: Zero missing values achieved!\n")
    cat("✅ Dataset is now completely free of missing data.\n")
  } else {
    cat("❌ ERROR: Still has", missing_count, "missing values\n")
    cat("❌ Please contact support - this shouldn't happen.\n")
  }
  
  return(result)
}
