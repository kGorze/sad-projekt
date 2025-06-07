# Medical Data Statistical Analysis Tool
# Main entry point for command-line execution
# Author: SAD Project Team
# Usage: Rscript main.R --comparative_analysis --report
# Usage: Rscript main.R --correlation_analysis --report
# Usage: Rscript main.R --descriptive_stats --report
# Usage: Rscript main.R --comparative_analysis --export
# Usage: Rscript main.R --correlation_analysis --report --export
# Usage: Rscript main.R --input dane2.csv --descriptive_stats --export
# Usage: Rscript main.R --correlation_analysis --input mydata.csv --report --export

# Load required libraries for command line parsing with error handling
if (!require(optparse, quietly = TRUE)) {
  install.packages("optparse", repos = "https://cran.r-project.org")
  library(optparse)
}

# Source configuration first to get centralized package loading
source("modules/utils/config.R")

# Load all required packages centrally and quietly to avoid warnings
cat("Loading required packages...\n")
load_required_packages(quiet = TRUE)

# Source all required modules
source("modules/utils/logging.R")
source("modules/utils/statistical_helpers.R")

source("modules/data/fetch_dataset.R")
source("modules/data/inspect_data.R")
source("modules/data/validate_data.R")
source("modules/data/repair_dataset.R")

# Source centralized analysis modules first (to eliminate duplication)
source("modules/analysis/assumptions_dashboard.R")
source("modules/analysis/master_descriptive_summary.R")

source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")

source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")

# Main function to orchestrate the analysis
# Helper to suppress output from verbose functions
quiet <- function(expr) {
  capture.output(result <- eval(substitute(expr), envir = parent.frame()))
  result
}

# Parse command line arguments
parse_arguments <- function() {
  option_list <- list(
    make_option(c("--comparative_analysis"), action="store_true", default=FALSE,
                help="Run comparative analysis"),
    make_option(c("--correlation_analysis"), action="store_true", default=FALSE,
                help="Run correlation analysis"),
    make_option(c("--descriptive_stats"), action="store_true", default=FALSE,
                help="Run descriptive statistics analysis"),
    make_option(c("--statistical_tests"), action="store_true", default=FALSE,
                help="Run statistical tests"),
    make_option(c("--report"), action="store_true", default=FALSE,
                help="Generate HTML report for the analysis"),
    make_option(c("--export"), action="store_true", default=FALSE,
                help="Export analysis results to CSV files in output/tables/"),
    make_option(c("--input"), type="character", default="dane.csv",
                help="Input data file [default: %default]"),
    make_option(c("--repair_data"), action="store_true", default=TRUE,
                help="Repair data before analysis [default: %default]"),
    make_option(c("--validate_data"), action="store_true", default=TRUE,
                help="Validate data before analysis [default: %default]")
  )
  
  opt_parser <- OptionParser(option_list=option_list)
  opt <- parse_args(opt_parser)
  
  return(opt)
}

# Run analysis based on command line arguments
run_analysis_with_args <- function(args) {
  
  # Initialize enhanced logging system to capture all console output and warnings
  log_file <- init_logging()
  
  # Set up error handling to ensure logging is closed
  on.exit({
    # Restore warning option
    options(warn = old_warn)
    close_logging()
  })
  
  # Set warning options to capture all warnings
  old_warn <- getOption("warn")
  options(warn = 1)  # Print warnings immediately
  
  # Load and prepare data first
  log_analysis_step("DATA LOADING", paste("Loading data from:", args$input))
  cat("Loading and preparing data from:", args$input, "\n")
  data_prep_result <- main(
    data_file = args$input,
    repair_data = args$repair_data, 
    validate_data = args$validate_data
  )
  
  if (is.null(data_prep_result)) {
    log_error("Failed to load or prepare data")
    cat("Error: Failed to load or prepare data\n")
    return(NULL)
  }
  
  medical_data <- data_prep_result$data
  log_analysis_step("DATA PREPARATION COMPLETED", 
                   paste("Dataset loaded with", nrow(medical_data), "rows and", ncol(medical_data), "columns"))
  
  # Determine which analysis to run
  analysis_results <- NULL
  analysis_type <- NULL
  
  if (args$comparative_analysis) {
    log_analysis_step("COMPARATIVE ANALYSIS", "Starting comparative analysis between groups")
    cat("Running comparative analysis...\n")
    analysis_type <- "comparative_analysis"
    
    # Use the implemented comparative analysis function with warning capture
    analysis_results <- execute_with_warning_capture({
      perform_group_comparisons(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
      )
    })
    
    if (!is.null(analysis_results)) {
      log_analysis_step("COMPARATIVE ANALYSIS COMPLETED", "Group comparisons successfully completed")
    }
  }
  
  if (args$correlation_analysis) {
    log_analysis_step("CORRELATION ANALYSIS", "Starting correlation analysis of variables")
    cat("Running correlation analysis...\n")
    analysis_type <- "correlation_analysis"
    
    # Use the implemented correlation analysis function with warning capture
    analysis_results <- execute_with_warning_capture({
      perform_correlation_analysis(
        data = medical_data, 
        group_column = "grupa",
        variables = NULL,  # Will auto-detect numeric variables
        include_plots = TRUE
      )
    })
    
    if (!is.null(analysis_results)) {
      log_analysis_step("CORRELATION ANALYSIS COMPLETED", "Correlation analysis successfully completed")
    }
  }
  
  if (args$descriptive_stats) {
    log_analysis_step("DESCRIPTIVE STATISTICS", "Starting descriptive statistics generation")
    cat("Running descriptive statistics...\n")
    analysis_type <- "descriptive_stats"
    
    # Use the implemented descriptive statistics function with warning capture
    analysis_results <- execute_with_warning_capture({
      generate_descriptive_stats(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
      )
    })
    
    if (!is.null(analysis_results)) {
      log_analysis_step("DESCRIPTIVE STATISTICS COMPLETED", "Descriptive statistics successfully generated")
    }
  }
  
  if (args$statistical_tests) {
    log_analysis_step("STATISTICAL TESTS", "Starting statistical tests (placeholder)")
    cat("Running statistical tests...\n")
    analysis_type <- "statistical_tests"
    # Placeholder for future statistical tests implementation
    analysis_results <- list(
      analysis_type = "statistical_tests",
      message = "Statistical tests module ready for implementation",
      data_summary = list(
        n_observations = nrow(medical_data),
        n_variables = ncol(medical_data)
      )
    )
    
    if (!is.null(analysis_results)) {
      log_analysis_step("STATISTICAL TESTS COMPLETED", "Statistical tests placeholder completed")
    }
  }
  
  # Export CSV files if requested
  exported_files <- NULL
  if (args$export && !is.null(analysis_results) && !is.null(analysis_type)) {
    log_analysis_step("CSV EXPORT", "Starting CSV export of analysis results")
    cat("Exporting analysis results to CSV files...\n")
    
    # Create output directory if it doesn't exist
    if (!dir.exists("output/tables")) {
      dir.create("output/tables", recursive = TRUE)
    }
    
    # Generate timestamp for unique filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_filename <- paste0(analysis_type, "_", timestamp)
    
    # Export to CSV using the enhanced export infrastructure
    exported_files <- export_all_analysis_to_csv(analysis_results, base_filename)
    
    cat("CSV export completed! Files saved to output/tables/\n")
    cat("Exported files:\n")
    for (file_type in names(exported_files)) {
      cat("-", basename(exported_files[[file_type]]), "\n")
    }
    
    log_analysis_step("CSV EXPORT COMPLETED", 
                     paste("Exported", length(exported_files), "CSV files to output/tables/"))
  }
  
  # Generate report if requested
  if (args$report && !is.null(analysis_results) && !is.null(analysis_type)) {
    log_analysis_step("HTML REPORT GENERATION", "Starting HTML report generation")
    cat("Generating HTML report...\n")
    
    # Create output directory if it doesn't exist
    if (!dir.exists("output")) {
      dir.create("output", recursive = TRUE)
    }
    
    # Generate the HTML report
    report_file <- tryCatch({
      generate_html_report(
        analysis_results = analysis_results,
        analysis_type = analysis_type,
        output_path = "output/reports",
        title = paste("Statistical Analysis Report -", stringr::str_to_title(gsub("_", " ", analysis_type))),
        include_plots = TRUE
      )
    }, error = function(e) {
      cat("Note: HTML report generation encountered an issue. Analysis completed successfully.\n")
      cat("All analysis results are available in the analysis_results object.\n")
      cat("Plots have been saved to output/plots/\n")
      return(NULL)
    })
    
    if (!is.null(report_file)) {
      cat("Report generated successfully:", report_file, "\n")
      log_analysis_step("HTML REPORT COMPLETED", paste("Report saved to:", basename(report_file)))
    } else {
      cat("Analysis completed successfully. HTML report generation was skipped due to technical issues.\n")
      log_analysis_step("HTML REPORT SKIPPED", "Report generation encountered technical issues but analysis completed successfully")
    }
    
    # Return analysis results, report information, and exported files
    return(list(
      analysis_results = analysis_results,
      report_file = report_file,
      exported_files = exported_files,
      analysis_type = analysis_type
    ))
  }
  
  # If no report requested, just return analysis results
  if (!is.null(analysis_results)) {
    cat("Analysis completed successfully.\n")
    if (!args$export && !args$report) {
      cat("Use --report flag to generate HTML report.\n")
      cat("Use --export flag to export results to CSV files.\n")
    }
    return(list(
      analysis_results = analysis_results,
      exported_files = exported_files,
      analysis_type = analysis_type
    ))
  }
  
  # If no analysis specified, show help
  cat("No analysis specified. Available options:\n")
  cat("  --comparative_analysis: Run comparative analysis\n")
  cat("  --correlation_analysis: Run correlation analysis\n")
  cat("  --descriptive_stats: Run descriptive statistics\n")
  cat("  --statistical_tests: Run statistical tests\n")
  cat("  --report: Generate HTML report (use with any analysis option)\n")
  cat("  --export: Export analysis results to CSV files (use with any analysis option)\n")
  cat("\nExample usage:\n")
  cat("  Rscript main.R --comparative_analysis --report\n")
  cat("  Rscript main.R --comparative_analysis --export\n")
  cat("  Rscript main.R --correlation_analysis --report --export\n")
  cat("  Rscript main.R --input dane2.csv --descriptive_stats --export\n")
  cat("  Rscript main.R --correlation_analysis --input mydata.csv --report --export\n")
  
  return(NULL)
}

# Original main function for backward compatibility
main <- function(data_file = "dane.csv", repair_data = TRUE, validate_data = TRUE, missing_method = "regression",
                outlier_method = "iqr", missing_threshold = 0.05, zero_missing = FALSE) {

  tryCatch({
    cat("Attempting to fetch dataset from:", data_file, "\n")
    medical_data <- fetch_dataset(data_file)
    cat("Dataset fetched successfully\n")
    
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
    cat("Error in main function:", e$message, "\n")
    return(NULL)
  })
}

# Main execution logic
if (!interactive()) {
  # Capture any warnings that occur during execution
  old_warn_option <- getOption("warn")
  options(warn = 1)  # Print warnings as they occur
  
  tryCatch({
    # Parse command line arguments and run analysis
    args <- parse_arguments()
    result <- run_analysis_with_args(args)
    
    # Check for any accumulated warnings and capture them to the log
    accumulated_warnings <- warnings()
    if (length(accumulated_warnings) > 0) {
      cat("\n=== R WARNINGS SUMMARY ===\n")
      print(accumulated_warnings)
      cat("===========================\n")
      
      # Log warning count and sample warnings to the log system if available
      if (exists("log_data_quality")) {
        log_data_quality("R_WARNINGS", 
                         paste("Total R warnings encountered:", length(accumulated_warnings)), 
                         "INFO")
        
        # Log first few unique warnings as examples
        unique_warnings <- unique(names(accumulated_warnings))
        if (length(unique_warnings) > 0) {
          sample_warnings <- head(unique_warnings, 5)
          for (warning_msg in sample_warnings) {
            log_data_quality("R_WARNING_SAMPLE", warning_msg, "WARNING")
          }
        }
      }
    }
    
  }, error = function(e) {
    cat("Critical error occurred:", e$message, "\n")
    if (exists("log_error")) {
      log_error("Critical execution error", e$message)
    }
  }, finally = {
    # Restore original warning option
    options(warn = old_warn_option)
  })
}

