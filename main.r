# Medical Data Statistical Analysis Tool
# Main entry point for command-line execution
# Author: SAD Project Team
# Usage: Rscript main.R --input data.csv --output results/ --groups group_column
# Usage: Rscript main.R --comparative_analysis --report
# Usage: Rscript main.R --correlation_analysis --report
# Usage: Rscript main.R --descriptive_stats --report

# Load required libraries for command line parsing
library(optparse)

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
    make_option(c("--output"), type="character", default="output/",
                help="Output directory for reports [default: %default]"),
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
  
  # Load and prepare data first
  cat("Loading and preparing data...\n")
  data_prep_result <- main(
    repair_data = args$repair_data, 
    validate_data = args$validate_data
  )
  
  if (is.null(data_prep_result)) {
    cat("Error: Failed to load or prepare data\n")
    return(NULL)
  }
  
  medical_data <- data_prep_result$data
  
  # Determine which analysis to run
  analysis_results <- NULL
  analysis_type <- NULL
  
  if (args$comparative_analysis) {
    cat("Running comparative analysis...\n")
    analysis_type <- "comparative_analysis"
    # Placeholder for future comparative analysis implementation
    analysis_results <- list(
      analysis_type = "comparative_analysis",
      message = "Comparative analysis module ready for implementation",
      data_summary = list(
        n_observations = nrow(medical_data),
        n_variables = ncol(medical_data),
        group_column = "grupa"
      ),
      test_results = list(
        list(
          variable = "Example Variable",
          test_name = "Example Test",
          statistic = 2.5,
          p_value = 0.03,
          effect_size = 0.4
        )
      )
    )
  }
  
  if (args$correlation_analysis) {
    cat("Running correlation analysis...\n")
    analysis_type <- "correlation_analysis"
    # Placeholder for future correlation analysis implementation
    numeric_vars <- sapply(medical_data, is.numeric)
    if (sum(numeric_vars) > 1) {
      correlation_matrix <- cor(medical_data[, numeric_vars], use = "complete.obs")
    } else {
      correlation_matrix <- NULL
    }
    
    analysis_results <- list(
      analysis_type = "correlation_analysis",
      message = "Correlation analysis module ready for implementation",
      correlation_matrix = correlation_matrix,
      data_summary = list(
        n_observations = nrow(medical_data),
        n_numeric_variables = sum(numeric_vars)
      )
    )
  }
  
  if (args$descriptive_stats) {
    cat("Running descriptive statistics...\n")
    analysis_type <- "descriptive_stats"
    # Placeholder for future descriptive statistics implementation
    numeric_vars <- sapply(medical_data, is.numeric)
    
    if (sum(numeric_vars) > 0) {
      summary_stats <- data.frame(
        n = sapply(medical_data[, numeric_vars, drop=FALSE], function(x) sum(!is.na(x))),
        mean = sapply(medical_data[, numeric_vars, drop=FALSE], mean, na.rm = TRUE),
        sd = sapply(medical_data[, numeric_vars, drop=FALSE], sd, na.rm = TRUE),
        min = sapply(medical_data[, numeric_vars, drop=FALSE], min, na.rm = TRUE),
        max = sapply(medical_data[, numeric_vars, drop=FALSE], max, na.rm = TRUE),
        missing = sapply(medical_data[, numeric_vars, drop=FALSE], function(x) sum(is.na(x)))
      )
    } else {
      summary_stats <- NULL
    }
    
    analysis_results <- list(
      analysis_type = "descriptive_stats",
      message = "Descriptive statistics module ready for implementation",
      summary_stats = summary_stats,
      data_summary = list(
        n_observations = nrow(medical_data),
        n_variables = ncol(medical_data)
      )
    )
  }
  
  if (args$statistical_tests) {
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
  }
  
  # Generate report if requested
  if (args$report && !is.null(analysis_results) && !is.null(analysis_type)) {
    cat("Generating HTML report...\n")
    
    # Create output directory if it doesn't exist
    if (!dir.exists(args$output)) {
      dir.create(args$output, recursive = TRUE)
    }
    
    # Generate the HTML report
    report_file <- generate_html_report(
      analysis_results = analysis_results,
      analysis_type = analysis_type,
      output_path = args$output,
      title = paste("Statistical Analysis Report -", stringr::str_to_title(gsub("_", " ", analysis_type))),
      include_plots = TRUE
    )
    
    cat("Report generated successfully:", report_file, "\n")
    
    # Return both analysis results and report information
    return(list(
      analysis_results = analysis_results,
      report_file = report_file,
      analysis_type = analysis_type
    ))
  }
  
  # If no report requested, just return analysis results
  if (!is.null(analysis_results)) {
    cat("Analysis completed successfully.\n")
    cat("Use --report flag to generate HTML report.\n")
    return(list(
      analysis_results = analysis_results,
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
  cat("\nExample usage:\n")
  cat("  Rscript main.R --comparative_analysis --report\n")
  cat("  Rscript main.R --correlation_analysis --report\n")
  
  return(NULL)
}

# Original main function for backward compatibility
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

# Main execution logic
if (!interactive()) {
  # Parse command line arguments and run analysis
  args <- parse_arguments()
  result <- run_analysis_with_args(args)
}

