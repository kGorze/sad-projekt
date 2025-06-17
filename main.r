# Statistical Analysis Framework
# Author: Konrad Gorzelańczyk


# Command line parsing setup
if (!require(optparse, quietly = TRUE)) {
  install.packages("optparse", repos = "https://cran.r-project.org")
  library(optparse)
}

# Configuration and package loading
source("modules/utils/config.R")

# Package loading with improved conflict handling
cat("Loading required packages...\n")
# Suppress package conflict warnings temporarily  
suppress_package_conflicts()
load_required_packages(quiet = TRUE)
restore_warnings()

# Module loading
source("modules/utils/logging.R")
source("modules/utils/statistical_helpers.R")

source("modules/data/fetch_dataset.R")
source("modules/data/inspect_data.R")
source("modules/data/validate_data.R")
source("modules/data/repair_dataset.R")

# Analysis modules
source("modules/analysis/assumptions_dashboard.R")
source("modules/analysis/master_descriptive_summary.R")

source("modules/analysis/descriptive_stats.R")
source("modules/analysis/comparative_analysis.R")
source("modules/analysis/correlation_analysis.R")

source("modules/reporting/generate_report.R")
source("modules/reporting/export_results.R")

# Output suppression utility
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
    make_option(c("--enhanced_inferential"), action="store_true", default=FALSE,
                help="Run enhanced inferential analysis (multiple regression, ANCOVA, interactions)"),
    make_option(c("--unified_dashboard"), action="store_true", default=FALSE,
                help="Generate unified dashboard with all analyses"),
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

# Helper function to run analysis with consistent logging
run_single_analysis <- function(analysis_type, analysis_function, data, description) {
  log_analysis_step(toupper(analysis_type), paste("Starting", description))
  cat("Running", description, "...\n")
  
  analysis_results <- execute_with_warning_capture(analysis_function)
  
  if (!is.null(analysis_results)) {
    log_analysis_step(paste(toupper(analysis_type), "COMPLETED"), paste(description, "successfully completed"))
  }
  
  return(analysis_results)
}

# Run analysis based on command line arguments
run_analysis_with_args <- function(args) {
  
  # Initialize enhanced logging system to capture all console output and warnings
  log_file <- init_logging()
  
  # Setup graphics environment to prevent unwanted Rplots.pdf creation
  setup_graphics_environment()
  
  # Source plotting utilities for enhanced graphics management
  source("modules/utils/plotting_utils.R")
  init_graphics_session()
  
  # Set up error handling to ensure logging is closed
  on.exit({
    # Restore warning option
    options(warn = old_warn)
    close_logging()
    # End graphics session cleanly
    end_graphics_session()
    # Cleanup any unwanted graphics files before exiting
    cleanup_unwanted_graphics_files()
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
    validate_data = args$validate_data,
    zero_missing = TRUE  # Task G: Always perform missing data sensitivity analysis
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
  
  # Source enhanced inferential framework once if needed
  if (args$enhanced_inferential || args$unified_dashboard) {
    source("modules/analysis/enhanced_inferential_framework.R")
  }
  
  if (args$comparative_analysis) {
    analysis_type <- "comparative_analysis"
    analysis_results <- run_single_analysis(
      analysis_type, 
      function() perform_group_comparisons(data = medical_data, group_column = "grupa", include_plots = TRUE),
      medical_data,
      "comparative analysis between groups"
    )
  }
  
  if (args$correlation_analysis) {
    analysis_type <- "correlation_analysis"
    analysis_results <- run_single_analysis(
      analysis_type,
      function() perform_correlation_analysis(data = medical_data, group_column = "grupa", variables = NULL, include_plots = TRUE),
      medical_data,
      "correlation analysis of variables"
    )
  }
  
  if (args$descriptive_stats) {
    analysis_type <- "descriptive_stats"
    analysis_results <- run_single_analysis(
      analysis_type,
      function() generate_descriptive_stats(data = medical_data, group_column = "grupa", include_plots = TRUE),
      medical_data,
      "descriptive statistics generation"
    )
  }
  
  if (args$statistical_tests) {
    log_analysis_step("STATISTICAL TESTS", "Starting statistical tests (placeholder)")
    cat("Running statistical tests...\n")
    analysis_type <- "statistical_tests"
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
  
  if (args$enhanced_inferential) {
    analysis_type <- "enhanced_inferential"
    analysis_results <- run_single_analysis(
      analysis_type,
      function() perform_enhanced_inferential_analysis(data = medical_data, group_column = "grupa", include_plots = TRUE),
      medical_data,
      "enhanced inferential analysis with covariates"
    )
  }
  
  if (args$unified_dashboard) {
    log_analysis_step("UNIFIED DASHBOARD", "Starting unified dashboard generation with all analyses")
    cat("Generating unified dashboard with all analyses...\n")
    analysis_type <- "unified_dashboard"
    
    cat("Running comprehensive analysis suite with shared assumptions...\n")
    
    # Collect all results
    all_results <- list()
    
    # Run all four analyses with shared assumptions testing
    cat("  1/4: Descriptive Statistics with Assumptions Testing...\n")
    all_results$descriptive_stats <- execute_with_warning_capture({
      generate_descriptive_stats(data = medical_data, group_column = "grupa", include_plots = TRUE)
    })
    
    # Extract shared assumptions from descriptive stats
    shared_assumptions <- all_results$descriptive_stats$assumptions_analysis
    cat("- Shared assumptions extracted from descriptive stats for reuse\n")
    
    # Run remaining analyses with shared assumptions
    cat("  2/4: Correlation Analysis (reusing assumptions)...\n")
    all_results$correlation_analysis <- execute_with_warning_capture({
      perform_correlation_analysis(
        data = medical_data, 
        group_column = "grupa",
        variables = NULL,
        include_plots = TRUE,
        shared_assumptions = shared_assumptions
      )
    })
    
    cat("  3/4: Comparative Analysis (reusing assumptions)...\n")
    all_results$comparative_analysis <- execute_with_warning_capture({
      perform_group_comparisons(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE,
        shared_assumptions = shared_assumptions
      )
    })
    
    cat("  4/4: Enhanced Inferential Analysis (reusing assumptions)...\n")
    all_results$enhanced_inferential <- execute_with_warning_capture({
      perform_enhanced_inferential_analysis(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE,
        shared_assumptions = shared_assumptions
      )
    })
    
    # Create unified dashboard directory with timestamp
    timestamp <- format(Sys.time(), "%d%m_%H%M")
    dashboard_dir <- paste0("output/reports/full_report_", timestamp)
    dir.create(dashboard_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Generate individual HTML reports for each analysis
    cat("Generating individual HTML reports...\n")
    
    report_files <- list()
    analysis_names <- c("descriptive_stats", "correlation_analysis", "comparative_analysis", "enhanced_inferential")
    
    for (i in seq_along(analysis_names)) {
      analysis_name <- analysis_names[i]
      cat(sprintf("  Generating %s report...\n", analysis_name))
      
      if (!is.null(all_results[[analysis_name]])) {
        simple_name <- paste0(analysis_name, ".html")
        simple_filepath <- file.path(dashboard_dir, simple_name)
        
        report_file <- tryCatch({
          if (!exists("create_html_report_content")) {
            source("modules/reporting/generate_report.R")
          }
          
          title <- paste("Statistical Analysis Report -", stringr::str_to_title(gsub("_", " ", analysis_name)))
          plot_base_path <- file.path("..", "..")
          
          html_content <- create_html_report_content(
            all_results[[analysis_name]], 
            analysis_name, 
            title, 
            TRUE,
            plot_base_path
          )
          
          writeLines(html_content, simple_filepath)
          cat("HTML report generated:", simple_filepath, "\n")
          simple_filepath
        }, error = function(e) {
          cat(sprintf("  Warning: %s report generation failed: %s\n", analysis_name, e$message))
          NULL
        })
        
        if (!is.null(report_file)) {
          report_files[[analysis_name]] <- simple_name
          cat(sprintf("  ✓ %s report generated\n", analysis_name))
        }
      }
    }
    
    # Generate navigation index.html
    cat("Creating unified dashboard navigation...\n")
    
    # Source dashboard template
    source("modules/reporting/dashboard_template.R")
    
    # Generate button HTML for each analysis
    create_button <- function(report_file) {
      if (!is.null(report_file)) {
        sprintf('<a href="%s" target="_blank" class="btn-analysis"><i class="fas fa-eye"></i>View Report</a>', report_file)
      } else {
        '<button class="btn-analysis" disabled><i class="fas fa-exclamation-triangle"></i>Report Failed</button>'
      }
    }
    
    desc_button <- create_button(report_files$descriptive_stats)
    corr_button <- create_button(report_files$correlation_analysis)
    comp_button <- create_button(report_files$comparative_analysis)
    infer_button <- create_button(report_files$enhanced_inferential)
    
    # Generate HTML using template
    index_html <- generate_dashboard_html(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      nrow(medical_data),
      ncol(medical_data),
      desc_button,
      corr_button,
      comp_button,
      infer_button
    )
    
    # Write the index file
    writeLines(index_html, file.path(dashboard_dir, "index.html"))
    
    # Set analysis results for potential export
    analysis_results <- list(
      analysis_type = "unified_dashboard",
      dashboard_directory = dashboard_dir,
      individual_results = all_results,
      report_files = report_files,
      summary = list(
        total_analyses = length(analysis_names),
        successful_analyses = length(report_files),
        failed_analyses = length(analysis_names) - length(report_files),
        dashboard_url = file.path(dashboard_dir, "index.html")
      )
    )
    
    cat(sprintf("Unified dashboard generated successfully!\n"))
    cat(sprintf("Dashboard location: %s\n", dashboard_dir))
    cat(sprintf("Open: %s\n", file.path(dashboard_dir, "index.html")))
    cat(sprintf("Analyses completed: %d/%d\n", length(report_files), length(analysis_names)))
    
    log_analysis_step("UNIFIED DASHBOARD COMPLETED", 
                     paste("Dashboard generated with", length(report_files), "successful analyses"))
  }
  
  # Export CSV files if requested
  exported_files <- NULL
  if (args$export && !is.null(analysis_results) && !is.null(analysis_type)) {
    log_analysis_step("CSV EXPORT", "Starting CSV export of analysis results")
    cat("Exporting analysis results to CSV files...\n")
    
    if (!dir.exists("output/tables")) {
      dir.create("output/tables", recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_filename <- paste0(analysis_type, "_", timestamp)
    
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
    
    if (!dir.exists("output")) {
      dir.create("output", recursive = TRUE)
    }
    
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
  
  # Check if only export is requested (without analysis)
  if (args$export && is.null(analysis_results)) {
    log_analysis_step("DATA EXPORT", "Starting export of cleaned/validated data")
    cat("Exporting cleaned and validated data to CSV files...\n")
    
    if (!dir.exists("output/tables")) {
      dir.create("output/tables", recursive = TRUE)
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    cleaned_data_file <- file.path("output/tables", paste0("cleaned_data_", timestamp, ".csv"))
    write.csv(medical_data, cleaned_data_file, row.names = FALSE)
    cat("Cleaned data exported to:", cleaned_data_file, "\n")
    
    if (exists("data_prep_result") && !is.null(data_prep_result$original_data)) {
      original_data_file <- file.path("output/tables", paste0("original_data_", timestamp, ".csv"))
      write.csv(data_prep_result$original_data, original_data_file, row.names = FALSE)
      cat("Original data exported to:", original_data_file, "\n")
    }
    
    if (exists("data_prep_result") && !is.null(data_prep_result$repair_log)) {
      prep_summary <- data.frame(
        step = c("original_rows", "original_cols", "cleaned_rows", "cleaned_cols", 
                "missing_values_before", "missing_values_after", "readiness_status"),
        value = c(
          if(!is.null(data_prep_result$original_data)) nrow(data_prep_result$original_data) else nrow(medical_data),
          if(!is.null(data_prep_result$original_data)) ncol(data_prep_result$original_data) else ncol(medical_data),
          nrow(medical_data),
          ncol(medical_data),
          if(!is.null(data_prep_result$original_data)) sum(is.na(data_prep_result$original_data)) else "unknown",
          sum(is.na(medical_data)),
          if(!is.null(data_prep_result$readiness_status)) data_prep_result$readiness_status else "processed"
        ),
        stringsAsFactors = FALSE
      )
      
      prep_summary_file <- file.path("output/tables", paste0("data_preparation_summary_", timestamp, ".csv"))
      write.csv(prep_summary, prep_summary_file, row.names = FALSE)
      cat("Data preparation summary exported to:", prep_summary_file, "\n")
    }
    
    log_analysis_step("DATA EXPORT COMPLETED", "Data export completed successfully")
    cat("Data export completed! Files saved to output/tables/\n")
    cat("Use any analysis option (--descriptive_stats, --correlation_analysis, etc.) to perform statistical analysis.\n")
    
    return(list(
      exported_files = list(
        cleaned_data = cleaned_data_file,
        original_data = if(exists("original_data_file")) original_data_file else NULL,
        preparation_summary = if(exists("prep_summary_file")) prep_summary_file else NULL
      ),
      analysis_type = "data_export"
    ))
  }
  
  # If no analysis specified, show help
  cat("No analysis specified. Available options:\n")
  cat("  --comparative_analysis: Run comparative analysis\n")
  cat("  --correlation_analysis: Run correlation analysis\n")
  cat("  --descriptive_stats: Run descriptive statistics\n")
  cat("  --enhanced_inferential: Run enhanced inferential analysis\n")
  cat("  --unified_dashboard: Generate unified dashboard with all analyses\n")
  cat("  --statistical_tests: Run statistical tests\n")
  cat("  --report: Generate HTML report (use with any analysis option)\n")
  cat("  --export: Export cleaned data to CSV files (works without analysis option)\n")
  cat("  --input <file>: Specify input data file (default: dane.csv)\n")
  cat("\nExample usage:\n")
  cat("  Rscript main.R --comparative_analysis --report\n")
  cat("  Rscript main.R --unified_dashboard\n")
  cat("  Rscript main.R --unified_dashboard --input data.csv\n")
  cat("  Rscript main.R --correlation_analysis --report --export\n")
  cat("  Rscript main.R --input dane2.csv --descriptive_stats --export\n")
  cat("  Rscript main.R --correlation_analysis --input mydata.csv --report --export\n")
  cat("  Rscript main.R --unified_dashboard --input clinical_data.xlsx\n")
  cat("  Rscript main.R --input dane2.csv --export  # Export cleaned data only\n")
  
  return(NULL)
}

# Graphics environment setup
setup_graphics_environment <- function() {
  # Close any existing graphics devices
  if (length(dev.list()) > 0) {
    graphics.off()
  }
  
  # Set options to prevent default PDF device creation
  options(device = function(...) {
    # Create a null device that doesn't output files
    if (requireNamespace("grDevices", quietly = TRUE)) {
      if (.Platform$OS.type == "windows") {
        grDevices::windows(width = 10, height = 8)
      } else {
        grDevices::x11(width = 10, height = 8)
      }
    }
  })
  
  cat("Graphics environment configured to prevent unwanted PDF creation\n")
}

# Enhanced function to cleanup unwanted graphics files
cleanup_unwanted_graphics_files <- function() {
  # Check for and remove Rplots.pdf if it exists
  if (file.exists("Rplots.pdf")) {
    tryCatch({
      file.remove("Rplots.pdf")
      cat("Removed unwanted Rplots.pdf file\n")
    }, error = function(e) {
      cat("Warning: Could not remove Rplots.pdf:", e$message, "\n")
    })
  }
  
  # Close any open graphics devices to prevent future unwanted files
  if (length(dev.list()) > 0) {
    graphics.off()
  }
}

# Original main function for backward compatibility
main <- function(data_file = "dane.csv", repair_data = TRUE, validate_data = TRUE, missing_method = "regression",
                outlier_method = "iqr", missing_threshold = 0.05, zero_missing = TRUE) {

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
      
      # Task G: Always perform missing data imputation sensitivity analysis
      if (zero_missing) {
        cat("=== TASK G: MISSING DATA SENSITIVITY ANALYSIS ===\n")
        cat("Forcing imputation for all missing values to perform sensitivity analysis\n")
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
    
    # Cleanup any unwanted graphics files
    cleanup_unwanted_graphics_files()
    
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

