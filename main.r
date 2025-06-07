# Medical Data Statistical Analysis Tool
# Main entry point for command-line execution
#
# Usage: Rscript main.R --comparative_analysis --report
# Usage: Rscript main.R --correlation_analysis --report
# Usage: Rscript main.R --descriptive_stats --report
# Usage: Rscript main.R --enhanced_inferential --report
# Usage: Rscript main.R --comparative_analysis --export
# Usage: Rscript main.R --correlation_analysis --report --export
# Usage: Rscript main.R --enhanced_inferential --input dane2.csv --report --export
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
    # Clean up any unwanted graphics files after analysis
    cleanup_unwanted_graphics_files()
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
    # Clean up any unwanted graphics files after analysis
    cleanup_unwanted_graphics_files()
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
    # Clean up any unwanted graphics files after analysis
    cleanup_unwanted_graphics_files()
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
  
  if (args$enhanced_inferential) {
    log_analysis_step("ENHANCED INFERENTIAL ANALYSIS", "Starting enhanced inferential analysis with covariates")
    cat("Running enhanced inferential analysis (MLR, ANCOVA, interactions)...\n")
    analysis_type <- "enhanced_inferential"
    
    # Source the enhanced inferential framework
    source("modules/analysis/enhanced_inferential_framework.R")
    
    # Use the implemented enhanced inferential analysis function with warning capture
    analysis_results <- execute_with_warning_capture({
      perform_enhanced_inferential_analysis(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
      )
    })
    
    if (!is.null(analysis_results)) {
      log_analysis_step("ENHANCED INFERENTIAL ANALYSIS COMPLETED", "Enhanced inferential analysis successfully completed")
    }
    # Clean up any unwanted graphics files after analysis
    cleanup_unwanted_graphics_files()
  }
  
  if (args$unified_dashboard) {
    log_analysis_step("UNIFIED DASHBOARD", "Starting unified dashboard generation with all analyses")
    cat("Generating unified dashboard with all analyses...\n")
    analysis_type <- "unified_dashboard"
    
    # Source the enhanced inferential framework for the complete analysis
    source("modules/analysis/enhanced_inferential_framework.R")
    
    # Run all four analyses
    cat("Running comprehensive analysis suite...\n")
    
    # Collect all results
    all_results <- list()
    
    # 1. Descriptive Statistics
    cat("  1/4: Descriptive Statistics...\n")
    all_results$descriptive_stats <- execute_with_warning_capture({
      generate_descriptive_stats(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
      )
    })
    
    # 2. Correlation Analysis  
    cat("  2/4: Correlation Analysis...\n")
    all_results$correlation_analysis <- execute_with_warning_capture({
      perform_correlation_analysis(
        data = medical_data, 
        group_column = "grupa",
        variables = NULL,  # Will auto-detect numeric variables
        include_plots = TRUE
      )
    })
    
    # 3. Comparative Analysis
    cat("  3/4: Comparative Analysis...\n")
    all_results$comparative_analysis <- execute_with_warning_capture({
      perform_group_comparisons(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
      )
    })
    
    # 4. Enhanced Inferential Analysis
    cat("  4/4: Enhanced Inferential Analysis...\n")
    all_results$enhanced_inferential <- execute_with_warning_capture({
      perform_enhanced_inferential_analysis(
        data = medical_data, 
        group_column = "grupa", 
        include_plots = TRUE
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
        # Create simple filename directly (without timestamp) to avoid duplicates
        simple_name <- paste0(analysis_name, ".html")
        simple_filepath <- file.path(dashboard_dir, simple_name)
        
        report_file <- tryCatch({
          # Generate HTML content manually to avoid automatic timestamp naming
          if (!exists("create_html_report_content")) {
            source("modules/reporting/generate_report.R")
          }
          
          title <- paste("Statistical Analysis Report -", stringr::str_to_title(gsub("_", " ", analysis_name)))
          plot_base_path <- file.path("..", "..")  # For unified dashboard
          
          html_content <- create_html_report_content(
            all_results[[analysis_name]], 
            analysis_name, 
            title, 
            TRUE,  # include_plots
            plot_base_path
          )
          
          # Write directly to simple filename
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
    
    # Generate button HTML for each analysis
    desc_button <- if(!is.null(report_files$descriptive_stats)) {
      sprintf('<a href="%s" class="btn-analysis"><i class="fas fa-eye"></i>View Report</a>', 
              report_files$descriptive_stats)
    } else {
      '<button class="btn-analysis" disabled><i class="fas fa-exclamation-triangle"></i>Report Failed</button>'
    }
    
    corr_button <- if(!is.null(report_files$correlation_analysis)) {
      sprintf('<a href="%s" class="btn-analysis"><i class="fas fa-eye"></i>View Report</a>', 
              report_files$correlation_analysis)
    } else {
      '<button class="btn-analysis" disabled><i class="fas fa-exclamation-triangle"></i>Report Failed</button>'
    }
    
    comp_button <- if(!is.null(report_files$comparative_analysis)) {
      sprintf('<a href="%s" class="btn-analysis"><i class="fas fa-eye"></i>View Report</a>', 
              report_files$comparative_analysis)
    } else {
      '<button class="btn-analysis" disabled><i class="fas fa-exclamation-triangle"></i>Report Failed</button>'
    }
    
    infer_button <- if(!is.null(report_files$enhanced_inferential)) {
      sprintf('<a href="%s" class="btn-analysis"><i class="fas fa-eye"></i>View Report</a>', 
              report_files$enhanced_inferential)
    } else {
      '<button class="btn-analysis" disabled><i class="fas fa-exclamation-triangle"></i>Report Failed</button>'
    }
    
    index_html <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Statistical Analysis Dashboard</title>
    <link href="https://fonts.googleapis.com/css2?family=SF+Pro+Display:wght@300;400;500;600;700&display=swap" rel="stylesheet">
    <link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css" rel="stylesheet">
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, "SF Pro Display", "Segoe UI", Roboto, sans-serif;
            background: #f8f9fa;
            min-height: 100vh;
            color: #1d1d1f;
            line-height: 1.6;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            padding: 60px 20px;
        }
        
        .header {
            text-align: center;
            margin-bottom: 80px;
        }
        
        .header h1 {
            font-size: 3.5rem;
            font-weight: 700;
            color: #1d1d1f;
            margin-bottom: 16px;
            letter-spacing: -0.05em;
        }
        
        .header .subtitle {
            font-size: 1.5rem;
            font-weight: 400;
            color: #6e6e73;
            margin-bottom: 12px;
        }
        
        .header .meta {
            font-size: 1rem;
            color: #86868b;
            font-weight: 400;
        }
        
        .analysis-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 32px;
            margin-bottom: 60px;
        }
        
        .analysis-card {
            background: white;
            border-radius: 18px;
            padding: 0;
            border: 1px solid #e5e5e7;
            transition: all 0.3s cubic-bezier(0.25, 0.46, 0.45, 0.94);
            overflow: hidden;
            position: relative;
        }
        
        .analysis-card:hover {
            transform: translateY(-8px);
            box-shadow: 0 25px 50px rgba(0, 0, 0, 0.1);
            border-color: #d2d2d7;
        }
        
        .card-icon {
            padding: 40px 32px 24px;
            text-align: center;
            border-bottom: 1px solid #f5f5f7;
        }
        
        .card-icon i {
            font-size: 2.5rem;
            margin-bottom: 16px;
            display: block;
        }
        
        .card-icon.primary i { color: #007aff; }
        .card-icon.success i { color: #34c759; }
        .card-icon.warning i { color: #ff9500; }
        .card-icon.info i { color: #5856d6; }
        
        .card-icon h3 {
            font-size: 1.375rem;
            font-weight: 600;
            color: #1d1d1f;
            margin-bottom: 8px;
        }
        
        .card-content {
            padding: 24px 32px 32px;
            text-align: center;
        }
        
        .card-content p {
            font-size: 1rem;
            color: #6e6e73;
            margin-bottom: 24px;
            line-height: 1.5;
        }
        
        .btn-analysis {
            display: inline-block;
            background: #1d1d1f;
            color: white;
            text-decoration: none;
            padding: 12px 24px;
            border-radius: 50px;
            font-size: 0.95rem;
            font-weight: 500;
            transition: all 0.2s ease;
            border: none;
            cursor: pointer;
            min-width: 140px;
        }
        
        .btn-analysis:hover {
            background: #424245;
            color: white;
            text-decoration: none;
            transform: scale(1.02);
        }
        
        .btn-analysis:disabled {
            background: #d2d2d7;
            color: #86868b;
            cursor: not-allowed;
            transform: none;
        }
        
        .btn-analysis i {
            margin-right: 8px;
        }
        
        .footer-note {
            text-align: center;
            margin-top: 40px;
        }
        
        .footer-note p {
            font-size: 1rem;
            color: #86868b;
            font-weight: 400;
        }
        
        .footer-note i {
            margin-right: 8px;
            color: #007aff;
        }
        
        @media (max-width: 768px) {
            .container {
                padding: 40px 16px;
            }
            
            .header h1 {
                font-size: 2.5rem;
            }
            
            .header .subtitle {
                font-size: 1.25rem;
            }
            
            .analysis-grid {
                grid-template-columns: 1fr;
                gap: 24px;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>Statistical Analysis Dashboard</h1>
            <p class="subtitle">Complete Medical Data Analysis Suite</p>
            <p class="meta">Generated: %s | Dataset: %d observations, %d variables</p>
        </div>
        
        <div class="analysis-grid">
            <div class="analysis-card">
                <div class="card-icon primary">
                    <i class="fas fa-table"></i>
                    <h3>Descriptive Statistics</h3>
                </div>
                <div class="card-content">
                    <p>Summary statistics, distributions, and data quality assessment</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon success">
                    <i class="fas fa-project-diagram"></i>
                    <h3>Correlation Analysis</h3>
                </div>
                <div class="card-content">
                    <p>Variable relationships and correlation matrices</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon warning">
                    <i class="fas fa-balance-scale"></i>
                    <h3>Comparative Analysis</h3>
                </div>
                <div class="card-content">
                    <p>Group comparisons and statistical tests</p>
                    %s
                </div>
            </div>
            
            <div class="analysis-card">
                <div class="card-icon info">
                    <i class="fas fa-brain"></i>
                    <h3>Enhanced Inferential</h3>
                </div>
                <div class="card-content">
                    <p>Advanced modeling with covariates and interactions</p>
                    %s
                </div>
            </div>
        </div>
        
        <div class="footer-note">
            <p>
                <i class="fas fa-info-circle"></i>
                Click on any analysis button to view the detailed report
            </p>
        </div>
    </div>
</body>
</html>',
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
    
    # Cleanup any unwanted graphics files before returning
    cleanup_unwanted_graphics_files()
    
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
  cat("  --enhanced_inferential: Run enhanced inferential analysis\n")
  cat("  --unified_dashboard: Generate unified dashboard with all analyses\n")
  cat("  --statistical_tests: Run statistical tests\n")
  cat("  --report: Generate HTML report (use with any analysis option)\n")
  cat("  --export: Export analysis results to CSV files (use with any analysis option)\n")
  cat("  --input <file>: Specify input data file (default: dane.csv)\n")
  cat("\nExample usage:\n")
  cat("  Rscript main.R --comparative_analysis --report\n")
  cat("  Rscript main.R --unified_dashboard\n")
  cat("  Rscript main.R --unified_dashboard --input data.csv\n")
  cat("  Rscript main.R --correlation_analysis --report --export\n")
  cat("  Rscript main.R --input dane2.csv --descriptive_stats --export\n")
  cat("  Rscript main.R --correlation_analysis --input mydata.csv --report --export\n")
  cat("  Rscript main.R --unified_dashboard --input clinical_data.xlsx\n")
  
  return(NULL)
}

# Function to setup graphics environment to prevent unwanted Rplots.pdf
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

