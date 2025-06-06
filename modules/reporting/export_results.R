# Results Export Module
# Functions for exporting analysis results to various formats
# Supports CSV, Excel, PDF, and other output formats
# Updated to work with HTML reporting system

# Load required libraries with error handling
if (!require(writexl, quietly = TRUE)) {
  install.packages("writexl", repos = "https://cran.r-project.org")
  library(writexl)
}

if (!require(readr, quietly = TRUE)) {
  install.packages("readr", repos = "https://cran.r-project.org")
  library(readr)
}

# Create standardized result structure for reporting
create_analysis_result <- function(analysis_type, test_results = NULL, summary_data = NULL, 
                                  plots = NULL, metadata = NULL) {
  
  result <- list(
    analysis_type = analysis_type,
    timestamp = Sys.time(),
    test_results = test_results,
    summary_data = summary_data,
    plots = plots,
    metadata = metadata %||% list()
  )
  
  class(result) <- c("analysis_result", "list")
  return(result)
}

# Add test result to analysis results
add_test_result <- function(analysis_result, variable_name, test_name, statistic, 
                           p_value, effect_size = NULL, confidence_interval = NULL,
                           interpretation = NULL) {
  
  test_result <- list(
    variable = variable_name,
    test_name = test_name,
    statistic = statistic,
    p_value = p_value,
    effect_size = effect_size,
    confidence_interval = confidence_interval,
    interpretation = interpretation,
    significant = p_value < 0.05
  )
  
  if (is.null(analysis_result$test_results)) {
    analysis_result$test_results <- list()
  }
  
  analysis_result$test_results[[length(analysis_result$test_results) + 1]] <- test_result
  
  return(analysis_result)
}

# Generate quick report for any analysis result
quick_report <- function(analysis_result, open_report = FALSE) {
  
  if (!inherits(analysis_result, "analysis_result")) {
    stop("Input must be an analysis_result object created with create_analysis_result()")
  }
  
  # Source the reporting module if not already loaded
  if (!exists("generate_html_report")) {
    source("modules/reporting/generate_report.R")
  }
  
  # Generate the report using fixed output path
  report_file <- generate_html_report(
    analysis_results = analysis_result,
    analysis_type = analysis_result$analysis_type,
    output_path = "output/reports",
    include_plots = !is.null(analysis_result$plots)
  )
  
  # Optionally open the report
  if (open_report && interactive()) {
    browseURL(report_file)
  }
  
  return(report_file)
}

# Export statistical results to CSV
export_results_to_csv <- function(analysis_result, filename = "results.csv") {
  
  output_path <- file.path("output", "tables", filename)
  
  if (is.null(analysis_result$test_results)) {
    warning("No test results to export")
    return(NULL)
  }
  
  # Create data frame from test results
  results_df <- data.frame(
    variable = sapply(analysis_result$test_results, function(x) x$variable %||% "Unknown"),
    test_name = sapply(analysis_result$test_results, function(x) x$test_name %||% "Unknown Test"),
    statistic = sapply(analysis_result$test_results, function(x) x$statistic %||% NA),
    p_value = sapply(analysis_result$test_results, function(x) x$p_value %||% NA),
    effect_size = sapply(analysis_result$test_results, function(x) x$effect_size %||% NA),
    significant = sapply(analysis_result$test_results, function(x) x$significant %||% FALSE),
    stringsAsFactors = FALSE
  )
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Write CSV
  write_csv(results_df, output_path)
  cat("Results exported to:", output_path, "\n")
  
  return(output_path)
}

# Export results to Excel with multiple sheets
export_results_to_excel <- function(analysis_result, filename = "results.xlsx") {
  
  output_path <- file.path("output", "tables", filename)
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Prepare data for Excel export
  excel_data <- list()
  
  # Test results sheet
  if (!is.null(analysis_result$test_results)) {
    test_results_df <- data.frame(
      variable = sapply(analysis_result$test_results, function(x) x$variable %||% "Unknown"),
      test_name = sapply(analysis_result$test_results, function(x) x$test_name %||% "Unknown Test"),
      statistic = sapply(analysis_result$test_results, function(x) x$statistic %||% NA),
      p_value = sapply(analysis_result$test_results, function(x) x$p_value %||% NA),
      effect_size = sapply(analysis_result$test_results, function(x) x$effect_size %||% NA),
      significant = sapply(analysis_result$test_results, function(x) x$significant %||% FALSE),
      stringsAsFactors = FALSE
    )
    excel_data[["Test_Results"]] <- test_results_df
  }
  
  # Summary data sheet
  if (!is.null(analysis_result$summary_data)) {
    excel_data[["Summary_Statistics"]] <- analysis_result$summary_data
  }
  
  # Metadata sheet
  metadata_df <- data.frame(
    Property = c("Analysis Type", "Timestamp", "Number of Tests"),
    Value = c(
      analysis_result$analysis_type %||% "Unknown",
      as.character(analysis_result$timestamp %||% Sys.time()),
      length(analysis_result$test_results %||% list())
    ),
    stringsAsFactors = FALSE
  )
  excel_data[["Metadata"]] <- metadata_df
  
  # Write Excel file
  write_xlsx(excel_data, output_path)
  cat("Results exported to:", output_path, "\n")
  
  return(output_path)
}

# Export plots to various formats
export_plots <- function(plots, subdirectory = "general") {
  
  output_directory <- file.path("output", "plots", subdirectory)
  
  if (is.null(plots) || length(plots) == 0) {
    warning("No plots to export")
    return(NULL)
  }
  
  # Create output directory
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  exported_files <- c()
  
  for (i in seq_along(plots)) {
    plot_name <- names(plots)[i] %||% paste0("plot_", i)
    
    # Export as PNG
    png_file <- file.path(output_directory, paste0(plot_name, ".png"))
    ggsave(png_file, plots[[i]], width = 10, height = 8, dpi = 300)
    exported_files <- c(exported_files, png_file)
    
    # Export as PDF
    pdf_file <- file.path(output_directory, paste0(plot_name, ".pdf"))
    ggsave(pdf_file, plots[[i]], width = 10, height = 8)
    exported_files <- c(exported_files, pdf_file)
  }
  
  cat("Plots exported to:", output_directory, "\n")
  return(exported_files)
}

# Create data dictionary
create_data_dictionary <- function(data, filename = "data_dictionary.csv") {
  
  output_path <- file.path("output", "tables", filename)
  
  # Create data dictionary data frame
  dict_df <- data.frame(
    variable = names(data),
    type = sapply(data, class),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    n_unique = sapply(data, function(x) length(unique(x[!is.na(x)]))),
    example_values = sapply(data, function(x) {
      unique_vals <- unique(x[!is.na(x)])
      if (length(unique_vals) > 3) {
        paste(head(unique_vals, 3), collapse = ", ")
      } else {
        paste(unique_vals, collapse = ", ")
      }
    }),
    stringsAsFactors = FALSE
  )
  
  # Add summary statistics for numeric variables
  numeric_vars <- sapply(data, is.numeric)
  dict_df$mean <- NA
  dict_df$sd <- NA
  dict_df$min <- NA
  dict_df$max <- NA
  
  if (sum(numeric_vars) > 0) {
    dict_df$mean[numeric_vars] <- sapply(data[, numeric_vars, drop=FALSE], mean, na.rm = TRUE)
    dict_df$sd[numeric_vars] <- sapply(data[, numeric_vars, drop=FALSE], sd, na.rm = TRUE)
    dict_df$min[numeric_vars] <- sapply(data[, numeric_vars, drop=FALSE], min, na.rm = TRUE)
    dict_df$max[numeric_vars] <- sapply(data[, numeric_vars, drop=FALSE], max, na.rm = TRUE)
  }
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Write data dictionary
  write_csv(dict_df, output_path)
  cat("Data dictionary created:", output_path, "\n")
  
  return(dict_df)
}

# Export summary tables
export_summary_tables <- function(tables, filename = "summary_tables.xlsx") {
  
  output_path <- file.path("output", "tables", filename)
  
  if (is.null(tables) || length(tables) == 0) {
    warning("No tables to export")
    return(NULL)
  }
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # If tables is a single data frame, convert to list
  if (is.data.frame(tables)) {
    tables <- list("Summary_Table" = tables)
  }
  
  # Write Excel file with multiple sheets
  write_xlsx(tables, output_path)
  cat("Summary tables exported to:", output_path, "\n")
  
  return(output_path)
}

# Create analysis log
create_analysis_log <- function(analysis_steps, filename = "analysis_log.txt") {
  
  output_path <- file.path("output", "logs", filename)
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create log content
  log_content <- c(
    paste("Analysis Log - Generated on:", Sys.time()),
    paste(rep("=", 50), collapse = ""),
    ""
  )
  
  if (!is.null(analysis_steps) && length(analysis_steps) > 0) {
    for (i in seq_along(analysis_steps)) {
      log_content <- c(
        log_content,
        paste("Step", i, ":", analysis_steps[i]),
        ""
      )
    }
  } else {
    log_content <- c(log_content, "No analysis steps recorded.")
  }
  
  # Write log file
  writeLines(log_content, output_path)
  cat("Analysis log created:", output_path, "\n")
  
  return(output_path)
} 