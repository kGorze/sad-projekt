# Statistical analysis export functions

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
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

# ==============================================================================
# ENHANCED CSV EXPORT INFRASTRUCTURE FOR STATISTICAL ANALYSIS
# ==============================================================================

# Master function to export all analysis results to CSV format
export_all_analysis_to_csv <- function(analysis_result, base_filename = NULL) {
  
  if (!inherits(analysis_result, "analysis_result")) {
    stop("Input must be an analysis_result object")
  }
  
  analysis_type <- analysis_result$analysis_type %||% "unknown_analysis"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (is.null(base_filename)) {
    base_filename <- paste0(analysis_type, "_", timestamp)
  }
  
  exported_files <- list()
  
  cat("Exporting", analysis_type, "analysis results to CSV format...\n")
  
  # Export based on analysis type
  if (analysis_type == "descriptive_stats") {
    exported_files <- export_descriptive_stats_csv(analysis_result, base_filename)
  } else if (analysis_type == "comparative_analysis") {
    exported_files <- export_comparative_analysis_csv(analysis_result, base_filename)
  } else if (analysis_type == "correlation_analysis") {
    exported_files <- export_correlation_analysis_csv(analysis_result, base_filename)
  } else {
    # Generic export for unknown analysis types
    exported_files <- export_generic_analysis_csv(analysis_result, base_filename)
  }
  
  cat("Analysis export completed. Files saved to output/tables/\n")
  return(exported_files)
}

# Export descriptive statistics to CSV files
export_descriptive_stats_csv <- function(analysis_result, base_filename) {
  
  exported_files <- list()
  
  # 1. Export summary statistics for continuous variables
  if (!is.null(analysis_result$summary_data)) {
    filename <- paste0(base_filename, "_descriptive_stats.csv")
    output_path <- file.path("output", "tables", filename)
    
    # Ensure output directory exists
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Ensure it's a data frame before writing
    if (is.data.frame(analysis_result$summary_data)) {
      write_csv(analysis_result$summary_data, output_path)
      exported_files[["descriptive_stats"]] <- output_path
      cat("- Descriptive statistics exported to:", filename, "\n")
    } else {
      cat("- Summary data is not in CSV-compatible format, skipping\n")
    }
  }
  
  # 2. Export categorical data summary
  if (!is.null(analysis_result$categorical_data)) {
    filename <- paste0(base_filename, "_categorical_stats.csv")
    output_path <- file.path("output", "tables", filename)
    
    # Convert categorical stats to data frame if needed
    if (is.list(analysis_result$categorical_data)) {
      categorical_df <- do.call(rbind, lapply(names(analysis_result$categorical_data), function(var) {
        var_data <- analysis_result$categorical_data[[var]]
        if (is.data.frame(var_data)) {
          var_data$variable_name <- var
          return(var_data)
        }
        return(NULL)
      }))
      
      if (!is.null(categorical_df)) {
        write_csv(categorical_df, output_path)
        exported_files[["categorical_stats"]] <- output_path
        cat("- Categorical statistics exported to:", filename, "\n")
      }
    }
  }
  
  # 3. Export normality test results
  if (!is.null(analysis_result$normality_analysis)) {
    filename <- paste0(base_filename, "_normality_tests.csv")
    output_path <- file.path("output", "tables", filename)
    
    normality_df <- create_normality_tests_df(analysis_result$normality_analysis)
    if (!is.null(normality_df) && is.data.frame(normality_df)) {
      write_csv(normality_df, output_path)
      exported_files[["normality_tests"]] <- output_path
      cat("- Normality tests exported to:", filename, "\n")
    } else {
      cat("- Normality tests data not available for CSV export\n")
    }
  }
  
  # 4. Export outlier analysis results
  if (!is.null(analysis_result$outlier_analysis)) {
    filename <- paste0(base_filename, "_outlier_analysis.csv")
    output_path <- file.path("output", "tables", filename)
    
    outlier_df <- create_outlier_analysis_df(analysis_result$outlier_analysis)
    if (!is.null(outlier_df) && is.data.frame(outlier_df)) {
      write_csv(outlier_df, output_path)
      exported_files[["outlier_analysis"]] <- output_path
      cat("- Outlier analysis exported to:", filename, "\n")
    } else {
      cat("- Outlier analysis data not available for CSV export\n")
    }
  }
  
  # 5. Export variable properties table
  if (!is.null(analysis_result$variable_properties)) {
    filename <- paste0(base_filename, "_variable_properties.csv")
    output_path <- file.path("output", "tables", filename)
    
    # Ensure it's a data frame before writing
    if (is.data.frame(analysis_result$variable_properties)) {
      write_csv(analysis_result$variable_properties, output_path)
      exported_files[["variable_properties"]] <- output_path
      cat("- Variable properties exported to:", filename, "\n")
    } else {
      cat("- Variable properties data is not in CSV-compatible format, skipping\n")
    }
  }
  
  return(exported_files)
}

# Export comparative analysis to CSV files
export_comparative_analysis_csv <- function(analysis_result, base_filename) {
  
  exported_files <- list()
  
  # 1. Export statistical test results
  if (!is.null(analysis_result$test_results)) {
    filename <- paste0(base_filename, "_statistical_tests.csv")
    output_path <- file.path("output", "tables", filename)
    
    # Ensure output directory exists
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    test_results_df <- create_test_results_df(analysis_result$test_results)
    if (!is.null(test_results_df) && is.data.frame(test_results_df)) {
      write_csv(test_results_df, output_path)
      exported_files[["statistical_tests"]] <- output_path
      cat("- Statistical test results exported to:", filename, "\n")
    } else {
      cat("- Statistical test results data not available for CSV export\n")
    }
  }
  
  # 2. Export effect sizes (Cohen's D)
  if (!is.null(analysis_result$effect_sizes)) {
    filename <- paste0(base_filename, "_effect_sizes.csv")
    output_path <- file.path("output", "tables", filename)
    
    effect_sizes_df <- create_effect_sizes_df(analysis_result$effect_sizes)
    if (!is.null(effect_sizes_df) && is.data.frame(effect_sizes_df)) {
      write_csv(effect_sizes_df, output_path)
      exported_files[["effect_sizes"]] <- output_path
      cat("- Effect sizes exported to:", filename, "\n")
    } else {
      cat("- Effect sizes data not available for CSV export\n")
    }
  }
  
  # 3. Export distribution analysis results
  if (!is.null(analysis_result$distribution_analysis)) {
    filename <- paste0(base_filename, "_distribution_analysis.csv")
    output_path <- file.path("output", "tables", filename)
    
    distribution_df <- create_distribution_analysis_df(analysis_result$distribution_analysis)
    write_csv(distribution_df, output_path)
    exported_files[["distribution_analysis"]] <- output_path
    cat("- Distribution analysis exported to:", filename, "\n")
  }
  
  # 4. Export homogeneity test results
  if (!is.null(analysis_result$homogeneity_analysis)) {
    filename <- paste0(base_filename, "_homogeneity_tests.csv")
    output_path <- file.path("output", "tables", filename)
    
    homogeneity_df <- create_homogeneity_tests_df(analysis_result$homogeneity_analysis)
    write_csv(homogeneity_df, output_path)
    exported_files[["homogeneity_tests"]] <- output_path
    cat("- Homogeneity test results exported to:", filename, "\n")
  }
  
  # 5. Export regression analysis results
  if (!is.null(analysis_result$regression_analysis)) {
    filename <- paste0(base_filename, "_regression_analysis.csv")
    output_path <- file.path("output", "tables", filename)
    
    regression_df <- create_regression_analysis_df(analysis_result$regression_analysis)
    write_csv(regression_df, output_path)
    exported_files[["regression_analysis"]] <- output_path
    cat("- Regression analysis exported to:", filename, "\n")
  }
  
  return(exported_files)
}

# Export correlation analysis to CSV files
export_correlation_analysis_csv <- function(analysis_result, base_filename) {
  
  exported_files <- list()
  
  # 1. Export overall correlation matrices
  if (!is.null(analysis_result$overall_correlations)) {
    # Pearson correlations
    filename_pearson <- paste0(base_filename, "_pearson_correlations.csv")
    output_path_pearson <- file.path("output", "tables", filename_pearson)
    
    # Ensure output directory exists
    output_dir <- dirname(output_path_pearson)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    if (!is.null(analysis_result$overall_correlations$pearson_matrix)) {
      pearson_df <- as.data.frame(analysis_result$overall_correlations$pearson_matrix)
      pearson_df$variable <- rownames(pearson_df)
      pearson_df <- pearson_df[, c("variable", setdiff(colnames(pearson_df), "variable"))]
      write_csv(pearson_df, output_path_pearson)
      exported_files[["pearson_correlations"]] <- output_path_pearson
      cat("- Pearson correlations exported to:", filename_pearson, "\n")
    }
    
    # Spearman correlations
    filename_spearman <- paste0(base_filename, "_spearman_correlations.csv")
    output_path_spearman <- file.path("output", "tables", filename_spearman)
    
    if (!is.null(analysis_result$overall_correlations$spearman_matrix)) {
      spearman_df <- as.data.frame(analysis_result$overall_correlations$spearman_matrix)
      spearman_df$variable <- rownames(spearman_df)
      spearman_df <- spearman_df[, c("variable", setdiff(colnames(spearman_df), "variable"))]
      write_csv(spearman_df, output_path_spearman)
      exported_files[["spearman_correlations"]] <- output_path_spearman
      cat("- Spearman correlations exported to:", filename_spearman, "\n")
    }
  }
  
  # 2. Export correlation significance tests
  if (!is.null(analysis_result$correlation_tests)) {
    filename <- paste0(base_filename, "_correlation_significance.csv")
    output_path <- file.path("output", "tables", filename)
    
    correlation_tests_df <- create_correlation_tests_df(analysis_result$correlation_tests)
    write_csv(correlation_tests_df, output_path)
    exported_files[["correlation_significance"]] <- output_path
    cat("- Correlation significance tests exported to:", filename, "\n")
  }
  
  # 3. Export group-wise correlations
  if (!is.null(analysis_result$group_correlations)) {
    filename <- paste0(base_filename, "_group_correlations.csv")
    output_path <- file.path("output", "tables", filename)
    
    group_correlations_df <- create_group_correlations_df(analysis_result$group_correlations)
    write_csv(group_correlations_df, output_path)
    exported_files[["group_correlations"]] <- output_path
    cat("- Group-wise correlations exported to:", filename, "\n")
  }
  
  # 4. Export correlation summary
  if (!is.null(analysis_result$correlation_summary)) {
    filename <- paste0(base_filename, "_correlation_summary.csv")
    output_path <- file.path("output", "tables", filename)
    
    if (is.data.frame(analysis_result$correlation_summary)) {
      write_csv(analysis_result$correlation_summary, output_path)
      exported_files[["correlation_summary"]] <- output_path
      cat("- Correlation summary exported to:", filename, "\n")
    }
  }
  
  return(exported_files)
}

# Generic export for unknown analysis types
export_generic_analysis_csv <- function(analysis_result, base_filename) {
  
  exported_files <- list()
  
  # Export any data frames found in the analysis result
  for (element_name in names(analysis_result)) {
    element <- analysis_result[[element_name]]
    
    if (is.data.frame(element)) {
      filename <- paste0(base_filename, "_", element_name, ".csv")
      output_path <- file.path("output", "tables", filename)
      
      # Ensure output directory exists
      output_dir <- dirname(output_path)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      write_csv(element, output_path)
      exported_files[[element_name]] <- output_path
      cat("- Generic data exported to:", filename, "\n")
    }
  }
  
  return(exported_files)
}

# Test results data frame creation
create_test_results_df <- function(test_results) {
  
  if (is.null(test_results) || length(test_results) == 0) {
    return(NULL)
  }
  
  results_list <- list()
  
  for (var_name in names(test_results)) {
    var_tests <- test_results[[var_name]]
    
    if (is.list(var_tests)) {
      for (test_name in names(var_tests)) {
        test_data <- var_tests[[test_name]]
        
        if (is.list(test_data)) {
          results_list[[length(results_list) + 1]] <- data.frame(
            variable = var_name,
            test_name = test_name,
            statistic = test_data$statistic %||% NA,
            p_value = test_data$p_value %||% NA,
            effect_size = test_data$effect_size %||% NA,
            confidence_interval_lower = if(!is.null(test_data$conf.int)) test_data$conf.int[1] else NA,
            confidence_interval_upper = if(!is.null(test_data$conf.int)) test_data$conf.int[2] else NA,
            significant = (test_data$p_value %||% 1) < 0.05,
            interpretation = test_data$interpretation %||% "",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(results_list) > 0) {
    return(do.call(rbind, results_list))
  }
  
  return(NULL)
}

# Effect sizes data frame creation
create_effect_sizes_df <- function(effect_sizes) {
  
  if (is.null(effect_sizes) || length(effect_sizes) == 0) {
    return(NULL)
  }
  
  effect_size_list <- list()
  
  for (var_name in names(effect_sizes)) {
    var_effect_sizes <- effect_sizes[[var_name]]
    
    if (is.list(var_effect_sizes)) {
      for (comparison in names(var_effect_sizes)) {
        effect_data <- var_effect_sizes[[comparison]]
        
        # Handle different effect size data structures
        cohens_d_value <- NA
        interpretation <- ""
        conf_lower <- NA
        conf_upper <- NA
        
        if (is.list(effect_data)) {
          cohens_d_value <- effect_data$cohens_d %||% effect_data$d %||% effect_data$estimate %||% NA
          interpretation <- effect_data$interpretation %||% effect_data$magnitude %||% ""
          if (!is.null(effect_data$conf.int)) {
            conf_lower <- effect_data$conf.int[1]
            conf_upper <- effect_data$conf.int[2]
          }
        } else if (is.numeric(effect_data)) {
          # If effect_data is just a numeric value (Cohen's D)
          cohens_d_value <- effect_data
          interpretation <- ifelse(abs(cohens_d_value) < 0.2, "small",
                                 ifelse(abs(cohens_d_value) < 0.5, "medium", "large"))
        }
        
        effect_size_list[[length(effect_size_list) + 1]] <- data.frame(
          variable = var_name,
          comparison = comparison,
          cohens_d = cohens_d_value,
          effect_size_interpretation = interpretation,
          confidence_interval_lower = conf_lower,
          confidence_interval_upper = conf_upper,
          stringsAsFactors = FALSE
        )
      }
    } else if (is.numeric(var_effect_sizes)) {
      # If the effect size is just a single numeric value
      effect_size_list[[length(effect_size_list) + 1]] <- data.frame(
        variable = var_name,
        comparison = "overall",
        cohens_d = var_effect_sizes,
        effect_size_interpretation = ifelse(abs(var_effect_sizes) < 0.2, "small",
                                          ifelse(abs(var_effect_sizes) < 0.5, "medium", "large")),
        confidence_interval_lower = NA,
        confidence_interval_upper = NA,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(effect_size_list) > 0) {
    return(do.call(rbind, effect_size_list))
  }
  
  return(NULL)
}

# Normality tests data frame creation
create_normality_tests_df <- function(normality_analysis) {
  
  if (is.null(normality_analysis) || length(normality_analysis) == 0) {
    return(NULL)
  }
  
  normality_list <- list()
  
  for (var_name in names(normality_analysis)) {
    var_normality <- normality_analysis[[var_name]]
    
    # Overall normality test
    if (!is.null(var_normality$overall_normality)) {
      overall <- var_normality$overall_normality
      normality_list[[length(normality_list) + 1]] <- data.frame(
        variable = var_name,
        group = "Overall",
        test_name = overall$test %||% "Unknown",
        statistic = overall$statistic %||% NA,
        p_value = overall$p_value %||% NA,
        is_normal = overall$normal %||% FALSE,
        interpretation = overall$interpretation %||% "",
        stringsAsFactors = FALSE
      )
    }
    
    # Group-wise normality tests
    if (!is.null(var_normality$group_normality)) {
      for (group_name in names(var_normality$group_normality)) {
        group_test <- var_normality$group_normality[[group_name]]
        
        normality_list[[length(normality_list) + 1]] <- data.frame(
          variable = var_name,
          group = group_name,
          test_name = group_test$test %||% "Unknown",
          statistic = group_test$statistic %||% NA,
          p_value = group_test$p_value %||% NA,
          is_normal = group_test$normal %||% FALSE,
          interpretation = group_test$interpretation %||% "",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(normality_list) > 0) {
    return(do.call(rbind, normality_list))
  }
  
  return(NULL)
}

# Outlier analysis data frame creation
create_outlier_analysis_df <- function(outlier_analysis) {
  
  if (is.null(outlier_analysis) || length(outlier_analysis) == 0) {
    return(NULL)
  }
  
  outlier_list <- list()
  
  for (var_name in names(outlier_analysis)) {
    var_outliers <- outlier_analysis[[var_name]]
    
    if (is.list(var_outliers)) {
      # Overall outliers
      if (!is.null(var_outliers$overall_outliers)) {
        overall_outliers <- var_outliers$overall_outliers
        outlier_list[[length(outlier_list) + 1]] <- data.frame(
          variable = var_name,
          group = "Overall",
          n_outliers = length(overall_outliers$outlier_indices %||% c()),
          outlier_method = "IQR",
          lower_bound = overall_outliers$lower_bound %||% NA,
          upper_bound = overall_outliers$upper_bound %||% NA,
          stringsAsFactors = FALSE
        )
      }
      
      # Group-wise outliers
      if (!is.null(var_outliers$group_outliers)) {
        for (group_name in names(var_outliers$group_outliers)) {
          group_outliers <- var_outliers$group_outliers[[group_name]]
          
          outlier_list[[length(outlier_list) + 1]] <- data.frame(
            variable = var_name,
            group = group_name,
            n_outliers = length(group_outliers$outlier_indices %||% c()),
            outlier_method = "IQR",
            lower_bound = group_outliers$lower_bound %||% NA,
            upper_bound = group_outliers$upper_bound %||% NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(outlier_list) > 0) {
    return(do.call(rbind, outlier_list))
  }
  
  return(NULL)
}

# Distribution analysis data frame creation
create_distribution_analysis_df <- function(distribution_analysis) {
  
  if (is.null(distribution_analysis) || length(distribution_analysis) == 0) {
    return(NULL)
  }
  
  distribution_list <- list()
  
  for (var_name in names(distribution_analysis)) {
    var_dist <- distribution_analysis[[var_name]]
    
    if (!is.null(var_dist$descriptive_stats)) {
      for (group_name in names(var_dist$descriptive_stats)) {
        group_stats <- var_dist$descriptive_stats[[group_name]]
        
        distribution_list[[length(distribution_list) + 1]] <- data.frame(
          variable = var_name,
          group = group_name,
          n = group_stats$n %||% NA,
          mean = group_stats$mean %||% NA,
          median = group_stats$median %||% NA,
          sd = group_stats$sd %||% NA,
          min = group_stats$min %||% NA,
          max = group_stats$max %||% NA,
          q25 = group_stats$q25 %||% NA,
          q75 = group_stats$q75 %||% NA,
          skewness = group_stats$skewness %||% NA,
          kurtosis = group_stats$kurtosis %||% NA,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(distribution_list) > 0) {
    return(do.call(rbind, distribution_list))
  }
  
  return(NULL)
}

# Homogeneity tests data frame creation
create_homogeneity_tests_df <- function(homogeneity_analysis) {
  
  if (is.null(homogeneity_analysis) || length(homogeneity_analysis) == 0) {
    return(NULL)
  }
  
  homogeneity_list <- list()
  
  for (var_name in names(homogeneity_analysis)) {
    var_homogeneity <- homogeneity_analysis[[var_name]]
    
    if (is.list(var_homogeneity)) {
      homogeneity_list[[length(homogeneity_list) + 1]] <- data.frame(
        variable = var_name,
        test_name = var_homogeneity$test_name %||% "Levene Test",
        statistic = var_homogeneity$statistic %||% NA,
        p_value = var_homogeneity$p_value %||% NA,
        equal_variances = var_homogeneity$equal_variances %||% FALSE,
        interpretation = var_homogeneity$interpretation %||% "",
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(homogeneity_list) > 0) {
    return(do.call(rbind, homogeneity_list))
  }
  
  return(NULL)
}

# Regression analysis data frame creation
create_regression_analysis_df <- function(regression_analysis) {
  
  if (is.null(regression_analysis) || length(regression_analysis) == 0) {
    return(NULL)
  }
  
  regression_list <- list()
  
  for (var_name in names(regression_analysis)) {
    var_regression <- regression_analysis[[var_name]]
    
    if (is.list(var_regression) && !is.null(var_regression$summary)) {
      coefficients <- var_regression$summary$coefficients
      
      for (i in 1:nrow(coefficients)) {
        regression_list[[length(regression_list) + 1]] <- data.frame(
          dependent_variable = var_name,
          predictor = rownames(coefficients)[i],
          coefficient = coefficients[i, "Estimate"],
          std_error = coefficients[i, "Std. Error"],
          t_statistic = coefficients[i, "t value"],
          p_value = coefficients[i, "Pr(>|t|)"],
          significant = coefficients[i, "Pr(>|t|)"] < 0.05,
          r_squared = var_regression$summary$r.squared %||% NA,
          adjusted_r_squared = var_regression$summary$adj.r.squared %||% NA,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(regression_list) > 0) {
    return(do.call(rbind, regression_list))
  }
  
  return(NULL)
}

# Correlation tests data frame creation
create_correlation_tests_df <- function(correlation_tests) {
  
  if (is.null(correlation_tests) || length(correlation_tests) == 0) {
    return(NULL)
  }
  
  correlation_list <- list()
  
  # Overall correlation tests
  if (!is.null(correlation_tests$overall_tests)) {
    for (test_name in names(correlation_tests$overall_tests)) {
      test_result <- correlation_tests$overall_tests[[test_name]]
      
      if (is.list(test_result)) {
        correlation_list[[length(correlation_list) + 1]] <- data.frame(
          variable_pair = test_name,
          group = "Overall",
          correlation_method = test_result$method %||% "Unknown",
          correlation_coefficient = test_result$estimate %||% NA,
          p_value = test_result$p.value %||% NA,
          significant = (test_result$p.value %||% 1) < 0.05,
          confidence_interval_lower = if(!is.null(test_result$conf.int)) test_result$conf.int[1] else NA,
          confidence_interval_upper = if(!is.null(test_result$conf.int)) test_result$conf.int[2] else NA,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # Group-wise correlation tests
  if (!is.null(correlation_tests$group_tests)) {
    for (group_name in names(correlation_tests$group_tests)) {
      group_tests <- correlation_tests$group_tests[[group_name]]
      
      for (test_name in names(group_tests)) {
        test_result <- group_tests[[test_name]]
        
        if (is.list(test_result)) {
          correlation_list[[length(correlation_list) + 1]] <- data.frame(
            variable_pair = test_name,
            group = group_name,
            correlation_method = test_result$method %||% "Unknown",
            correlation_coefficient = test_result$estimate %||% NA,
            p_value = test_result$p.value %||% NA,
            significant = (test_result$p.value %||% 1) < 0.05,
            confidence_interval_lower = if(!is.null(test_result$conf.int)) test_result$conf.int[1] else NA,
            confidence_interval_upper = if(!is.null(test_result$conf.int)) test_result$conf.int[2] else NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(correlation_list) > 0) {
    return(do.call(rbind, correlation_list))
  }
  
  return(NULL)
}

# Group correlations data frame creation
create_group_correlations_df <- function(group_correlations) {
  
  if (is.null(group_correlations) || length(group_correlations) == 0) {
    return(NULL)
  }
  
  group_corr_list <- list()
  
  for (group_name in names(group_correlations)) {
    group_data <- group_correlations[[group_name]]
    
    if (!is.null(group_data$pearson_matrix)) {
      pearson_matrix <- group_data$pearson_matrix
      n_vars <- nrow(pearson_matrix)
      
      for (i in 1:(n_vars-1)) {
        for (j in (i+1):n_vars) {
          group_corr_list[[length(group_corr_list) + 1]] <- data.frame(
            group = group_name,
            variable1 = rownames(pearson_matrix)[i],
            variable2 = colnames(pearson_matrix)[j],
            pearson_correlation = pearson_matrix[i, j],
            spearman_correlation = if(!is.null(group_data$spearman_matrix)) group_data$spearman_matrix[i, j] else NA,
            pearson_p_value = if(!is.null(group_data$pearson_p_values)) group_data$pearson_p_values[i, j] else NA,
            spearman_p_value = if(!is.null(group_data$spearman_p_values)) group_data$spearman_p_values[i, j] else NA,
            pearson_significant = if(!is.null(group_data$pearson_p_values)) group_data$pearson_p_values[i, j] < 0.05 else FALSE,
            spearman_significant = if(!is.null(group_data$spearman_p_values)) group_data$spearman_p_values[i, j] < 0.05 else FALSE,
            n_observations = group_data$n_observations %||% NA,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (length(group_corr_list) > 0) {
    return(do.call(rbind, group_corr_list))
  }
  
  return(NULL)
}

# Quick export function for any analysis result
quick_csv_export <- function(analysis_result, filename = NULL) {
  
  if (is.null(filename)) {
    analysis_type <- analysis_result$analysis_type %||% "analysis"
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0(analysis_type, "_", timestamp)
  }
  
  exported_files <- export_all_analysis_to_csv(analysis_result, filename)
  
  cat("\nQuick CSV export completed!\n")
  cat("Files exported to output/tables/:\n")
  for (file_type in names(exported_files)) {
    cat("-", basename(exported_files[[file_type]]), "\n")
  }
  
  return(exported_files)
} 