# Report Generation Module
# Functions for creating comprehensive statistical analysis reports
# Generates HTML reports with results, plots, and interpretations

# Load required libraries with error handling
if (!require(rmarkdown, quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cran.r-project.org")
  library(rmarkdown)
}

if (!require(knitr, quietly = TRUE)) {
  install.packages("knitr", repos = "https://cran.r-project.org")
  library(knitr)
}

if (!require(htmltools, quietly = TRUE)) {
  install.packages("htmltools", repos = "https://cran.r-project.org")
  library(htmltools)
}

# Optional libraries - load if available, but don't require
tryCatch({
  library(DT)
}, error = function(e) {
  # DT not available, skip
})

tryCatch({
  library(plotly)
}, error = function(e) {
  # plotly not available, skip
})

# Generate HTML report for any analysis module
generate_html_report <- function(analysis_results, analysis_type, output_path = "output/reports", 
                                 title = NULL, include_plots = TRUE) {
  
  if (is.null(title)) {
    title <- paste("Statistical Analysis Report -", stringr::str_to_title(gsub("_", " ", analysis_type)))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # Generate HTML content
  html_content <- create_html_report_content(analysis_results, analysis_type, title, include_plots)
  
  # Write HTML file
  report_filename <- file.path(output_path, paste0(analysis_type, "_report_", 
                                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"))
  
  writeLines(html_content, report_filename)
  
  cat("HTML report generated:", report_filename, "\n")
  return(report_filename)
}

# Create complete HTML report content
create_html_report_content <- function(analysis_results, analysis_type, title, include_plots) {
  
  # HTML header with Bootstrap CSS for styling
  html_header <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>', title, '</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <style>
        body { font-family: "Arial", sans-serif; margin: 20px; }
        .result-section { margin-bottom: 30px; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }
        .test-result { background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px; }
        .significant { border-left: 5px solid #28a745; }
        .not-significant { border-left: 5px solid #dc3545; }
        .warning { border-left: 5px solid #ffc107; }
        .plot-container { text-align: center; margin: 20px 0; }
        .stats-table { font-size: 0.9em; }
        .interpretation { background-color: #e9ecef; padding: 15px; border-radius: 5px; font-style: italic; }
        .timestamp { color: #6c757d; font-size: 0.8em; }
    </style>
</head>
<body>
    <div class="container-fluid">
        <div class="row">
            <div class="col-12">
                <h1 class="text-center mb-4">', title, '</h1>
                <p class="text-center timestamp">Generated on: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
            </div>
        </div>')
  
  # Generate body content based on analysis type
  html_body <- create_analysis_specific_content(analysis_results, analysis_type, include_plots)
  
  # HTML footer
  html_footer <- '
    </div>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>'
  
  return(paste0(html_header, html_body, html_footer))
}

# Create analysis-specific content
create_analysis_specific_content <- function(analysis_results, analysis_type, include_plots) {
  
  content <- switch(analysis_type,
    "comparative_analysis" = create_comparative_analysis_content(analysis_results, include_plots),
    "correlation_analysis" = create_correlation_analysis_content(analysis_results, include_plots),
    "descriptive_stats" = create_descriptive_stats_content(analysis_results, include_plots),
    "statistical_tests" = create_statistical_tests_content(analysis_results, include_plots),
    # Default content for any analysis
    create_generic_analysis_content(analysis_results, include_plots)
  )
  
  return(content)
}

# Create content for comparative analysis
create_comparative_analysis_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  # Summary section
  content <- paste0(content, '
    <div class="result-section">
        <h2>Comparative Analysis Summary</h2>
        <p>This analysis compared groups across multiple variables using appropriate statistical tests based on distribution and homogeneity assumptions.</p>')
  
  # Metadata overview
  if (!is.null(results$metadata)) {
    content <- paste0(content, '
        <h3>Analysis Overview</h3>
        <div class="interpretation">
            <strong>Groups Analyzed:</strong> ', paste(results$metadata$groups, collapse = ", "), '<br>
            <strong>Group Sizes:</strong> ', paste(names(results$metadata$group_sizes), "=", results$metadata$group_sizes, collapse = ", "), '<br>
            <strong>Continuous Variables:</strong> ', results$metadata$numeric_variables, '<br>
            <strong>Categorical Variables:</strong> ', results$metadata$categorical_variables, '<br>
            <strong>Total Observations:</strong> ', results$metadata$total_observations, '
        </div>')
  }
  
  # Distribution Analysis Results
  if (!is.null(results$distribution_analysis)) {
    content <- paste0(content, '
        <h3>Distribution Analysis</h3>
        <p>Assessment of normality for each variable overall and by group:</p>')
    
    for (var_name in names(results$distribution_analysis)) {
      var_data <- results$distribution_analysis[[var_name]]
      content <- paste0(content, '
        <h4>', var_name, '</h4>
        <div class="test-result">
            <strong>Overall Normality:</strong> ', var_data$overall_normality$interpretation, '<br>')
      
      # Group-wise normality
      if (!is.null(var_data$group_normality)) {
        content <- paste0(content, '<strong>Group-wise Normality:</strong><br>')
        for (group_name in names(var_data$group_normality)) {
          group_result <- var_data$group_normality[[group_name]]
          content <- paste0(content, '&nbsp;&nbsp;• ', group_name, ': ', group_result$interpretation, '<br>')
        }
      }
      
      content <- paste0(content, '</div>')
    }
  }
  
  # Homogeneity Analysis Results
  if (!is.null(results$homogeneity_analysis)) {
    content <- paste0(content, '
        <h3>Homogeneity of Variances</h3>
        <p>Assessment of variance homogeneity across groups:</p>')
    
    for (var_name in names(results$homogeneity_analysis)) {
      var_data <- results$homogeneity_analysis[[var_name]]
      homo_class <- if (!is.na(var_data$homogeneous) && var_data$homogeneous) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', homo_class, '">
            <h4>', var_name, '</h4>
            <strong>Test:</strong> ', var_data$test, '<br>
            <strong>Result:</strong> ', var_data$interpretation, '
        </div>')
    }
  }
  
  # Test Recommendations
  if (!is.null(results$test_recommendations)) {
    content <- paste0(content, '
        <h3>Statistical Test Recommendations</h3>
        <p>Based on distribution and homogeneity analysis, the following tests were selected:</p>')
    
    for (var_name in names(results$test_recommendations)) {
      recommendation <- results$test_recommendations[[var_name]]
      content <- paste0(content, '
        <div class="interpretation">
            <strong>', var_name, ':</strong> ', recommendation$test_type, '<br>
            <em>Reasoning:</em> ', recommendation$reasoning, '
        </div>')
    }
  }
  
  # Statistical Test Results
  if (!is.null(results$test_results)) {
    content <- paste0(content, '
        <h3>Statistical Test Results</h3>
        <p>Results of group comparisons:</p>')
    
    for (var_name in names(results$test_results)) {
      test <- results$test_results[[var_name]]
      significance_class <- if (!is.na(test$p_value) && test$p_value < 0.05) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', significance_class, '">
            <h4>', var_name, ' - ', test$test_name, '</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Test Statistic:</strong> ', 
                    ifelse(is.null(test$statistic), "Not available", round(test$statistic, 4)), '<br>
                    <strong>p-value:</strong> ', 
                    ifelse(is.null(test$p_value), "Not available", format.pval(test$p_value, digits = 4)), '<br>
                    <strong>Effect Size:</strong> ', 
                    ifelse(is.null(test$effect_size), "Not calculated", round(test$effect_size, 3)), '
                </div>
                <div class="col-md-6">
                    <strong>Interpretation:</strong> ', test$interpretation, '
                </div>
            </div>')
      
      # Post-hoc results if available
      if (!is.null(test$posthoc) && !is.null(test$posthoc$significant_pairs)) {
        if (length(test$posthoc$significant_pairs) > 0) {
          content <- paste0(content, '
            <div class="mt-2">
                <strong>Post-hoc Analysis (', test$posthoc$test_name, '):</strong><br>
                <em>Significant pairwise differences:</em> ', paste(test$posthoc$significant_pairs, collapse = ", "), '
            </div>')
        } else {
          content <- paste0(content, '
            <div class="mt-2">
                <strong>Post-hoc Analysis:</strong> No significant pairwise differences found
            </div>')
        }
      }
      
      # Additional notes if available
      if (!is.null(test$note)) {
        content <- paste0(content, '
            <div class="mt-2">
                <em>Note:</em> ', test$note, '
            </div>')
      }
      
      content <- paste0(content, '</div>')
    }
  }
  
  # Analysis metadata
  if (!is.null(results$metadata)) {
    content <- paste0(content, '
        <h3>Analysis Metadata</h3>
        <div class="interpretation">
            <strong>Analysis Date:</strong> ', format(results$metadata$analysis_date, "%Y-%m-%d %H:%M:%S"), '<br>
            <strong>Group Column:</strong> ', results$metadata$group_column, '<br>
            <strong>Statistical Approach:</strong> Comprehensive assumption testing followed by appropriate test selection
        </div>')
  }
  
  # Include plots if available
  if (include_plots && !is.null(results$plot_files) && length(results$plot_files) > 0) {
    content <- paste0(content, '
        <h3>Visualizations</h3>
        <p>The following plots illustrate the group comparisons:</p>')
    
    for (plot_name in names(results$plot_files)) {
      plot_file <- results$plot_files[[plot_name]]
      
      # Convert absolute path to relative path for HTML
      if (file.exists(plot_file)) {
        # Get relative path from the report location
        relative_plot_path <- file.path("..", "plots", "comparative_analysis", basename(plot_file))
        
        # Create a nice title for the plot
        plot_title <- gsub("_", " ", gsub("boxplot_|barplot_", "", plot_name))
        plot_title <- stringr::str_to_title(plot_title)
        
        content <- paste0(content, '
        <div class="plot-container">
            <h4>', plot_title, '</h4>
            <img src="', relative_plot_path, '" alt="', plot_title, '" class="img-fluid" style="max-width: 100%; height: auto;">
        </div>')
      }
    }
  }
  
  content <- paste0(content, '</div></div></div>')
  return(content)
}

# Create content for correlation analysis
create_correlation_analysis_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  content <- paste0(content, '
    <div class="result-section">
        <h2>Correlation Analysis Summary</h2>
        <p>This analysis examined relationships between variables using correlation coefficients.</p>')
  
  if (!is.null(results$correlation_matrix)) {
    content <- paste0(content, '
        <h3>Correlation Matrix</h3>
        <div class="table-responsive">
            ', create_correlation_table_html(results$correlation_matrix), '
        </div>')
  }
  
  content <- paste0(content, '</div></div></div>')
  return(content)
}

# Create content for descriptive statistics
create_descriptive_stats_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  content <- paste0(content, '
    <div class="result-section">
        <h2>Descriptive Statistics Summary</h2>
        <p>This section provides comprehensive summary statistics for all variables in the dataset.</p>')
  
  # Dataset overview
  if (!is.null(results$data_summary) && !is.null(results$data_summary$dataset_overview)) {
    content <- paste0(content, '
        <h3>Dataset Overview</h3>
        <div class="table-responsive">
            ', create_overview_table_html(results$data_summary$dataset_overview), '
        </div>')
  }
  
  # Variable types summary
  if (!is.null(results$data_summary) && !is.null(results$data_summary$variable_types)) {
    content <- paste0(content, '
        <h3>Variable Types</h3>
        <div class="table-responsive">
            ', create_variable_types_table_html(results$data_summary$variable_types), '
        </div>')
  }
  
  # Group summary if available
  if (!is.null(results$data_summary) && !is.null(results$data_summary$group_summary)) {
    content <- paste0(content, '
        <h3>Group Distribution</h3>
        <div class="table-responsive">
            ', create_group_summary_table_html(results$data_summary$group_summary), '
        </div>')
  }
  
  # Continuous variables statistics
  if (!is.null(results$summary_data)) {
    content <- paste0(content, '
        <h3>Continuous Variables Statistics</h3>
        <div class="table-responsive">
            ', create_descriptive_table_html(results$summary_data), '
        </div>')
  }
  
  # Categorical variables statistics
  if (!is.null(results$categorical_data)) {
    content <- paste0(content, '
        <h3>Categorical Variables Statistics</h3>')
    
    for (var_name in names(results$categorical_data)) {
      var_data <- results$categorical_data[[var_name]]
      content <- paste0(content, '
        <h4>', var_name, '</h4>
        <div class="table-responsive">
            ', create_categorical_table_html(var_data), '
        </div>')
    }
  }
  
  # Missing data summary if available
  if (!is.null(results$data_summary) && !is.null(results$data_summary$missing_data)) {
    content <- paste0(content, '
        <h3>Missing Data Summary</h3>
        <div class="table-responsive">
            ', create_missing_data_table_html(results$data_summary$missing_data), '
        </div>')
  }
  
  # Metadata information
  if (!is.null(results$metadata)) {
    content <- paste0(content, '
        <h3>Analysis Metadata</h3>
        <div class="interpretation">
            <strong>Analysis Date:</strong> ', format(results$metadata$analysis_date, "%Y-%m-%d %H:%M:%S"), '<br>
            <strong>Total Observations:</strong> ', results$metadata$total_observations, '<br>
            <strong>Total Variables:</strong> ', results$metadata$total_variables, '<br>
            <strong>Numeric Variables:</strong> ', results$metadata$numeric_variables, '<br>
            <strong>Categorical Variables:</strong> ', results$metadata$categorical_variables, '<br>
            <strong>Group Column:</strong> ', ifelse(is.null(results$metadata$group_column), "None", results$metadata$group_column), '<br>
            <strong>Missing Data Present:</strong> ', ifelse(results$metadata$missing_data_present, "Yes", "No"), '
        </div>')
  }
  
  # Include plots if available
  if (include_plots && !is.null(results$plot_files) && length(results$plot_files) > 0) {
    content <- paste0(content, '
        <h3>Visualizations</h3>
        <p>The following plots illustrate the data distributions and patterns:</p>')
    
    for (plot_name in names(results$plot_files)) {
      plot_file <- results$plot_files[[plot_name]]
      
      # Convert absolute path to relative path for HTML
      if (file.exists(plot_file)) {
        # Get relative path from the report location
        relative_plot_path <- file.path("..", "plots", "descriptive_stats", basename(plot_file))
        
        # Create a nice title for the plot
        plot_title <- gsub("_", " ", gsub("histogram_|boxplot_|barplot_", "", plot_name))
        plot_title <- stringr::str_to_title(plot_title)
        if (plot_name == "missing_data_summary") {
          plot_title <- "Missing Data Summary"
        }
        
        content <- paste0(content, '
        <div class="plot-container">
            <h4>', plot_title, '</h4>
            <img src="', relative_plot_path, '" alt="', plot_title, '" class="img-fluid" style="max-width: 100%; height: auto;">
        </div>')
      }
    }
  }
  
  content <- paste0(content, '</div></div></div>')
  return(content)
}

# Create content for statistical tests
create_statistical_tests_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  content <- paste0(content, '
    <div class="result-section">
        <h2>Statistical Tests Summary</h2>
        <p>This section provides results from various statistical tests performed on the data.</p>')
  
  # Process test results if available
  if (!is.null(results$test_results)) {
    content <- paste0(content, '<h3>Test Results</h3>')
    
    for (i in seq_along(results$test_results)) {
      test <- results$test_results[[i]]
      significance_class <- if (!is.null(test$p_value) && test$p_value < 0.05) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', significance_class, '">
            <h4>', ifelse(is.null(test$test_name), paste("Test", i), test$test_name), '</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Test Statistic:</strong> ', 
                    ifelse(is.null(test$statistic), "Not available", round(test$statistic, 4)), '<br>
                    <strong>p-value:</strong> ', 
                    ifelse(is.null(test$p_value), "Not available", format.pval(test$p_value, digits = 4)), '<br>
                    <strong>Effect Size:</strong> ', 
                    ifelse(is.null(test$effect_size), "Not calculated", round(test$effect_size, 3)), '
                </div>
                <div class="col-md-6">
                    <strong>Interpretation:</strong> ', 
                    ifelse(!is.null(test$p_value) && test$p_value < 0.05, 
                           "Statistically significant result", 
                           "No statistically significant result"), '
                </div>
            </div>
        </div>')
    }
  } else {
    content <- paste0(content, '<p>No statistical test results available.</p>')
  }
  
  content <- paste0(content, '</div></div></div>')
  return(content)
}

# Create generic content for any analysis
create_generic_analysis_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  content <- paste0(content, '
    <div class="result-section">
        <h2>Analysis Results</h2>
        <p>Statistical analysis completed successfully.</p>')
  
  # Add results summary if available
  if (!is.null(results)) {
    content <- paste0(content, '<h3>Results Summary</h3><pre>', 
                     capture.output(str(results)), '</pre>')
  }
  
  content <- paste0(content, '</div></div></div>')
  return(content)
}

# Helper function to create correlation table HTML
create_correlation_table_html <- function(corr_matrix) {
  if (is.null(corr_matrix)) return("")
  
  html <- '<table class="table table-striped stats-table"><thead><tr><th>Variable</th>'
  for (col in colnames(corr_matrix)) {
    html <- paste0(html, '<th>', col, '</th>')
  }
  html <- paste0(html, '</tr></thead><tbody>')
  
  for (i in 1:nrow(corr_matrix)) {
    html <- paste0(html, '<tr><td><strong>', rownames(corr_matrix)[i], '</strong></td>')
    for (j in 1:ncol(corr_matrix)) {
      value <- corr_matrix[i, j]
      cell_class <- ""
      if (abs(value) > 0.7) cell_class <- "table-success"
      else if (abs(value) > 0.3) cell_class <- "table-warning"
      
      html <- paste0(html, '<td class="', cell_class, '">', round(value, 3), '</td>')
    }
    html <- paste0(html, '</tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create descriptive statistics table HTML
create_descriptive_table_html <- function(stats_data) {
  if (is.null(stats_data)) return("")
  
  # Check if it's grouped data
  if ("group" %in% names(stats_data)) {
    # Grouped data table
    html <- '<table class="table table-striped stats-table">
              <thead>
                  <tr>
                      <th>Variable</th>
                      <th>Group</th>
                      <th>N</th>
                      <th>Missing</th>
                      <th>Mean</th>
                      <th>SD</th>
                      <th>Median</th>
                      <th>Q25</th>
                      <th>Q75</th>
                      <th>Min</th>
                      <th>Max</th>
                  </tr>
              </thead>
              <tbody>'
    
    for (i in 1:nrow(stats_data)) {
      html <- paste0(html, '<tr>
                          <td><strong>', stats_data$variable[i], '</strong></td>
                          <td>', stats_data$group[i], '</td>
                          <td>', ifelse(is.na(stats_data$n[i]), "-", stats_data$n[i]), '</td>
                          <td>', ifelse(is.na(stats_data$missing[i]), "-", stats_data$missing[i]), '</td>
                          <td>', ifelse(is.na(stats_data$mean[i]), "-", stats_data$mean[i]), '</td>
                          <td>', ifelse(is.na(stats_data$sd[i]), "-", stats_data$sd[i]), '</td>
                          <td>', ifelse(is.na(stats_data$median[i]), "-", stats_data$median[i]), '</td>
                          <td>', ifelse(is.na(stats_data$q25[i]), "-", stats_data$q25[i]), '</td>
                          <td>', ifelse(is.na(stats_data$q75[i]), "-", stats_data$q75[i]), '</td>
                          <td>', ifelse(is.na(stats_data$min[i]), "-", stats_data$min[i]), '</td>
                          <td>', ifelse(is.na(stats_data$max[i]), "-", stats_data$max[i]), '</td>
                        </tr>')
    }
  } else {
    # Overall data table with more comprehensive statistics
    html <- '<table class="table table-striped stats-table">
              <thead>
                  <tr>
                      <th>Variable</th>
                      <th>N</th>
                      <th>Missing</th>
                      <th>Mean</th>
                      <th>SD</th>
                      <th>Median</th>
                      <th>Q25</th>
                      <th>Q75</th>
                      <th>Min</th>
                      <th>Max</th>
                      <th>Range</th>
                      <th>IQR</th>
                      <th>CV%</th>
                      <th>Skewness</th>
                      <th>Kurtosis</th>
                  </tr>
              </thead>
              <tbody>'
    
    for (i in 1:nrow(stats_data)) {
      html <- paste0(html, '<tr>
                          <td><strong>', stats_data$variable[i], '</strong></td>
                          <td>', ifelse(is.na(stats_data$n[i]), "-", stats_data$n[i]), '</td>
                          <td>', ifelse(is.na(stats_data$missing[i]), "-", stats_data$missing[i]), '</td>
                          <td>', ifelse(is.na(stats_data$mean[i]), "-", stats_data$mean[i]), '</td>
                          <td>', ifelse(is.na(stats_data$sd[i]), "-", stats_data$sd[i]), '</td>
                          <td>', ifelse(is.na(stats_data$median[i]), "-", stats_data$median[i]), '</td>
                          <td>', ifelse(is.na(stats_data$q25[i]), "-", stats_data$q25[i]), '</td>
                          <td>', ifelse(is.na(stats_data$q75[i]), "-", stats_data$q75[i]), '</td>
                          <td>', ifelse(is.na(stats_data$min[i]), "-", stats_data$min[i]), '</td>
                          <td>', ifelse(is.na(stats_data$max[i]), "-", stats_data$max[i]), '</td>
                          <td>', ifelse(is.na(stats_data$range[i]), "-", stats_data$range[i]), '</td>
                          <td>', ifelse(is.na(stats_data$iqr[i]), "-", stats_data$iqr[i]), '</td>
                          <td>', ifelse(is.na(stats_data$cv[i]), "-", paste0(stats_data$cv[i], "%")), '</td>
                          <td>', ifelse(is.na(stats_data$skewness[i]), "-", stats_data$skewness[i]), '</td>
                          <td>', ifelse(is.na(stats_data$kurtosis[i]), "-", stats_data$kurtosis[i]), '</td>
                        </tr>')
    }
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create overview table HTML
create_overview_table_html <- function(overview_data) {
  if (is.null(overview_data)) return("")
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Metric</th>
                    <th>Value</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(overview_data)) {
    html <- paste0(html, '<tr>
                        <td><strong>', overview_data$metric[i], '</strong></td>
                        <td>', overview_data$value[i], '</td>
                      </tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create variable types table HTML
create_variable_types_table_html <- function(var_types_data) {
  if (is.null(var_types_data)) return("")
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Variable Type</th>
                    <th>Count</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(var_types_data)) {
    html <- paste0(html, '<tr>
                        <td><strong>', var_types_data$type[i], '</strong></td>
                        <td>', var_types_data$count[i], '</td>
                      </tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create group summary table HTML
create_group_summary_table_html <- function(group_data) {
  if (is.null(group_data)) return("")
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Group</th>
                    <th>N</th>
                    <th>Percentage</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(group_data)) {
    html <- paste0(html, '<tr>
                        <td><strong>', group_data$group[i], '</strong></td>
                        <td>', group_data$n[i], '</td>
                        <td>', group_data$percentage[i], '%</td>
                      </tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create categorical table HTML
create_categorical_table_html <- function(cat_data) {
  if (is.null(cat_data)) return("")
  
  if ("group" %in% names(cat_data)) {
    # Grouped categorical data
    html <- '<table class="table table-striped stats-table">
              <thead>
                  <tr>
                      <th>Category</th>
                      <th>Group</th>
                      <th>Frequency</th>
                      <th>Percentage</th>
                  </tr>
              </thead>
              <tbody>'
    
    for (i in 1:nrow(cat_data)) {
      html <- paste0(html, '<tr>
                          <td>', cat_data$category[i], '</td>
                          <td>', cat_data$group[i], '</td>
                          <td>', cat_data$frequency[i], '</td>
                          <td>', cat_data$percentage[i], '%</td>
                        </tr>')
    }
  } else {
    # Overall categorical data
    html <- '<table class="table table-striped stats-table">
              <thead>
                  <tr>
                      <th>Category</th>
                      <th>Frequency</th>
                      <th>Percentage</th>
                  </tr>
              </thead>
              <tbody>'
    
    for (i in 1:nrow(cat_data)) {
      html <- paste0(html, '<tr>
                          <td>', cat_data$category[i], '</td>
                          <td>', cat_data$frequency[i], '</td>
                          <td>', cat_data$percentage[i], '%</td>
                        </tr>')
    }
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create missing data table HTML
create_missing_data_table_html <- function(missing_data) {
  if (is.null(missing_data)) return("")
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Variable</th>
                    <th>Missing Count</th>
                    <th>Missing Percentage</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(missing_data)) {
    # Color code based on missing percentage
    row_class <- ""
    if (missing_data$missing_percentage[i] > 20) {
      row_class <- "table-danger"
    } else if (missing_data$missing_percentage[i] > 10) {
      row_class <- "table-warning"
    }
    
    html <- paste0(html, '<tr class="', row_class, '">
                        <td><strong>', missing_data$variable[i], '</strong></td>
                        <td>', missing_data$missing_count[i], '</td>
                        <td>', missing_data$missing_percentage[i], '%</td>
                      </tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Legacy functions maintained for compatibility
generate_complete_report <- function(analysis_results, output_path) {
  return(generate_html_report(analysis_results, "complete_analysis", output_path))
}

create_report_sections <- function(data, results) {
  # Wrapper for backward compatibility
  return(create_analysis_specific_content(results, "generic", TRUE))
}

format_statistical_results <- function(test_results) {
  # Format statistical results for HTML display
  formatted <- list()
  
  if (is.list(test_results)) {
    for (i in seq_along(test_results)) {
      test <- test_results[[i]]
      formatted[[i]] <- list(
        variable = names(test_results)[i] %||% paste("Test", i),
        test_name = test$method %||% "Statistical Test",
        statistic = test$statistic %||% test$statistic,
        p_value = test$p.value %||% test$p_value,
        effect_size = test$effect_size
      )
    }
  }
  
  return(formatted)
}

generate_methodology_section <- function(tests_used, assumptions_checked) {
  methodology <- paste0('
    <div class="result-section">
        <h2>Methodology</h2>
        <h3>Statistical Tests Used</h3>
        <ul>')
  
  if (!is.null(tests_used)) {
    for (test in tests_used) {
      methodology <- paste0(methodology, '<li>', test, '</li>')
    }
  }
  
  methodology <- paste0(methodology, '</ul>')
  
  if (!is.null(assumptions_checked)) {
    methodology <- paste0(methodology, '
        <h3>Assumptions Checked</h3>
        <ul>')
    for (assumption in assumptions_checked) {
      methodology <- paste0(methodology, '<li>', assumption, '</li>')
    }
    methodology <- paste0(methodology, '</ul>')
  }
  
  methodology <- paste0(methodology, '</div>')
  return(methodology)
}

create_interpretation_guidelines <- function() {
  guidelines <- '
    <div class="result-section">
        <h2>Interpretation Guidelines</h2>
        <div class="interpretation">
            <h4>Statistical Significance</h4>
            <p><strong>p-value &lt; 0.05:</strong> Result is statistically significant</p>
            <p><strong>p-value ≥ 0.05:</strong> Result is not statistically significant</p>
            
            <h4>Effect Sizes</h4>
            <p><strong>Small effect:</strong> Practical significance may be limited</p>
            <p><strong>Medium effect:</strong> Moderate practical significance</p>
            <p><strong>Large effect:</strong> High practical significance</p>
            
            <h4>Correlation Interpretation</h4>
            <p><strong>|r| &lt; 0.3:</strong> Weak correlation</p>
            <p><strong>0.3 ≤ |r| &lt; 0.7:</strong> Moderate correlation</p>
            <p><strong>|r| ≥ 0.7:</strong> Strong correlation</p>
        </div>
    </div>'
  
  return(guidelines)
}

generate_executive_summary <- function(key_findings) {
  summary <- '
    <div class="result-section">
        <h2>Executive Summary</h2>
        <div class="interpretation">'
  
  if (!is.null(key_findings) && length(key_findings) > 0) {
    summary <- paste0(summary, '<h4>Key Findings:</h4><ul>')
    for (finding in key_findings) {
      summary <- paste0(summary, '<li>', finding, '</li>')
    }
    summary <- paste0(summary, '</ul>')
  } else {
    summary <- paste0(summary, '<p>Analysis completed successfully. Please review the detailed results below.</p>')
  }
  
  summary <- paste0(summary, '</div></div>')
  return(summary)
} 