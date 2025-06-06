# Report Generation Module
# Functions for creating comprehensive statistical analysis reports
# Generates HTML reports with results, plots, and interpretations

library(rmarkdown)
library(knitr)
library(htmltools)
library(DT)
library(plotly)

# Generate HTML report for any analysis module
generate_html_report <- function(analysis_results, analysis_type, output_path = "output/", 
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
        <p>This analysis compared groups across multiple variables using appropriate statistical tests.</p>')
  
  # Process each test result
  if (!is.null(results$test_results)) {
    content <- paste0(content, '<h3>Statistical Test Results</h3>')
    
    for (i in seq_along(results$test_results)) {
      test <- results$test_results[[i]]
      significance_class <- if (test$p_value < 0.05) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', significance_class, '">
            <h4>', test$variable, ' - ', test$test_name, '</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Test Statistic:</strong> ', round(test$statistic, 4), '<br>
                    <strong>p-value:</strong> ', format.pval(test$p_value, digits = 4), '<br>
                    <strong>Effect Size:</strong> ', ifelse(is.null(test$effect_size), "Not calculated", round(test$effect_size, 3)), '
                </div>
                <div class="col-md-6">
                    <strong>Interpretation:</strong> ', 
                    ifelse(test$p_value < 0.05, 
                           "Statistically significant difference between groups", 
                           "No statistically significant difference between groups"), '
                </div>
            </div>
        </div>')
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
        <p>This section provides summary statistics for all variables in the dataset.</p>')
  
  if (!is.null(results$summary_stats)) {
    content <- paste0(content, '
        <h3>Summary Statistics</h3>
        <div class="table-responsive">
            ', create_descriptive_table_html(results$summary_stats), '
        </div>')
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
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Variable</th>
                    <th>N</th>
                    <th>Mean</th>
                    <th>SD</th>
                    <th>Min</th>
                    <th>Max</th>
                    <th>Missing</th>
                </tr>
            </thead>
            <tbody>'
  
  if (is.data.frame(stats_data)) {
    for (i in 1:nrow(stats_data)) {
      html <- paste0(html, '<tr>
                        <td><strong>', rownames(stats_data)[i], '</strong></td>
                        <td>', ifelse(is.na(stats_data$n[i]), "-", stats_data$n[i]), '</td>
                        <td>', ifelse(is.na(stats_data$mean[i]), "-", round(stats_data$mean[i], 2)), '</td>
                        <td>', ifelse(is.na(stats_data$sd[i]), "-", round(stats_data$sd[i], 2)), '</td>
                        <td>', ifelse(is.na(stats_data$min[i]), "-", round(stats_data$min[i], 2)), '</td>
                        <td>', ifelse(is.na(stats_data$max[i]), "-", round(stats_data$max[i], 2)), '</td>
                        <td>', ifelse(is.na(stats_data$missing[i]), "-", stats_data$missing[i]), '</td>
                      </tr>')
    }
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