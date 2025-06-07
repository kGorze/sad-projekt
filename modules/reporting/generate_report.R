# Report Generation Module
# Functions for generating comprehensive statistical reports in multiple formats

# Load required libraries with error handling
# NOTE: Packages are now loaded centrally in config.R - no individual loading needed

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
}

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
    "enhanced_inferential" = create_enhanced_inferential_content(analysis_results, include_plots),
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
      # Use levene test result as the primary homogeneity indicator
      homogeneous <- if (!is.null(var_data$levene_test) && !is.null(var_data$levene_test$homogeneous)) {
        var_data$levene_test$homogeneous
      } else {
        FALSE
      }
      homo_class <- if (!is.na(homogeneous) && homogeneous) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', homo_class, '">
            <h4>', var_name, '</h4>
            <strong>Test:</strong> ', ifelse(!is.null(var_data$levene_test), var_data$levene_test$test, "Levene"), '<br>
            <strong>Result:</strong> ', ifelse(!is.null(var_data$recommendation), var_data$recommendation, "Not available"), '
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
  
  # Cohen's D Effect Size Analysis
  if (!is.null(results$effect_sizes)) {
    content <- paste0(content, '
        <h3>Effect Size Analysis (Cohen\'s D)</h3>
        <p>Cohen\'s D measures the standardized difference between group means:</p>')
    
    for (var_name in names(results$effect_sizes)) {
      effect_data <- results$effect_sizes[[var_name]]
      
      if (!is.null(effect_data$effect_sizes) && length(effect_data$effect_sizes) > 0) {
        content <- paste0(content, '
        <h4>', var_name, '</h4>
        <div class="test-result">
            <div class="row">')
        
        for (comparison_name in names(effect_data$effect_sizes)) {
          comparison <- effect_data$effect_sizes[[comparison_name]]
          
          # Determine effect size class for styling
          effect_class <- "not-significant"
          if (!is.na(comparison$cohens_d)) {
            if (abs(comparison$cohens_d) >= 0.8) effect_class <- "significant"
            else if (abs(comparison$cohens_d) >= 0.5) effect_class <- "warning"
          }
          
          content <- paste0(content, '
                <div class="col-md-6">
                    <div class="', effect_class, '" style="margin: 5px; padding: 10px; border-radius: 5px;">
                        <strong>', comparison_name, ':</strong><br>
                        <strong>Cohen\'s D:</strong> ', 
                        ifelse(is.na(comparison$cohens_d), "Not calculated", round(comparison$cohens_d, 3)), '<br>
                        <strong>Effect Size:</strong> ', comparison$magnitude, '<br>
                        <strong>Sample Sizes:</strong> n₁=', comparison$n1, ', n₂=', comparison$n2, '
                    </div>
                </div>')
        }
        
        content <- paste0(content, '
            </div>
        </div>')
      }
    }
    
    content <- paste0(content, '
        <div class="interpretation">
            <strong>Cohen\'s D Interpretation:</strong><br>
            • |d| < 0.2: Negligible effect<br>
            • 0.2 ≤ |d| < 0.5: Small effect<br>
            • 0.5 ≤ |d| < 0.8: Medium effect<br>
            • |d| ≥ 0.8: Large effect
        </div>')
  }
  
  # Linear Regression Analysis
  if (!is.null(results$regression_analysis)) {
    content <- paste0(content, '
        <h3>Linear Regression Analysis</h3>
        <p>Linear models examining the relationship between group membership and continuous variables:</p>')
    
    for (var_name in names(results$regression_analysis)) {
      reg_data <- results$regression_analysis[[var_name]]
      
      if (!is.null(reg_data$model)) {
        significance_class <- if (!is.na(reg_data$f_p_value) && reg_data$f_p_value < 0.05) "significant" else "not-significant"
        
        content <- paste0(content, '
        <div class="test-result ', significance_class, '">
            <h4>', var_name, ' ~ ', results$metadata$group_column, '</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>R-squared:</strong> ', round(reg_data$r_squared, 4), '<br>
                    <strong>Adjusted R-squared:</strong> ', round(reg_data$adj_r_squared, 4), '<br>
                    <strong>F-statistic:</strong> ', round(reg_data$f_statistic, 3), '<br>
                    <strong>p-value:</strong> ', format.pval(reg_data$f_p_value, digits = 4), '
                </div>
                <div class="col-md-6">
                    <strong>Model Interpretation:</strong> ', reg_data$interpretation, '<br>
                    <strong>Residuals Normality:</strong> ', 
                    ifelse(reg_data$normality_test$p.value > 0.05, 
                           paste("Normal (p =", round(reg_data$normality_test$p.value, 4), ")"),
                           paste("Non-normal (p =", round(reg_data$normality_test$p.value, 4), ")")), '
                </div>
            </div>')
        
        # Add coefficients table if available
        if (!is.null(reg_data$coefficients)) {
          content <- paste0(content, '
            <div class="mt-2">
                <strong>Model Coefficients:</strong>
                <table class="table table-sm table-striped">
                    <thead>
                        <tr><th>Term</th><th>Estimate</th><th>Std.Error</th><th>t-value</th><th>p-value</th><th>95% CI</th></tr>
                    </thead>
                    <tbody>')
          
          for (i in 1:nrow(reg_data$coefficients)) {
            coef_row <- reg_data$coefficients[i, ]
            content <- paste0(content, '
                        <tr>
                            <td>', coef_row$term, '</td>
                            <td>', round(coef_row$estimate, 4), '</td>
                            <td>', round(coef_row$std.error, 4), '</td>
                            <td>', round(coef_row$statistic, 3), '</td>
                            <td>', format.pval(coef_row$p.value, digits = 4), '</td>
                            <td>[', round(coef_row$conf.low, 3), ', ', round(coef_row$conf.high, 3), ']</td>
                        </tr>')
          }
          
          content <- paste0(content, '
                    </tbody>
                </table>
            </div>')
        }
        
        content <- paste0(content, '</div>')
      } else {
        content <- paste0(content, '
        <div class="test-result not-significant">
            <h4>', var_name, '</h4>
            <p>', reg_data$interpretation, '</p>
        </div>')
      }
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
  
  # Variable Properties Analysis - Comprehensive table for each group
  if (!is.null(results$variable_properties)) {
    content <- paste0(content, '
        <h3>Variable Properties Analysis</h3>
        <p>Comprehensive analysis of variable characteristics for each group - essential for statistical test selection:</p>')
    
    if (!is.null(results$variable_properties$properties_table)) {
      content <- paste0(content, '
        <div class="table-responsive">
            ', create_variable_properties_table_html(results$variable_properties$properties_table), '
        </div>')
      
      # Add interpretation guide
      content <- paste0(content, '
        <div class="interpretation">
            <h4>Interpretation Guide:</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Normal Distribution:</strong><br>
                    • <span style="color: green;">Normal</span>: Data follows normal distribution (parametric tests suitable)<br>
                    • <span style="color: red;">Non-normal</span>: Data deviates from normality (consider non-parametric tests)<br><br>
                    <strong>Homogeneity Status:</strong><br>
                    • <span style="color: green;">Homogeneous</span>: Equal variances across groups<br>
                    • <span style="color: red;">Heterogeneous</span>: Unequal variances (use Welch corrections)<br><br>
                    <strong>Data Quality:</strong><br>
                    • <span style="color: green;">Good</span>: No major issues detected<br>
                    • <span style="color: orange;">Fair</span>: Minor issues present<br>
                    • <span style="color: red;">Poor</span>: Multiple quality issues
                </div>
                <div class="col-md-6">
                    <strong>CV% (Coefficient of Variation):</strong><br>
                    • <15%: Low variability<br>
                    • 15-35%: Moderate variability<br>
                    • >35%: High variability<br><br>
                    <strong>Outliers:</strong><br>
                    • <5%: Acceptable level<br>
                    • 5-10%: Moderate concern<br>
                    • >10%: High concern<br><br>
                    <strong>Recommended Tests:</strong><br>
                    Based on normality, homogeneity, and data quality assessment
                </div>
            </div>
        </div>')
    }
    
    # Homogeneity test summary
    if (!is.null(results$variable_properties$homogeneity_p_values)) {
      content <- paste0(content, '
        <h4>Homogeneity Test Summary (Levene\'s Test)</h4>
        <div class="table-responsive">
            <table class="table table-striped stats-table">
                <thead>
                    <tr>
                        <th>Variable</th>
                        <th>Levene\'s Test p-value</th>
                        <th>Homogeneity Status</th>
                        <th>Interpretation</th>
                    </tr>
                </thead>
                <tbody>')
      
      # Use properties table instead of homogeneity_p_values since the latter contains NULL values
      if (!is.null(results$variable_properties$properties_table) && 
          "Homogeneity_P" %in% names(results$variable_properties$properties_table)) {
        
        properties_table <- results$variable_properties$properties_table
        for (i in 1:nrow(properties_table)) {
          var_name <- properties_table$Variable[i]
          p_val <- properties_table$Homogeneity_P[i]
          homog_status <- if(is.na(p_val)) "Unknown" else if(p_val > 0.05) "Homogeneous" else "Heterogeneous"
          row_class <- if(is.na(p_val)) "" else if(p_val > 0.05) "table-success" else "table-warning"
          interpretation <- if(is.na(p_val)) {
            "Insufficient data for testing"
          } else if(p_val > 0.05) {
            "Variances are equal across groups"
          } else {
            "Variances differ significantly between groups"
          }
          
          content <- paste0(content, '
                      <tr class="', row_class, '">
                          <td><strong>', var_name, '</strong></td>
                          <td>', ifelse(is.na(p_val), "Not tested", format.pval(p_val, digits = 4)), '</td>
                          <td>', homog_status, '</td>
                          <td>', interpretation, '</td>
                      </tr>')
        }
      }
      
      content <- paste0(content, '
                </tbody>
            </table>
        </div>')
    }
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
  
  # Summary section
  content <- paste0(content, '
    <div class="result-section">
        <h2>Correlation Analysis Summary</h2>
        <p>This analysis examines the strength and direction of linear relationships between continuous variables, 
           using Pearson or Spearman correlation coefficients based on data characteristics.</p>')
  
  # Metadata overview
  if (!is.null(results$metadata)) {
    content <- paste0(content, '
        <h3>Analysis Overview</h3>
        <div class="interpretation">
            <strong>Total Variables:</strong> ', results$metadata$total_variables, '<br>
            <strong>Total Observations:</strong> ', results$metadata$total_observations, '<br>')
    
    if (!is.null(results$metadata$group_column)) {
      content <- paste0(content, 
                       '<strong>Group Column:</strong> ', results$metadata$group_column, '<br>
                        <strong>Groups:</strong> ', paste(results$metadata$groups, collapse = ", "), '<br>')
    }
    
    content <- paste0(content, '</div>')
  }
  
  # Overall Correlation Results
  if (!is.null(results$overall_correlations) && !is.null(results$overall_correlations$pearson_matrix)) {
    content <- paste0(content, '
        <h3>Overall Correlation Matrix</h3>
        <p>Correlation coefficients between all pairs of variables:</p>')
    
    # Create correlation matrix table
    cor_matrix <- results$overall_correlations$pearson_matrix
    p_matrix <- results$overall_correlations$pearson_p_values
    
    content <- paste0(content, '<div class="table-responsive">
        <table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Variable 1</th>
                    <th>Variable 2</th>
                    <th>Pearson r</th>
                    <th>p-value</th>
                    <th>Strength</th>
                    <th>Significance</th>
                </tr>
            </thead>
            <tbody>')
    
    # Fill correlation table
    variables <- rownames(cor_matrix)
    for (i in 1:(length(variables)-1)) {
      for (j in (i+1):length(variables)) {
        var1 <- variables[i]
        var2 <- variables[j]
        r_val <- cor_matrix[i, j]
        p_val <- p_matrix[i, j]
        
        # Determine strength and significance
        abs_r <- abs(r_val)
        if (abs_r < 0.1) strength <- "negligible"
        else if (abs_r < 0.3) strength <- "weak"
        else if (abs_r < 0.5) strength <- "moderate"
        else if (abs_r < 0.7) strength <- "strong"
        else strength <- "very strong"
        
        significance <- ifelse(p_val < 0.05, "significant", "not significant")
        row_class <- ifelse(p_val < 0.05, "table-success", "table-light")
        
        content <- paste0(content, '
                <tr class="', row_class, '">
                    <td>', var1, '</td>
                    <td>', var2, '</td>
                    <td>', round(r_val, 3), '</td>
                    <td>', format.pval(p_val, digits = 4), '</td>
                    <td>', strength, '</td>
                    <td>', significance, '</td>
                </tr>')
      }
    }
    
    content <- paste0(content, '</tbody></table></div>')
  }
  
  # Significant Correlations Summary
  if (!is.null(results$correlation_summary$significant_correlations)) {
    sig_cors <- results$correlation_summary$significant_correlations
    
    if (nrow(sig_cors) > 0) {
      content <- paste0(content, '
          <h3>Significant Correlations Summary</h3>
          <p>Variables with statistically significant correlations (p < 0.05):</p>')
      
      for (i in 1:nrow(sig_cors)) {
        cor_row <- sig_cors[i, ]
        
        content <- paste0(content, '
          <div class="test-result significant">
              <strong>', cor_row$variable1, ' ↔ ', cor_row$variable2, '</strong><br>
              <strong>Correlation:</strong> r = ', round(cor_row$pearson_r, 3), 
              ' (', cor_row$strength, ' ', cor_row$direction, ')<br>
              <strong>p-value:</strong> ', format.pval(cor_row$pearson_p, digits = 4), '
          </div>')
      }
    } else {
      content <- paste0(content, '
          <h3>Significant Correlations</h3>
          <div class="interpretation">
              <strong>No significant correlations found</strong> at α = 0.05 level.
          </div>')
    }
  }
  
  # Include plots if requested
  if (include_plots && !is.null(results$plot_files)) {
    content <- paste0(content, '
        <h3>Correlation Visualizations</h3>
        <p>Visual representations of correlation patterns:</p>')
    
    for (plot_name in names(results$plot_files)) {
      plot_file <- results$plot_files[[plot_name]]
      relative_plot_path <- file.path("..", "plots", "correlation_analysis", basename(plot_file))
      
      content <- paste0(content, '
        <div class="plot-container">
            <h4>', stringr::str_to_title(gsub("_", " ", plot_name)), '</h4>
            <img src="', relative_plot_path, '" alt="', plot_name, '" class="img-fluid" style="max-width: 100%; height: auto;">
        </div>')
    }
  }
  
  # Interpretation guidelines
  content <- paste0(content, '
      <h3>Interpretation Guidelines</h3>
      <div class="interpretation">
          <h4>Correlation Strength Classification:</h4>
          <strong>Negligible:</strong> |r| < 0.1 - Very weak or no linear relationship<br>
          <strong>Weak:</strong> 0.1 ≤ |r| < 0.3 - Weak linear relationship<br>
          <strong>Moderate:</strong> 0.3 ≤ |r| < 0.5 - Moderate linear relationship<br>
          <strong>Strong:</strong> 0.5 ≤ |r| < 0.7 - Strong linear relationship<br>
          <strong>Very Strong:</strong> |r| ≥ 0.7 - Very strong linear relationship<br><br>
          
          <h4>Important Notes:</h4>
          • Correlation does not imply causation<br>
          • Pearson correlation measures linear relationships only<br>
          • Statistical significance (p < 0.05) indicates the correlation is unlikely due to chance<br>
          • Effect size (correlation strength) is often more important than statistical significance
      </div>')
  
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
  
  # Variable Properties Analysis - Comprehensive table for each group
  if (!is.null(results$variable_properties)) {
    content <- paste0(content, '
        <h3>Variable Properties Analysis</h3>
        <p>Comprehensive analysis of variable characteristics for each group - essential for statistical test selection:</p>')
    
    if (!is.null(results$variable_properties$properties_table)) {
      content <- paste0(content, '
        <div class="table-responsive">
            ', create_variable_properties_table_html(results$variable_properties$properties_table), '
        </div>')
      
      # Add interpretation guide
      content <- paste0(content, '
        <div class="interpretation">
            <h4>Interpretation Guide:</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Normal Distribution:</strong><br>
                    • <span style="color: green;">Normal</span>: Data follows normal distribution (parametric tests suitable)<br>
                    • <span style="color: red;">Non-normal</span>: Data deviates from normality (consider non-parametric tests)<br><br>
                    <strong>Homogeneity Status:</strong><br>
                    • <span style="color: green;">Homogeneous</span>: Equal variances across groups<br>
                    • <span style="color: red;">Heterogeneous</span>: Unequal variances (use Welch corrections)<br><br>
                    <strong>Data Quality:</strong><br>
                    • <span style="color: green;">Good</span>: No major issues detected<br>
                    • <span style="color: orange;">Fair</span>: Minor issues present<br>
                    • <span style="color: red;">Poor</span>: Multiple quality issues
                </div>
                <div class="col-md-6">
                    <strong>CV% (Coefficient of Variation):</strong><br>
                    • <15%: Low variability<br>
                    • 15-35%: Moderate variability<br>
                    • >35%: High variability<br><br>
                    <strong>Outliers:</strong><br>
                    • <5%: Acceptable level<br>
                    • 5-10%: Moderate concern<br>
                    • >10%: High concern<br><br>
                    <strong>Recommended Tests:</strong><br>
                    Based on normality, homogeneity, and data quality assessment
                </div>
            </div>
        </div>')
    }
    
    # Homogeneity test summary
    if (!is.null(results$variable_properties$homogeneity_p_values)) {
      content <- paste0(content, '
        <h4>Homogeneity Test Summary (Levene\'s Test)</h4>
        <div class="table-responsive">
            <table class="table table-striped stats-table">
                <thead>
                    <tr>
                        <th>Variable</th>
                        <th>Levene\'s Test p-value</th>
                        <th>Homogeneity Status</th>
                        <th>Interpretation</th>
                    </tr>
                </thead>
                <tbody>')
      
      # Use properties table instead of homogeneity_p_values since the latter contains NULL values
      if (!is.null(results$variable_properties$properties_table) && 
          "Homogeneity_P" %in% names(results$variable_properties$properties_table)) {
        
        properties_table <- results$variable_properties$properties_table
        for (i in 1:nrow(properties_table)) {
          var_name <- properties_table$Variable[i]
          p_val <- properties_table$Homogeneity_P[i]
          homog_status <- if(is.na(p_val)) "Unknown" else if(p_val > 0.05) "Homogeneous" else "Heterogeneous"
          row_class <- if(is.na(p_val)) "" else if(p_val > 0.05) "table-success" else "table-warning"
          interpretation <- if(is.na(p_val)) {
            "Insufficient data for testing"
          } else if(p_val > 0.05) {
            "Variances are equal across groups"
          } else {
            "Variances differ significantly between groups"
          }
          
          content <- paste0(content, '
                      <tr class="', row_class, '">
                          <td><strong>', var_name, '</strong></td>
                          <td>', ifelse(is.na(p_val), "Not tested", format.pval(p_val, digits = 4)), '</td>
                          <td>', homog_status, '</td>
                          <td>', interpretation, '</td>
                      </tr>')
        }
      }
      
      content <- paste0(content, '
                </tbody>
            </table>
        </div>')
    }
  }
  
  # Correlation Analysis Results
  if (!is.null(results$correlation_analysis)) {
    content <- paste0(content, '
        <h3>Correlation Analysis</h3>
        <p>Comprehensive correlation analysis between numeric variables:</p>')
    
    # Correlation matrix heatmap description
    if (!is.null(results$correlation_analysis$correlation_matrix)) {
      content <- paste0(content, '
        <h4>Correlation Matrix</h4>
        <div class="table-responsive">
            ', create_correlation_table_html(results$correlation_analysis$correlation_matrix), '
        </div>')
    }
    
    # Significant correlations
    if (!is.null(results$correlation_analysis$correlation_tests)) {
      content <- paste0(content, '
        <h4>Significant Correlations</h4>')
      
      significant_cors <- list()
      for (test_name in names(results$correlation_analysis$correlation_tests)) {
        test <- results$correlation_analysis$correlation_tests[[test_name]]
        if (!is.na(test$pearson_p) && test$pearson_p < 0.05) {
          significant_cors[[test_name]] <- test
        }
      }
      
      if (length(significant_cors) > 0) {
        for (test_name in names(significant_cors)) {
          test <- significant_cors[[test_name]]
          significance_class <- if (abs(test$pearson_r) >= 0.5) "significant" else "warning"
          
          content <- paste0(content, '
        <div class="test-result ', significance_class, '">
            <h5>', test_name, '</h5>
            <div class="row">
                <div class="col-md-6">
                    <strong>Pearson r:</strong> ', round(test$pearson_r, 3), '<br>
                    <strong>p-value:</strong> ', format.pval(test$pearson_p, digits = 4), '<br>
                    <strong>95% CI:</strong> [', round(test$pearson_ci[1], 3), ', ', round(test$pearson_ci[2], 3), ']
                </div>
                <div class="col-md-6">
                    <strong>Spearman ρ:</strong> ', round(test$spearman_rho, 3), '<br>
                    <strong>Spearman p:</strong> ', format.pval(test$spearman_p, digits = 4), '<br>
                    <strong>Strength:</strong> ', test$interpretation, '
                </div>
            </div>
        </div>')
        }
      } else {
        content <- paste0(content, '
        <div class="interpretation">
            <p>No statistically significant correlations found (p < 0.05).</p>
        </div>')
      }
    }
  }
  
  # Normality Testing Results
  if (!is.null(results$normality_analysis)) {
    content <- paste0(content, '
        <h3>Normality Testing</h3>
        <p>Assessment of normal distribution for each numeric variable:</p>')
    
    for (var_name in names(results$normality_analysis)) {
      var_data <- results$normality_analysis[[var_name]]
      # Check if normal field exists and is logical, otherwise default to FALSE
      normal_result <- if (!is.null(var_data$normal) && is.logical(var_data$normal) && !is.na(var_data$normal)) {
        var_data$normal
      } else {
        FALSE
      }
      normality_class <- if (normal_result) "significant" else "not-significant"
      
      content <- paste0(content, '
        <div class="test-result ', normality_class, '">
            <h4>', var_name, ' - ', var_data$test, '</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>Test Statistic:</strong> ', 
                    ifelse(is.null(var_data$statistic), "Not available", round(var_data$statistic, 4)), '<br>
                    <strong>p-value:</strong> ', 
                    ifelse(is.null(var_data$p_value), "Not available", format.pval(var_data$p_value, digits = 4)), '<br>
                    <strong>Normal Distribution:</strong> ', 
                    ifelse(normal_result, "Yes", "No"), '
                </div>
                <div class="col-md-6">
                    <strong>Skewness:</strong> ', 
                    ifelse(is.null(var_data$skewness), "Not calculated", round(var_data$skewness, 3)), '<br>
                    <strong>Kurtosis:</strong> ', 
                    ifelse(is.null(var_data$kurtosis), "Not calculated", round(var_data$kurtosis, 3)), '<br>
                    <strong>Interpretation:</strong> ', var_data$interpretation, '
                </div>
            </div>')
      
      # Group-wise normality if available
      if (!is.null(var_data$group_normality)) {
        content <- paste0(content, '
            <div class="mt-2">
                <strong>Group-wise Normality:</strong><br>')
        for (group_name in names(var_data$group_normality)) {
          group_result <- var_data$group_normality[[group_name]]
          content <- paste0(content, '
                &nbsp;&nbsp;• ', group_name, ': ', 
                ifelse(group_result$normal, "Normal", "Non-normal"), 
                ' (', group_result$test, ' p = ', round(group_result$p_value, 4), ')<br>')
        }
        content <- paste0(content, '
            </div>')
      }
      
      content <- paste0(content, '</div>')
    }
    
    content <- paste0(content, '
        <div class="interpretation">
            <strong>Normality Interpretation:</strong><br>
            • p > 0.05: Data appears normally distributed<br>
            • p ≤ 0.05: Data deviates from normal distribution<br>
            • Skewness close to 0: Symmetric distribution<br>
            • Kurtosis close to 0: Normal tail behavior
        </div>')
  }
  
  # Outlier Analysis Results
  if (!is.null(results$outlier_analysis)) {
    content <- paste0(content, '
        <h3>Outlier Analysis</h3>
        <p>Detection of outliers using multiple methods:</p>')
    
    for (var_name in names(results$outlier_analysis)) {
      var_data <- results$outlier_analysis[[var_name]]
      
      if (!is.null(var_data$iqr_outliers)) {
        outlier_class <- if (var_data$outlier_percentage > 10) "warning" else if (var_data$outlier_percentage > 5) "not-significant" else "significant"
        
        content <- paste0(content, '
        <div class="test-result ', outlier_class, '">
            <h4>', var_name, ' - Outlier Detection</h4>
            <div class="row">
                <div class="col-md-6">
                    <strong>IQR Method:</strong> ', length(var_data$iqr_outliers), ' outliers<br>
                    <strong>Percentage:</strong> ', var_data$outlier_percentage, '%<br>
                    <strong>IQR Bounds:</strong> [', round(var_data$iqr_bounds[1], 3), ', ', round(var_data$iqr_bounds[2], 3), ']
                </div>
                <div class="col-md-6">
                    <strong>Z-score Method:</strong> ', length(var_data$z_outliers), ' outliers<br>
                    <strong>Modified Z-score:</strong> ', length(var_data$modified_z_outliers), ' outliers<br>
                    <strong>Interpretation:</strong> ', var_data$interpretation, '
                </div>
            </div>')
        
        # Group-wise outliers if available
        if (!is.null(var_data$group_outliers)) {
          content <- paste0(content, '
            <div class="mt-2">
                <strong>Group-wise Outliers:</strong><br>')
          for (group_name in names(var_data$group_outliers)) {
            group_result <- var_data$group_outliers[[group_name]]
            content <- paste0(content, '
                &nbsp;&nbsp;• ', group_name, ': ', group_result$outlier_count, 
                ' outliers (', group_result$outlier_percentage, '%)<br>')
          }
          content <- paste0(content, '
            </div>')
        }
        
        content <- paste0(content, '</div>')
      }
    }
    
    content <- paste0(content, '
        <div class="interpretation">
            <strong>Outlier Detection Methods:</strong><br>
            • <strong>IQR Method:</strong> Values beyond Q1 - 1.5×IQR or Q3 + 1.5×IQR<br>
            • <strong>Z-score Method:</strong> |Z| > 3 (assuming normal distribution)<br>
            • <strong>Modified Z-score:</strong> Uses median and MAD, more robust to outliers<br>
            • <strong>Interpretation:</strong> >10% outliers may indicate data quality issues
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
  
  # Laboratory Guide - Decision Menu
  content <- paste0(content, '
    <div class="interpretation" style="margin-top: 20px;">
        <h4>Laboratory Guide - Statistical Test Decision Menu:</h4>
        <div class="table-responsive">
            <table class="table table-sm table-bordered">
                <thead class="table-dark">
                    <tr>
                        <th>Situation</th>
                        <th>Main Test</th>
                        <th>Post-hoc/Follow-up</th>
                        <th>When to Use</th>
                    </tr>
                </thead>
                <tbody>
                    <tr class="table-success">
                        <td><strong>>2 groups</strong><br>• Normal + Homogeneous</td>
                        <td><strong>One-way ANOVA</strong></td>
                        <td>• Tukey HSD (if p < 0.05)<br>• Shapiro-Wilk/KS normality check</td>
                        <td>Always run Tukey if ANOVA significant</td>
                    </tr>
                    <tr class="table-warning">
                        <td><strong>>2 groups</strong><br>• Non-normal OR unequal variances</td>
                        <td><strong>Kruskal-Wallis</strong></td>
                        <td>• Dunn test (Bonferroni/Holm)<br>• Try log/sqrt transformation</td>
                        <td>Identifies specific group differences</td>
                    </tr>
                    <tr class="table-info">
                        <td><strong>Exactly 2 groups</strong><br>• Normal + Homogeneous</td>
                        <td><strong>Student\'s t-test</strong></td>
                        <td>• Cohen\'s d effect size</td>
                        <td>Lighter alternative for 2 groups</td>
                    </tr>
                    <tr class="table-info">
                        <td><strong>2 groups</strong><br>• Normal + Unequal variances</td>
                        <td><strong>Welch\'s t-test</strong></td>
                        <td>• Cohen\'s d effect size</td>
                        <td>Handles heteroscedasticity</td>
                    </tr>
                    <tr class="table-info">
                        <td><strong>2 groups</strong><br>• Non-normal</td>
                        <td><strong>Mann-Whitney U</strong></td>
                        <td>• Rank-biserial correlation</td>
                        <td>Non-parametric alternative</td>
                    </tr>
                    <tr class="table-secondary">
                        <td><strong>Categorical variables</strong></td>
                        <td><strong>χ² test</strong></td>
                        <td>• Fisher\'s exact (low frequencies)</td>
                        <td>For nominal data analysis</td>
                    </tr>
                    <tr class="table-light">
                        <td><strong>Relationships</strong></td>
                        <td><strong>Pearson/Spearman</strong></td>
                        <td>• Pearson (both normal)<br>• Spearman (≥1 non-normal)</td>
                        <td>Exploring correlations</td>
                    </tr>
                </tbody>
            </table>
        </div>
        
        <h5>Key Implementation Notes:</h5>
        <div class="row">
            <div class="col-md-6">
                <strong>🔍 Borderline Cases (p ≈ 0.04-0.06):</strong><br>
                • Run both parametric and non-parametric tests<br>
                • Report both results if conclusions differ<br>
                • Document assumption violations<br><br>
                <strong>📊 Effect Sizes (not just p-values):</strong><br>
                • t-tests: Cohen\'s d<br>
                • ANOVA: η² (eta-squared)<br>
                • Mann-Whitney: rank-biserial r<br>
                • Makes practical relevance clearer
            </div>
            <div class="col-md-6">
                <strong>🔄 Data Transformations:</strong><br>
                • Log transformation (right-skewed data)<br>
                • Square root (count data)<br>
                • Re-test normality after transformation<br>
                • Use parametric tests if transformation succeeds<br><br>
                <strong>⚠️ Assumption Checks:</strong><br>
                • Always check before main test<br>
                • Shapiro-Wilk/KS (normality)<br>
                • Levene/Fisher (homogeneity)<br>
                • Document all assumption violations
            </div>
        </div>
    </div>')
  
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

# Create content for enhanced inferential analysis
create_enhanced_inferential_content <- function(results, include_plots) {
  content <- '<div class="row"><div class="col-12">'
  
  # Summary section
  content <- paste0(content, '
    <div class="result-section">
        <h2>Enhanced Inferential Analysis Summary</h2>
        <p>Advanced statistical modeling with multiple regression, ANCOVA, and interaction analysis.</p>')
  
  # Metadata overview
  if (!is.null(results$metadata)) {
    content <- paste0(content, '
        <h3>Analysis Overview</h3>
        <div class="interpretation">
            <strong>Groups Analyzed:</strong> ', paste(results$metadata$groups, collapse = ", "), '<br>
            <strong>Group Sizes:</strong> ', paste(names(results$metadata$group_sizes), "=", results$metadata$group_sizes, collapse = ", "), '<br>
            <strong>Dependent Variables:</strong> ', results$metadata$dependent_variables, '<br>
            <strong>Covariates:</strong> ', results$metadata$covariates, '<br>
            <strong>Total Observations:</strong> ', results$metadata$total_observations, '<br>')
    
    # Add covariate centering information
    if (!is.null(results$metadata$covariate_centering) && length(results$metadata$covariate_centering) > 0) {
      content <- paste0(content, '<strong>Covariate Centering:</strong> ')
      centering_info <- sapply(names(results$metadata$covariate_centering), function(cov) {
        mean_val <- round(results$metadata$covariate_centering[[cov]]$original_mean, 2)
        paste0(cov, " centered at ", mean_val)
      })
      content <- paste0(content, paste(centering_info, collapse = ", "))
    }
    
    content <- paste0(content, '
        </div>')
  }
  
  # Model Comparison Results
  if (!is.null(results$model_comparison)) {
    content <- paste0(content, '
        <h3>Model Selection Results</h3>
        <p>Comparison of different statistical models for each dependent variable:</p>')
    
    for (var_name in names(results$model_comparison)) {
      var_result <- results$model_comparison[[var_name]]
      if (!is.null(var_result$best_model)) {
        # Extract model comparison table
        comparison_table <- var_result$comparison_table
        best_model_name <- var_result$best_model$best_model_name
        
        content <- paste0(content, '
          <div class="test-result significant">
              <h4>', var_name, '</h4>
              <strong>Best Model:</strong> ', best_model_name, '<br>
              <strong>Selection Rationale:</strong> ', var_result$best_model$rationale, '<br>')
        
        # Add model metrics for best model
        if (!is.null(comparison_table)) {
          best_row <- comparison_table[comparison_table$Model == best_model_name, ]
          if (nrow(best_row) > 0) {
            content <- paste0(content, '
              <strong>Model Metrics:</strong> AIC = ', round(best_row$AIC, 2), 
              ', BIC = ', round(best_row$BIC, 2), 
              ', Adj R² = ', round(best_row$Adj_R_squared, 3))
          }
        }
        
        content <- paste0(content, '</div>')
      }
    }
  }
  
  # Interaction Analysis Results
  if (!is.null(results$interaction_analysis)) {
    content <- paste0(content, '
        <h3>Significant Group × Covariate Interactions</h3>
        <p>Testing for group × covariate interactions that significantly improve model fit:</p>')
    
    significant_interactions <- 0
    for (var_name in names(results$interaction_analysis)) {
      var_interactions <- results$interaction_analysis[[var_name]]
      for (covariate in names(var_interactions)) {
        interaction_data <- var_interactions[[covariate]]
        if (!is.null(interaction_data$interaction_significant) && interaction_data$interaction_significant) {
          significant_interactions <- significant_interactions + 1
          r_sq_change <- if (!is.null(interaction_data$model_improvement$r_squared_change)) {
            round(interaction_data$model_improvement$r_squared_change, 4)
          } else {
            "N/A"
          }
          
          # Extract interaction coefficient if available
          interaction_coeff_info <- ""
          if (!is.null(interaction_data$coefficients$estimates)) {
            coeff_table <- interaction_data$coefficients$estimates
            interaction_terms <- grep(":", rownames(coeff_table), value = TRUE)
            if (length(interaction_terms) > 0) {
              for (term in interaction_terms) {
                estimate <- round(coeff_table[term, "Estimate"], 4)
                se <- round(coeff_table[term, "Std. Error"], 4)
                interaction_coeff_info <- paste0(interaction_coeff_info, 
                  '<br><em>Interaction coefficient:</em> β = ', estimate, ' (SE = ', se, ')')
              }
            }
          }
          
          content <- paste0(content, '
            <div class="test-result significant">
                <strong>', var_name, ': Group × ', covariate, ' interaction</strong><br>
                <em>p-value:</em> ', format.pval(interaction_data$interaction_p_value, digits = 4), '<br>
                <em>R² change:</em> ', r_sq_change, interaction_coeff_info, '
            </div>')
        }
      }
    }
    
    if (significant_interactions == 0) {
      content <- paste0(content, '<p><em>No significant group × covariate interactions detected.</em></p>')
    }
  }
  
  # Effect Sizes Summary
  if (!is.null(results$effect_sizes)) {
    content <- paste0(content, '
        <h3>Effect Sizes Summary</h3>
        <p>Summary of effect sizes for significant models:</p>')
    
    for (var_name in names(results$effect_sizes)) {
      var_effects <- results$effect_sizes[[var_name]]
      if (!is.null(var_effects)) {
        content <- paste0(content, '
          <div class="interpretation">
              <strong>', var_name, ':</strong><br>')
        
        if (!is.null(var_effects$regression)) {
          r_squared <- round(var_effects$regression$r_squared, 3)
          interpretation <- var_effects$regression$interpretation
          
          # Enhanced interpretation based on variable type
          clinical_interpretation <- ""
          if (var_name %in% c("HGB", "HCT")) {
            clinical_interpretation <- " - indicating potential treatment effects on oxygen-carrying capacity"
          } else if (var_name %in% c("ERY", "PLT", "LEU", "MON")) {
            clinical_interpretation <- " - suggesting group differences in hematological parameters"
          } else if (var_name == "MCHC") {
            clinical_interpretation <- " - reflecting changes in red blood cell concentration"
          }
          
          content <- paste0(content, '
              <em>Regression R²:</em> ', r_squared, ' (', interpretation, ')<br>
              <em>Clinical relevance:</em> After adjusting for age and biomarkers, group membership explains ', 
              round(r_squared * 100, 1), '% of variance in ', var_name, clinical_interpretation, '<br>')
        }
        
        if (!is.null(var_effects$ancova)) {
          content <- paste0(content, '<em>ANCOVA effects available</em><br>')
        }
        
        content <- paste0(content, '</div>')
      }
    }
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

# Helper function to create variable properties table HTML
create_variable_properties_table_html <- function(properties_data) {
  if (is.null(properties_data) || nrow(properties_data) == 0) return("")
  
  # Check if this is the new simplified format from assumptions dashboard
  if ("Overall_Normal" %in% names(properties_data)) {
    return(create_simplified_properties_table_html(properties_data))
  }
  
  html <- '<table class="table table-striped stats-table" style="font-size: 0.85em;">
            <thead>
                <tr>
                    <th>Variable</th>
                    <th>Group</th>
                    <th>N</th>
                    <th>Missing</th>
                    <th>Mean</th>
                    <th>SD</th>
                    <th>Median</th>
                    <th>IQR</th>
                    <th>CV%</th>
                    <th>Skewness</th>
                    <th>Kurtosis</th>
                    <th>Normal Distribution</th>
                    <th>Normality p-value</th>
                    <th>Outliers</th>
                    <th>Outliers %</th>
                    <th>Homogeneity</th>
                    <th>Data Quality</th>
                    <th>Recommended Test</th>
                    <th>Post-hoc Needed</th>
                    <th>Alternative Tests</th>
                    <th>Effect Size</th>
                    <th>Borderline Cases</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(properties_data)) {
    row <- properties_data[i, ]
    
    # Color coding for normal distribution
    normal_class <- ""
    if (row$Normal_Distribution == "Normal") {
      normal_class <- "table-success"
    } else if (row$Normal_Distribution == "Non-normal") {
      normal_class <- "table-danger"
    }
    
    # Color coding for homogeneity
    homog_class <- ""
    if (row$Homogeneity_Status == "Homogeneous") {
      homog_class <- "table-success"
    } else if (row$Homogeneity_Status == "Heterogeneous") {
      homog_class <- "table-warning"
    }
    
    # Color coding for data quality
    quality_class <- ""
    if (row$Data_Quality == "Good") {
      quality_class <- "table-success"
    } else if (row$Data_Quality == "Fair") {
      quality_class <- "table-warning"
    } else if (row$Data_Quality == "Poor") {
      quality_class <- "table-danger"
    }
    
    # Color coding for outliers
    outlier_class <- ""
    if (!is.na(row$Outliers_Percent)) {
      if (row$Outliers_Percent <= 5) {
        outlier_class <- "table-success"
      } else if (row$Outliers_Percent <= 10) {
        outlier_class <- "table-warning"
      } else {
        outlier_class <- "table-danger"
      }
    }
    
    # Color coding for post-hoc needs
    posthoc_class <- ""
    if (grepl("Tukey|Dunn", row$Post_Hoc_Needed)) {
      posthoc_class <- "table-info"
    }
    
    # Color coding for borderline cases
    borderline_class <- ""
    if (grepl("Borderline|transformation", row$Borderline_Cases)) {
      borderline_class <- "table-warning"
    }
    
    html <- paste0(html, '
                <tr>
                    <td><strong>', row$Variable, '</strong></td>
                    <td>', row$Group, '</td>
                    <td>', ifelse(is.na(row$N), "-", row$N), '</td>
                    <td>', ifelse(is.na(row$Missing) || row$Missing == 0, "-", row$Missing), '</td>
                    <td>', ifelse(is.na(row$Mean), "-", row$Mean), '</td>
                    <td>', ifelse(is.na(row$SD), "-", row$SD), '</td>
                    <td>', ifelse(is.na(row$Median), "-", row$Median), '</td>
                    <td>', ifelse(is.na(row$IQR), "-", row$IQR), '</td>
                    <td>', ifelse(is.na(row$CV_Percent), "-", paste0(row$CV_Percent, "%")), '</td>
                    <td>', ifelse(is.na(row$Skewness), "-", row$Skewness), '</td>
                    <td>', ifelse(is.na(row$Kurtosis), "-", row$Kurtosis), '</td>
                    <td class="', normal_class, '"><strong>', row$Normal_Distribution, '</strong></td>
                    <td>', ifelse(is.na(row$Normality_P_Value), "-", format.pval(row$Normality_P_Value, digits = 4)), '</td>
                    <td class="', outlier_class, '">', ifelse(is.na(row$Outliers_Count), "-", row$Outliers_Count), '</td>
                    <td class="', outlier_class, '">', ifelse(is.na(row$Outliers_Percent), "-", paste0(row$Outliers_Percent, "%")), '</td>
                    <td class="', homog_class, '"><strong>', row$Homogeneity_Status, '</strong></td>
                    <td class="', quality_class, '"><strong>', row$Data_Quality, '</strong></td>
                    <td><em>', row$Recommended_Test, '</em></td>
                    <td class="', posthoc_class, '"><small>', row$Post_Hoc_Needed, '</small></td>
                    <td><small>', row$Alternative_Tests, '</small></td>
                    <td><small>', row$Effect_Size_Measure, '</small></td>
                    <td class="', borderline_class, '"><small>', row$Borderline_Cases, '</small></td>
                </tr>')
  }
  
  html <- paste0(html, '</tbody></table>')
  return(html)
}

# Helper function to create simplified variable properties table HTML (for new assumptions dashboard)
create_simplified_properties_table_html <- function(properties_data) {
  if (is.null(properties_data) || nrow(properties_data) == 0) return("")
  
  html <- '<table class="table table-striped stats-table">
            <thead>
                <tr>
                    <th>Variable</th>
                    <th>N Total</th>
                    <th>N Missing</th>
                    <th>Overall Normal</th>
                    <th>Normality p-value</th>
                    <th>Group Normality</th>
                    <th>Homogeneity</th>
                    <th>Homogeneity p-value</th>
                    <th>Skewness</th>
                    <th>Kurtosis</th>
                </tr>
            </thead>
            <tbody>'
  
  for (i in 1:nrow(properties_data)) {
    row <- properties_data[i, ]
    
    # Color coding for normality
    normal_class <- ""
    if (!is.na(row$Overall_Normal)) {
      if (row$Overall_Normal == "Yes") {
        normal_class <- "table-success"
      } else {
        normal_class <- "table-danger"
      }
    }
    
    # Color coding for homogeneity
    homog_class <- ""
    if (!is.na(row$Homogeneity)) {
      if (row$Homogeneity == "Yes") {
        homog_class <- "table-success"
      } else {
        homog_class <- "table-warning"
      }
    }
    
    html <- paste0(html, '
                <tr>
                    <td><strong>', row$Variable, '</strong></td>
                    <td>', ifelse(is.na(row$N_Total), "-", row$N_Total), '</td>
                    <td>', ifelse(is.na(row$N_Missing) || row$N_Missing == 0, "-", row$N_Missing), '</td>
                    <td class="', normal_class, '"><strong>', ifelse(is.na(row$Overall_Normal), "-", row$Overall_Normal), '</strong></td>
                    <td>', ifelse(is.na(row$Normality_P), "-", format.pval(row$Normality_P, digits = 4)), '</td>
                    <td>', ifelse(is.na(row$Group_Normality), "-", row$Group_Normality), '</td>
                    <td class="', homog_class, '"><strong>', ifelse(is.na(row$Homogeneity), "-", row$Homogeneity), '</strong></td>
                    <td>', ifelse(is.na(row$Homogeneity_P), "-", format.pval(row$Homogeneity_P, digits = 4)), '</td>
                    <td>', ifelse(is.na(row$Skewness), "-", round(row$Skewness, 3)), '</td>
                    <td>', ifelse(is.na(row$Kurtosis), "-", round(row$Kurtosis, 3)), '</td>
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