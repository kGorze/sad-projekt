 # Residual transformation and robust regression

#box-cox transformation





# Library dependencies
if (!require(MASS, quietly = TRUE)) {
  install.packages("MASS", repos = "https://cran.r-project.org")
  library(MASS)
}

if (!require(robustbase, quietly = TRUE)) {
  install.packages("robustbase", repos = "https://cran.r-project.org")
  library(robustbase)
}

# Function to diagnose and fix residual normality issues
fix_model_residual_issues <- function(data, variables, group_column = "grupa") {
  
  results <- list()
  
  for (var in variables) {
    cat("\n=== ANALYZING MODEL RESIDUALS FOR", var, "===\n")
    
    # Step 1: Fit original model and check residuals
    original_model_result <- fit_and_check_residuals(data, var, group_column, "original")
    results[[var]] <- list(original = original_model_result)
    
    # Step 2: If residuals are non-normal, try transformations
    if (!original_model_result$residuals_normal) {
      cat("⚠ Non-normal residuals detected for", var, "(p =", round(original_model_result$normality_p, 4), ")\n")
      cat("Trying data transformations...\n")
      
      # Try log transformation (if all values are positive)
      log_result <- NULL
      if (all(data[[var]][!is.na(data[[var]])] > 0, na.rm = TRUE)) {
        cat("- Testing log transformation...\n")
        log_result <- fit_and_check_residuals(data, var, group_column, "log")
        results[[var]]$log_transformed <- log_result
      }
      
      # Try square root transformation (if all values are non-negative)
      sqrt_result <- NULL
      if (all(data[[var]][!is.na(data[[var]])] >= 0, na.rm = TRUE)) {
        cat("- Testing square root transformation...\n")
        sqrt_result <- fit_and_check_residuals(data, var, group_column, "sqrt")
        results[[var]]$sqrt_transformed <- sqrt_result
      }
      
      # Try Box-Cox transformation
      cat("- Testing Box-Cox transformation...\n")
      boxcox_result <- fit_and_check_residuals(data, var, group_column, "boxcox")
      results[[var]]$boxcox_transformed <- boxcox_result
      
      # Select best transformation
      best_transformation <- select_best_transformation(original_model_result, log_result, sqrt_result, boxcox_result)
      results[[var]]$best_transformation <- best_transformation
      
      # Step 3: If transformations don't work, use robust regression
      if (!best_transformation$residuals_normal) {
        cat("⚠ Transformations failed. Fitting robust regression...\n")
        
        # Huber M-estimator
        huber_result <- fit_robust_regression(data, var, group_column, "huber")
        results[[var]]$robust_huber <- huber_result
        
        # MM-estimator (if robustbase is available)
        mm_result <- fit_robust_regression(data, var, group_column, "mm")
        results[[var]]$robust_mm <- mm_result
        
        # Select best robust method
        best_robust <- select_best_robust_method(huber_result, mm_result)
        results[[var]]$best_robust <- best_robust
        
        # Final recommendation
        results[[var]]$final_recommendation <- best_robust
      } else {
        results[[var]]$final_recommendation <- best_transformation
      }
    } else {
      cat("✓ Residuals are normally distributed for", var, "(p =", round(original_model_result$normality_p, 4), ")\n")
      results[[var]]$final_recommendation <- original_model_result
    }
  }
  
  return(results)
}

# Function to fit model and check residual normality
fit_and_check_residuals <- function(data, variable, group_column, transformation = "original") {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(
      error = "Insufficient data for analysis",
      residuals_normal = FALSE
    ))
  }
  
  # Apply transformation
  if (transformation == "log") {
    if (any(clean_data[[variable]] <= 0, na.rm = TRUE)) {
      return(list(
        error = "Cannot apply log transformation to non-positive values",
        residuals_normal = FALSE
      ))
    }
    clean_data$transformed_var <- log(clean_data[[variable]])
    transformation_formula <- "log"
  } else if (transformation == "sqrt") {
    if (any(clean_data[[variable]] < 0, na.rm = TRUE)) {
      return(list(
        error = "Cannot apply sqrt transformation to negative values",
        residuals_normal = FALSE
      ))
    }
    clean_data$transformed_var <- sqrt(clean_data[[variable]])
    transformation_formula <- "sqrt"
  } else if (transformation == "boxcox") {
    # Use centralized Box-Cox transformation
    source("modules/utils/statistical_helpers.R")
    
    # Apply Box-Cox transformation with group factor
    group_factor <- as.factor(clean_data[[group_column]])
    boxcox_result <- apply_boxcox_transformation(clean_data[[variable]], group_factor)
    
    if (!boxcox_result$success) {
      return(list(
        error = boxcox_result$error,
        residuals_normal = FALSE
      ))
    }
    
    # Use transformed data
    clean_data$transformed_var <- boxcox_result$transformed_data
    transformation_formula <- boxcox_result$transformation_name
  } else {
    # Original variable
    clean_data$transformed_var <- clean_data[[variable]]
    transformation_formula <- variable
  }
  
  # Fit linear model
  tryCatch({
    clean_data[[group_column]] <- as.factor(clean_data[[group_column]])
    lm_model <- lm(transformed_var ~ get(group_column), data = clean_data)
    
    # Extract residuals
    residuals <- residuals(lm_model)
    
    # Test residual normality
    normality_test <- shapiro.test(residuals)
    residuals_normal <- normality_test$p.value > 0.05
    
    # Additional diagnostics
    fitted_values <- fitted(lm_model)
    model_summary <- summary(lm_model)
    
    # Homoscedasticity test (Breusch-Pagan if available)
    bp_test <- NULL
    if (requireNamespace("car", quietly = TRUE)) {
      tryCatch({
        bp_test <- car::ncvTest(lm_model)
      }, error = function(e) {
        bp_test <- NULL
      })
    }
    
    return(list(
      model = lm_model,
      transformation = transformation,
      transformation_formula = transformation_formula,
      residuals = residuals,
      fitted_values = fitted_values,
      normality_test = normality_test,
      normality_p = normality_test$p.value,
      residuals_normal = residuals_normal,
      r_squared = model_summary$r.squared,
      adj_r_squared = model_summary$adj.r.squared,
      f_statistic = model_summary$fstatistic[1],
      f_p_value = pf(model_summary$fstatistic[1], 
                     model_summary$fstatistic[2], 
                     model_summary$fstatistic[3], 
                     lower.tail = FALSE),
      bp_test = bp_test,
      model_summary = model_summary
    ))
    
  }, error = function(e) {
    return(list(
      error = paste("Model fitting failed:", e$message),
      residuals_normal = FALSE
    ))
  })
}

# Function to select best transformation
select_best_transformation <- function(original, log_result, sqrt_result, boxcox_result) {
  
  candidates <- list()
  
  # Original model
  if (!is.null(original) && !is.null(original$normality_p)) {
    candidates$original <- list(
      result = original,
      score = calculate_transformation_score(original)
    )
  }
  
  # Log transformation
  if (!is.null(log_result) && !is.null(log_result$normality_p)) {
    candidates$log <- list(
      result = log_result,
      score = calculate_transformation_score(log_result)
    )
  }
  
  # Square root transformation
  if (!is.null(sqrt_result) && !is.null(sqrt_result$normality_p)) {
    candidates$sqrt <- list(
      result = sqrt_result,
      score = calculate_transformation_score(sqrt_result)
    )
  }
  
  # Box-Cox transformation
  if (!is.null(boxcox_result) && !is.null(boxcox_result$normality_p)) {
    candidates$boxcox <- list(
      result = boxcox_result,
      score = calculate_transformation_score(boxcox_result)
    )
  }
  
  if (length(candidates) == 0) {
    return(original)
  }
  
  # Select transformation with highest score
  scores <- sapply(candidates, function(x) x$score)
  best_name <- names(scores)[which.max(scores)]
  best_result <- candidates[[best_name]]$result
  
  cat("- Best transformation:", best_name, "(score =", round(max(scores), 3), ")\n")
  
  return(best_result)
}

# Function to calculate transformation score (higher = better)
calculate_transformation_score <- function(result) {
  
  if (is.null(result) || !is.null(result$error)) {
    return(-Inf)
  }
  
  # Components of the score
  normality_score <- if (result$residuals_normal) {
    # Higher p-value is better for normality
    min(result$normality_p * 10, 5)  # Cap at 5
  } else {
    # Penalty for non-normal residuals
    result$normality_p * 2
  }
  
  # R-squared component (model fit)
  r_squared_score <- if (!is.null(result$r_squared)) {
    result$r_squared * 2
  } else {
    0
  }
  
  # Homoscedasticity bonus
  homosced_score <- if (!is.null(result$bp_test) && !is.null(result$bp_test$p)) {
    ifelse(result$bp_test$p > 0.05, 1, 0)
  } else {
    0
  }
  
  total_score <- normality_score + r_squared_score + homosced_score
  
  return(total_score)
}

# Function to fit robust regression
fit_robust_regression <- function(data, variable, group_column, method = "huber") {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(
      error = "Insufficient data for robust regression",
      method = method
    ))
  }
  
  clean_data[[group_column]] <- as.factor(clean_data[[group_column]])
  
  tryCatch({
    if (method == "huber") {
      # Huber M-estimator
      robust_model <- rlm(as.formula(paste(variable, "~", group_column)), 
                         data = clean_data, method = "M")
      method_name <- "Huber M-estimator"
    } else if (method == "mm") {
      # MM-estimator (requires robustbase)
      if (requireNamespace("robustbase", quietly = TRUE)) {
        robust_model <- robustbase::lmrob(as.formula(paste(variable, "~", group_column)), 
                                         data = clean_data)
        method_name <- "MM-estimator"
      } else {
        # Fallback to Huber
        robust_model <- rlm(as.formula(paste(variable, "~", group_column)), 
                           data = clean_data, method = "M")
        method_name <- "Huber M-estimator (MM not available)"
      }
    }
    
    # Extract information
    residuals <- residuals(robust_model)
    fitted_values <- fitted(robust_model)
    
    # Test residual normality
    normality_test <- shapiro.test(residuals)
    residuals_normal <- normality_test$p.value > 0.05
    
    # Extract coefficients and significance
    model_summary <- summary(robust_model)
    
    return(list(
      model = robust_model,
      method = method,
      method_name = method_name,
      residuals = residuals,
      fitted_values = fitted_values,
      normality_test = normality_test,
      normality_p = normality_test$p.value,
      residuals_normal = residuals_normal,
      model_summary = model_summary,
      robust = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      error = paste("Robust regression failed:", e$message),
      method = method
    ))
  })
}

# Function to select best robust method
select_best_robust_method <- function(huber_result, mm_result) {
  
  candidates <- list()
  
  if (!is.null(huber_result) && is.null(huber_result$error)) {
    candidates$huber <- huber_result
  }
  
  if (!is.null(mm_result) && is.null(mm_result$error)) {
    candidates$mm <- mm_result
  }
  
  if (length(candidates) == 0) {
    return(huber_result)  # Return whatever we have
  }
  
  # Prefer method with better residual normality
  scores <- sapply(candidates, function(x) {
    if (x$residuals_normal) {
      return(x$normality_p + 1)  # Bonus for normal residuals
    } else {
      return(x$normality_p)
    }
  })
  
  best_name <- names(scores)[which.max(scores)]
  best_result <- candidates[[best_name]]
  
  cat("- Best robust method:", best_result$method_name, "\n")
  
  return(best_result)
}

# Function to generate comprehensive residual diagnostics report
generate_residual_diagnostics_report <- function(results, output_dir = "output/reports") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_file <- file.path(output_dir, paste0("residual_diagnostics_", timestamp, ".html"))
  
  # Start HTML content
  timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  html_content <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Residual Diagnostics and Transformation Report</title>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <style>
        body { font-family: "Arial", sans-serif; margin: 20px; }
        .result-section { margin-bottom: 30px; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }
        .test-result { background-color: #f8f9fa; padding: 15px; margin: 10px 0; border-radius: 5px; }
        .normal-residuals { border-left: 5px solid #28a745; }
        .non-normal-residuals { border-left: 5px solid #dc3545; }
        .improved { border-left: 5px solid #17a2b8; }
        .robust { border-left: 5px solid #6f42c1; }
        .timestamp { color: #6c757d; font-size: 0.8em; }
    </style>
</head>
<body>
    <div class="container-fluid">
        <div class="row">
            <div class="col-12">
                <h1 class="text-center mb-4">Residual Diagnostics and Transformation Report</h1>
                <p class="text-center timestamp">Generated on: ', timestamp_str, '</p>
                <div class="alert alert-info">
                    <strong>Task F: Fix Model Residual Issues</strong><br>
                    This report shows the results of testing and fixing models with non-normal residuals through:
                    <ul>
                        <li>Data transformations (log, square root, Box-Cox)</li>
                        <li>Robust regression methods (Huber M-estimator, MM-estimator)</li>
                    </ul>
                </div>
            </div>
        </div>')
  
  # Add results for each variable
  for (var_name in names(results)) {
    var_result <- results[[var_name]]
    
    html_content <- paste0(html_content, '
        <div class="result-section">
            <h2>', var_name, '</h2>')
    
    # Original model results
    if (!is.null(var_result$original)) {
      orig <- var_result$original
      residual_class <- if (orig$residuals_normal) "normal-residuals" else "non-normal-residuals"
      
      html_content <- paste0(html_content, '
            <div class="test-result ', residual_class, '">
                <h4>Original Model</h4>
                <strong>Residual Normality:</strong> ', 
                ifelse(orig$residuals_normal, 
                       paste0("Normal (p = ", round(orig$normality_p, 4), ")"),
                       paste0("Non-normal (p = ", round(orig$normality_p, 4), ")")), '<br>
                <strong>R²:</strong> ', round(orig$r_squared, 4), '<br>
                <strong>Model F-test:</strong> p = ', format.pval(orig$f_p_value, digits = 4), '
            </div>')
    }
    
    # Transformation results
    transformations <- c("log_transformed", "sqrt_transformed", "boxcox_transformed")
    transformation_names <- c("Log Transformation", "Square Root Transformation", "Box-Cox Transformation")
    
    for (i in seq_along(transformations)) {
      trans_name <- transformations[i]
      trans_label <- transformation_names[i]
      
      if (!is.null(var_result[[trans_name]])) {
        trans <- var_result[[trans_name]]
        
        if (is.null(trans$error)) {
          improvement_class <- if (trans$residuals_normal) "improved" else "test-result"
          
          html_content <- paste0(html_content, '
            <div class="test-result ', improvement_class, '">
                <h4>', trans_label, '</h4>
                <strong>Formula:</strong> ', trans$transformation_formula, '<br>
                <strong>Residual Normality:</strong> ', 
                ifelse(trans$residuals_normal, 
                       paste0("Normal (p = ", round(trans$normality_p, 4), ")"),
                       paste0("Non-normal (p = ", round(trans$normality_p, 4), ")")), '<br>
                <strong>R²:</strong> ', round(trans$r_squared, 4), '<br>
                <strong>Model F-test:</strong> p = ', format.pval(trans$f_p_value, digits = 4), '
            </div>')
        }
      }
    }
    
    # Robust regression results
    robust_methods <- c("robust_huber", "robust_mm")
    robust_names <- c("Huber M-estimator", "MM-estimator")
    
    for (i in seq_along(robust_methods)) {
      robust_name <- robust_methods[i]
      robust_label <- robust_names[i]
      
      if (!is.null(var_result[[robust_name]])) {
        robust <- var_result[[robust_name]]
        
        if (is.null(robust$error)) {
          html_content <- paste0(html_content, '
            <div class="test-result robust">
                <h4>', robust$method_name, '</h4>
                <strong>Residual Normality:</strong> ', 
                ifelse(robust$residuals_normal, 
                       paste0("Normal (p = ", round(robust$normality_p, 4), ")"),
                       paste0("Non-normal (p = ", round(robust$normality_p, 4), ")")), '<br>
                <strong>Method:</strong> Robust regression (resistant to outliers)
            </div>')
        }
      }
    }
    
    # Final recommendation
    if (!is.null(var_result$final_recommendation)) {
      final <- var_result$final_recommendation
      
      recommendation_text <- if (final$transformation == "original") {
        "Use original model (residuals are normal)"
      } else if (!is.null(final$robust) && final$robust) {
        paste0("Use ", final$method_name, " (robust regression)")
      } else {
        paste0("Use ", final$transformation, " transformation")
      }
      
      html_content <- paste0(html_content, '
            <div class="alert alert-success">
                <strong>Final Recommendation:</strong> ', recommendation_text, '
            </div>')
    }
    
    html_content <- paste0(html_content, '</div>')
  }
  
  # Close HTML
  html_content <- paste0(html_content, '
    </div>
    <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>')
  
  # Write to file
  writeLines(html_content, report_file)
  
  return(report_file)
}

# Function to apply residual fixes to comparative analysis
apply_residual_fixes_to_analysis <- function(data, group_column = "grupa") {
  
  # Get numeric variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  numeric_vars <- numeric_vars[numeric_vars != group_column]
  
  cat("=== TASK F: FIXING MODEL RESIDUAL ISSUES ===\n")
  cat("Testing and fixing models with non-normal residuals...\n")
  
  # Run residual diagnostics and fixes
  residual_results <- fix_model_residual_issues(data, numeric_vars, group_column)
  
  # Note: Residual diagnostics integrated into main comparative analysis report
  cat("\nResidual diagnostics integrated into main analysis report.\n")
  
  # Summary of findings
  cat("\n=== RESIDUAL DIAGNOSTICS SUMMARY ===\n")
  
  normal_count <- 0
  fixed_by_transformation <- 0
  fixed_by_robust <- 0
  still_problematic <- 0
  
  for (var_name in names(residual_results)) {
    var_result <- residual_results[[var_name]]
    
    if (!is.null(var_result$original) && var_result$original$residuals_normal) {
      cat("✓", var_name, ": Residuals already normal\n")
      normal_count <- normal_count + 1
    } else if (!is.null(var_result$final_recommendation)) {
      final <- var_result$final_recommendation
      
      if (final$transformation != "original" && !isTRUE(final$robust)) {
        cat("🔧", var_name, ": Fixed with", final$transformation, "transformation\n")
        fixed_by_transformation <- fixed_by_transformation + 1
      } else if (isTRUE(final$robust)) {
        cat("🛡", var_name, ": Fixed with robust regression (", final$method_name, ")\n")
        fixed_by_robust <- fixed_by_robust + 1
      } else {
        cat("⚠", var_name, ": Still has residual issues\n")
        still_problematic <- still_problematic + 1
      }
    }
  }
  
  cat("\nSummary:\n")
  cat("- Models with normal residuals:", normal_count, "\n")
  cat("- Fixed by transformation:", fixed_by_transformation, "\n")
  cat("- Fixed by robust regression:", fixed_by_robust, "\n")
  cat("- Still problematic:", still_problematic, "\n")
  
  return(list(
    results = residual_results,
    summary = list(
      normal_count = normal_count,
      fixed_by_transformation = fixed_by_transformation,
      fixed_by_robust = fixed_by_robust,
      still_problematic = still_problematic
    )
  ))
}