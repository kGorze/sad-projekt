# Data validation for statistical analysis readiness
# Ensures data meets requirements for comparative statistical tests

# Main data validation function for statistical analysis
validate_data_for_analysis <- function(data, group_column = "grupa", 
                                      required_columns = NULL, 
                                      min_group_size = 5) {
  
  cat("\n=== STATISTICAL ANALYSIS VALIDATION ===\n")
  
  validation_results <- list()
  validation_issues <- c()
  validation_warnings <- c()
  
  # 1. Basic structure validation
  structure_validation <- validate_basic_structure(data, group_column, required_columns)
  validation_results$structure <- structure_validation
  if (!structure_validation$valid) {
    validation_issues <- c(validation_issues, structure_validation$issues)
  }
  
  # 2. Group independence validation
  independence_validation <- validate_group_independence(data, group_column)
  validation_results$independence <- independence_validation
  if (!independence_validation$valid) {
    validation_issues <- c(validation_issues, independence_validation$issues)
  }
  
  # 3. Sample size validation
  sample_size_validation <- check_sample_sizes(data, group_column, min_group_size)
  validation_results$sample_sizes <- sample_size_validation
  if (!sample_size_validation$valid) {
    validation_issues <- c(validation_issues, sample_size_validation$issues)
  }
  
  # 4. Data type validation for statistical tests
  type_validation <- validate_data_types_for_stats(data)
  validation_results$data_types <- type_validation
  if (length(type_validation$warnings) > 0) {
    validation_warnings <- c(validation_warnings, type_validation$warnings)
  }
  
  # 5. Statistical assumptions pre-check
  assumptions_validation <- validate_statistical_assumptions(data, group_column)
  validation_results$assumptions <- assumptions_validation
  if (length(assumptions_validation$warnings) > 0) {
    validation_warnings <- c(validation_warnings, assumptions_validation$warnings)
  }
  
  # 6. Power analysis and adequacy
  power_validation <- validate_statistical_power(data, group_column)
  validation_results$power <- power_validation
  if (length(power_validation$warnings) > 0) {
    validation_warnings <- c(validation_warnings, power_validation$warnings)
  }
  
  # Generate final validation report
  validation_report <- generate_validation_report(validation_results, validation_issues, validation_warnings)
  
  return(list(
    valid = length(validation_issues) == 0,
    issues = validation_issues,
    warnings = validation_warnings,
    results = validation_results,
    report = validation_report,
    recommendations = generate_analysis_recommendations(validation_results)
  ))
}

# Validate basic data structure for analysis
validate_basic_structure <- function(data, group_column, required_columns) {
  
  cat("1. BASIC STRUCTURE VALIDATION\n")
  cat("-----------------------------\n")
  
  issues <- c()
  
  # Check if data exists and is not empty
  if (is.null(data) || nrow(data) == 0) {
    issues <- c(issues, "Dataset is empty or null")
    cat("✗ Dataset is empty or null\n")
    return(list(valid = FALSE, issues = issues))
  }
  
  # Check if group column exists
  if (!group_column %in% names(data)) {
    issues <- c(issues, sprintf("Group column '%s' not found", group_column))
    cat(sprintf("✗ Group column '%s' not found\n", group_column))
  } else {
    cat(sprintf("✓ Group column '%s' found\n", group_column))
  }
  
  # Check required columns
  if (!is.null(required_columns)) {
    missing_cols <- required_columns[!required_columns %in% names(data)]
    if (length(missing_cols) > 0) {
      issues <- c(issues, sprintf("Required columns missing: %s", paste(missing_cols, collapse = ", ")))
      cat(sprintf("✗ Missing required columns: %s\n", paste(missing_cols, collapse = ", ")))
    } else {
      cat("✓ All required columns present\n")
    }
  }
  
  # Check for minimum data dimensions
  if (nrow(data) < 6) {
    issues <- c(issues, sprintf("Insufficient data: only %d rows", nrow(data)))
    cat(sprintf("✗ Insufficient data: only %d rows (minimum 6 needed)\n", nrow(data)))
  } else {
    cat(sprintf("✓ Adequate data size: %d rows\n", nrow(data)))
  }
  
  if (ncol(data) < 3) {
    issues <- c(issues, sprintf("Insufficient variables: only %d columns", ncol(data)))
    cat(sprintf("✗ Insufficient variables: only %d columns (minimum 3 needed)\n", ncol(data)))
  } else {
    cat(sprintf("✓ Adequate variables: %d columns\n", ncol(data)))
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues,
    dimensions = c(nrow(data), ncol(data)),
    group_column_exists = group_column %in% names(data)
  ))
}

# Validate group independence (each observation belongs to exactly one group)
validate_group_independence <- function(data, group_column) {
  
  cat("\n2. GROUP INDEPENDENCE VALIDATION\n")
  cat("--------------------------------\n")
  
  issues <- c()
  
  if (!group_column %in% names(data)) {
    issues <- c(issues, "Cannot validate independence: group column missing")
    cat("✗ Cannot validate independence: group column missing\n")
    return(list(valid = FALSE, issues = issues))
  }
  
  # Check for missing group assignments
  missing_groups <- sum(is.na(data[[group_column]]))
  if (missing_groups > 0) {
    issues <- c(issues, sprintf("%d observations have missing group assignments", missing_groups))
    cat(sprintf("✗ %d observations have missing group assignments\n", missing_groups))
  } else {
    cat("✓ No missing group assignments\n")
  }
  
  # Check group factor levels
  if (is.factor(data[[group_column]])) {
    all_levels <- levels(data[[group_column]])
    groups <- unique(data[[group_column]][!is.na(data[[group_column]])])
    unused_levels <- setdiff(all_levels, groups)

    if (length(unused_levels) > 0) {
      cat(sprintf("WARNING: Unused factor levels detected: %s\n", paste(unused_levels, collapse = ", ")))
    }
  } else {
    groups <- unique(data[[group_column]][!is.na(data[[group_column]])])
  }
  
  # Check number of groups
  if (length(groups) < 2) {
    issues <- c(issues, sprintf("Insufficient groups for comparison: only %d group(s)", length(groups)))
    cat(sprintf("✗ Insufficient groups for comparison: only %d group(s)\n", length(groups)))
  } else {
    cat(sprintf("✓ Adequate number of groups: %d groups (%s)\n", 
                length(groups), paste(groups, collapse = ", ")))
  }
  
  # Check for duplicate/overlapping observations (if ID column exists)
  if ("ID" %in% names(data)) {
    duplicate_ids <- sum(duplicated(data$ID))
    if (duplicate_ids > 0) {
      issues <- c(issues, sprintf("%d duplicate observations detected", duplicate_ids))
      cat(sprintf("✗ %d duplicate observations detected\n", duplicate_ids))
    } else {
      cat("✓ No duplicate observations detected\n")
    }
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues,
    groups = groups,
    group_count = length(groups),
    missing_assignments = missing_groups
  ))
}

# Check minimum sample sizes per group
check_sample_sizes <- function(data, group_column, min_size = 5) {
  
  cat("\n3. SAMPLE SIZE VALIDATION\n")
  cat("-------------------------\n")
  
  issues <- c()
  
  if (!group_column %in% names(data)) {
    issues <- c(issues, "Cannot check sample sizes: group column missing")
    cat("✗ Cannot check sample sizes: group column missing\n")
    return(list(valid = FALSE, issues = issues))
  }
  
  # Calculate sample sizes per group
  group_sizes <- table(data[[group_column]], useNA = "no")
  
  cat("Sample sizes per group:\n")
  for (group in names(group_sizes)) {
    size <- group_sizes[group]
    status <- if (size >= min_size) "✓" else "✗"
    cat(sprintf("%s %-10s: %d observations\n", status, group, size))
    
    if (size < min_size) {
      issues <- c(issues, sprintf("Group '%s' has insufficient sample size (%d < %d)", 
                                  group, size, min_size))
    }
  }
  
  # Overall assessment
  total_adequate <- sum(group_sizes >= min_size)
  total_groups <- length(group_sizes)
  
  if (total_adequate < 2) {
    issues <- c(issues, "Insufficient groups with adequate sample sizes for comparison")
    cat("✗ Insufficient groups with adequate sample sizes for comparison\n")
  } else {
    cat(sprintf("✓ %d out of %d groups have adequate sample sizes\n", total_adequate, total_groups))
  }
  
  return(list(
    valid = length(issues) == 0,
    issues = issues,
    group_sizes = group_sizes,
    min_required = min_size,
    adequate_groups = total_adequate
  ))
}

# Validate data types for statistical analysis
validate_data_types_for_stats <- function(data) {
  
  cat("\n4. DATA TYPE VALIDATION\n")
  cat("-----------------------\n")
  
  warnings <- c()
  
  # Identify column types
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  factor_cols <- names(data)[sapply(data, is.factor)]
  character_cols <- names(data)[sapply(data, is.character)]
  logical_cols <- names(data)[sapply(data, is.logical)]
  
  cat(sprintf("Numeric columns: %d (%s)\n", 
              length(numeric_cols), 
              if(length(numeric_cols) > 0) paste(numeric_cols, collapse = ", ") else "none"))
  
  cat(sprintf("Factor columns: %d (%s)\n", 
              length(factor_cols), 
              if(length(factor_cols) > 0) paste(factor_cols, collapse = ", ") else "none"))
  
  if (length(character_cols) > 0) {
    cat(sprintf("WARNING: Character columns detected: %s (consider converting to factors)\n", 
                paste(character_cols, collapse = ", ")))
    warnings <- c(warnings, "Character columns detected - may need conversion to factors")
  }
  
  if (length(logical_cols) > 0) {
    cat(sprintf("WARNING: Logical columns detected: %s (consider converting to factors)\n", 
                paste(logical_cols, collapse = ", ")))
    warnings <- c(warnings, "Logical columns detected - may need conversion to factors")
  }
  
  # Check for adequate variables for analysis
  if (length(numeric_cols) < 2) {
    warnings <- c(warnings, "Few numeric variables may limit analysis options")
    cat("WARNING: Few numeric variables available for correlation/regression analysis\n")
  }
  
  # Check factor levels
  for (col in factor_cols) {
    n_levels <- nlevels(data[[col]])
    if (n_levels < 2) {
      warnings <- c(warnings, sprintf("Factor '%s' has only %d level(s)", col, n_levels))
      cat(sprintf("WARNING: Factor '%s' has only %d level(s)\n", col, n_levels))
    } else if (n_levels > 10) {
      warnings <- c(warnings, sprintf("Factor '%s' has many levels (%d)", col, n_levels))
              cat(sprintf("WARNING: Factor '%s' has many levels (%d) - may need grouping\n", col, n_levels))
    }
  }
  
  return(list(
    warnings = warnings,
    numeric_count = length(numeric_cols),
    factor_count = length(factor_cols),
    column_types = list(
      numeric = numeric_cols,
      factor = factor_cols,
      character = character_cols,
      logical = logical_cols
    )
  ))
}

# Validate basic statistical assumptions
validate_statistical_assumptions <- function(data, group_column) {
  
  cat("\n5. STATISTICAL ASSUMPTIONS CHECK\n")
  cat("--------------------------------\n")
  
  warnings <- c()
  assumption_results <- list()
  
  if (!group_column %in% names(data)) {
    warnings <- c(warnings, "Cannot check assumptions: group column missing")
    cat("✗ Cannot check assumptions: group column missing\n")
    return(list(warnings = warnings, results = assumption_results))
  }
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- numeric_cols[numeric_cols != group_column]  # Exclude group column
  
  if (length(numeric_cols) == 0) {
    warnings <- c(warnings, "No numeric variables for assumption testing")
    cat("WARNING: No numeric variables available for assumption testing\n")
    return(list(warnings = warnings, results = assumption_results))
  }
  
  cat("Checking assumptions for numeric variables:\n")
  
  for (col in numeric_cols) {
    cat(sprintf("\nVariable: %s\n", col))
    
    # Remove missing values for testing
    complete_data <- data[!is.na(data[[col]]) & !is.na(data[[group_column]]), ]
    
    if (nrow(complete_data) < 10) {
      cat(sprintf("  WARNING: Insufficient data for assumption testing (n=%d)\n", nrow(complete_data)))
      warnings <- c(warnings, sprintf("Insufficient data for assumption testing: %s", col))
      next
    }
    
    # Basic normality check (Shapiro-Wilk for small samples)
    if (nrow(complete_data) <= 50) {
      tryCatch({
        shapiro_test <- shapiro.test(complete_data[[col]])
        if (shapiro_test$p.value < 0.05) {
          cat(sprintf("  WARNING: Possible non-normality detected (p=%.3f)\n", shapiro_test$p.value))
          warnings <- c(warnings, sprintf("Possible non-normality in %s", col))
        } else {
          cat(sprintf("  ✓ Normality assumption reasonable (p=%.3f)\n", shapiro_test$p.value))
        }
        assumption_results[[col]]$normality <- shapiro_test
      }, error = function(e) {
        cat(sprintf("  WARNING: Normality test failed for %s\n", col))
      })
    } else {
      cat(sprintf("  → Large sample (n=%d) - CLT may apply\n", nrow(complete_data)))
    }
    
    # Basic variance homogeneity check
    groups <- unique(complete_data[[group_column]])
    if (length(groups) >= 2) {
      group_vars <- tapply(complete_data[[col]], complete_data[[group_column]], var, na.rm = TRUE)
      max_var <- max(group_vars, na.rm = TRUE)
      min_var <- min(group_vars, na.rm = TRUE)
      
      if (max_var / min_var > 4) {  # Rule of thumb: variance ratio > 4
        cat(sprintf("  WARNING: Unequal variances detected (ratio=%.2f)\n", max_var / min_var))
        warnings <- c(warnings, sprintf("Unequal variances in %s", col))
      } else {
        cat(sprintf("  ✓ Variance homogeneity reasonable (ratio=%.2f)\n", max_var / min_var))
      }
      assumption_results[[col]]$variance_ratio <- max_var / min_var
    }
    
    # Check for extreme outliers
    q1 <- quantile(complete_data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(complete_data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    extreme_outliers <- sum(complete_data[[col]] < (q1 - 3 * iqr) | 
                           complete_data[[col]] > (q3 + 3 * iqr), na.rm = TRUE)
    
    if (extreme_outliers > 0) {
              cat(sprintf("  WARNING: %d extreme outliers detected\n", extreme_outliers))
      warnings <- c(warnings, sprintf("Extreme outliers in %s", col))
    } else {
      cat("  ✓ No extreme outliers detected\n")
    }
    assumption_results[[col]]$extreme_outliers <- extreme_outliers
  }
  
  return(list(
    warnings = warnings,
    results = assumption_results,
    variables_tested = numeric_cols
  ))
}

# Validate statistical power and adequacy
validate_statistical_power <- function(data, group_column) {
  
  cat("\n6. STATISTICAL POWER ASSESSMENT\n")
  cat("-------------------------------\n")
  
  warnings <- c()
  power_results <- list()
  
  if (!group_column %in% names(data)) {
    warnings <- c(warnings, "Cannot assess power: group column missing")
    cat("✗ Cannot assess power: group column missing\n")
    return(list(warnings = warnings, results = power_results))
  }
  
  # Calculate group sizes
  group_sizes <- table(data[[group_column]], useNA = "no")
  nonzero_sizes <- group_sizes[group_sizes > 0]
  min_group_size <- if (length(nonzero_sizes) > 0) min(nonzero_sizes) else 0
  max_group_size <- if (length(nonzero_sizes) > 0) max(nonzero_sizes) else 0
  total_n <- sum(group_sizes)
  
  cat(sprintf("Total sample size: %d\n", total_n))
  cat(sprintf("Group sizes: min=%d, max=%d\n", min_group_size, max_group_size))
  
  # Basic power assessment based on sample sizes
  if (min_group_size < 5) {
    warnings <- c(warnings, "Very low power expected due to small group sizes")
    cat("WARNING: Very low statistical power expected (group size < 5)\n")
  } else if (min_group_size < 10) {
    warnings <- c(warnings, "Low power for detecting small effect sizes")
          cat("WARNING: Low power for detecting small effect sizes (group size < 10)\n")
  } else if (min_group_size < 20) {
          cat("WARNING: Moderate power - may miss small effect sizes (group size < 20)\n")
    warnings <- c(warnings, "Moderate power - may miss small effect sizes")
  } else {
    cat("✓ Adequate power for detecting medium to large effect sizes\n")
  }
  
  # Check for balanced design using non-zero group sizes
  balance_ratio <- if (length(nonzero_sizes) > 1) max(nonzero_sizes) / min(nonzero_sizes) else NA
  if (!is.na(balance_ratio) && balance_ratio > 2) {
    warnings <- c(warnings, "Unbalanced design may reduce power")
          cat(sprintf("WARNING: Unbalanced design detected (ratio=%.2f)\n", balance_ratio))
  } else {
    cat("✓ Reasonably balanced design\n")
  }

  power_results$group_sizes <- group_sizes
  power_results$balance_ratio <- balance_ratio
  power_results$total_n <- total_n
  
  return(list(
    warnings = warnings,
    results = power_results
  ))
}

# Generate comprehensive validation report
generate_validation_report <- function(validation_results, issues, warnings) {
  
  cat("\n=== VALIDATION SUMMARY ===\n")
  
  # Overall status
  if (length(issues) == 0) {
    cat("VALIDATION PASSED: Data is ready for statistical analysis\n")
  } else {
    cat("VALIDATION FAILED: Critical issues must be addressed\n")
  }
  
  # Critical issues
  if (length(issues) > 0) {
    cat("\nCRITICAL ISSUES:\n")
    for (i in seq_along(issues)) {
      cat(sprintf("%d. %s\n", i, issues[i]))
    }
  }
  
  # Warnings
  if (length(warnings) > 0) {
    cat("\nWARNINGS:\n")
    for (i in seq_along(warnings)) {
      cat(sprintf("%d. %s\n", i, warnings[i]))
    }
  }
  
  cat("\n========================\n")
  
  return(list(
    timestamp = Sys.time(),
    overall_status = length(issues) == 0,
    critical_issues = length(issues),
    warnings = length(warnings),
    details = validation_results
  ))
}

# Generate analysis recommendations based on validation
generate_analysis_recommendations <- function(validation_results) {
  
  recommendations <- c()
  
  # Sample size recommendations
  if (!is.null(validation_results$sample_sizes)) {
    if (validation_results$sample_sizes$adequate_groups < 2) {
      recommendations <- c(recommendations, 
                          "Consider collecting more data or combining small groups")
    }
  }
  
  # Data type recommendations
  if (!is.null(validation_results$data_types)) {
    if (validation_results$data_types$numeric_count < 3) {
      recommendations <- c(recommendations, 
                          "Limited numeric variables - focus on categorical analyses")
    }
  }
  
  # Assumption recommendations
  if (!is.null(validation_results$assumptions)) {
    if (length(validation_results$assumptions$warnings) > 0) {
      recommendations <- c(recommendations, 
                          "Consider non-parametric tests due to assumption violations",
                          "Evaluate data transformations to improve normality")
    }
  }
  
  # Power recommendations
  if (!is.null(validation_results$power)) {
    if (validation_results$power$results$balance_ratio > 2) {
      recommendations <- c(recommendations, 
                          "Consider stratified analysis due to unbalanced design")
    }
  }
  
  return(recommendations)
}
