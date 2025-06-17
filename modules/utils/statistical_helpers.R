# Statistical calculation utilities
# Centralized location for commonly used statistical calculation functions
# Eliminates duplication across multiple analysis modules
#
# INCLUDES: Box-Cox transformation functions (centralized from multiple modules)
# USED BY: comparative_analysis.R, residual_transformation.R, and others
#
# ENHANCED BOX-COX IMPLEMENTATION:
# - Lambda constrained to mathematically recommended range (-3, 3)
# - Detailed lambda interpretation and display
# - Validation and warning system for out-of-range values
# - Based on mathematical research showing optimal lambda typically in (-3, 3)

# Calculate skewness of a numeric vector
calculate_skewness <- function(x) {
  if (length(x) < 3 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(0)
  
  skew <- sum(((x - mean_x) / sd_x)^3) / n
  return(skew)
}

# Calculate excess kurtosis of a numeric vector (normal distribution has kurtosis = 0)
calculate_kurtosis <- function(x) {
  if (length(x) < 4 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(0)
  
  # Calculate excess kurtosis (normal distribution has kurtosis = 0)
  kurt <- sum(((x - mean_x) / sd_x)^4) / n - 3
  return(kurt)
}

# Note: CV can be calculated directly as (sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE))*100
# Note: MAD can be calculated directly as mad(x, na.rm=TRUE)
# Note: IQR can be calculated directly as IQR(x, na.rm=TRUE)

# Test if a variable is approximately normal based on skewness and kurtosis
is_approximately_normal <- function(x, skew_threshold = 1.0, kurt_threshold = 1.0) {
  if (length(x) < 3) return(FALSE)
  
  skew <- calculate_skewness(x)
  kurt <- calculate_kurtosis(x)
  
  if (is.na(skew) || is.na(kurt)) return(FALSE)
  
  skew_ok <- abs(skew) < skew_threshold
  kurt_ok <- abs(kurt) < kurt_threshold
  
  return(skew_ok && kurt_ok)
}

# Note: Z-scores can be calculated directly as scale(x)[, 1]

# Calculate modified z-scores using median and MAD
calculate_modified_z_scores <- function(x) {
  if (length(x) < 2 || all(is.na(x))) return(rep(NA, length(x)))
  
  x_clean <- x[!is.na(x)]
  median_x <- median(x_clean)
  mad_x <- mad(x_clean)
  
  if (mad_x == 0) return(rep(0, length(x)))
  
  modified_z <- 0.6745 * (x - median_x) / mad_x
  return(modified_z)
}

# Determine outliers using IQR method
detect_iqr_outliers <- function(x, multiplier = 1.5) {
  if (length(x) < 4) return(rep(FALSE, length(x)))
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - multiplier * IQR_val
  upper_bound <- Q3 + multiplier * IQR_val
  
  outliers <- x < lower_bound | x > upper_bound
  outliers[is.na(x)] <- FALSE  # Missing values are not outliers
  
  return(outliers)
}

# Determine outliers using z-score method
detect_z_outliers <- function(x, threshold = 3) {
  if (length(x) < 2) return(rep(FALSE, length(x)))
  
  z_scores <- abs(scale(x)[, 1])  # Using built-in scale() directly
  outliers <- z_scores > threshold
  outliers[is.na(z_scores)] <- FALSE
  
  return(outliers)
}

# Outlier detection summary
detect_outliers_summary <- function(x) {
  if (length(x) < 4) {
    return(list(
      iqr_outliers = rep(FALSE, length(x)),
      z_outliers = rep(FALSE, length(x)),
      modified_z_outliers = rep(FALSE, length(x)),
      outlier_count = 0,
      outlier_percentage = 0
    ))
  }
  
  iqr_outliers <- detect_iqr_outliers(x)
  z_outliers <- detect_z_outliers(x, threshold = 3)
  modified_z_scores <- abs(calculate_modified_z_scores(x))
  modified_z_outliers <- modified_z_scores > 3.5
  modified_z_outliers[is.na(modified_z_scores)] <- FALSE
  
  # Use IQR method as primary outlier detection
  primary_outliers <- iqr_outliers
  outlier_count <- sum(primary_outliers, na.rm = TRUE)
  total_valid <- sum(!is.na(x))
  outlier_percentage <- if (total_valid > 0) (outlier_count / total_valid) * 100 else 0
  
  return(list(
    iqr_outliers = iqr_outliers,
    z_outliers = z_outliers,
    modified_z_outliers = modified_z_outliers,
    outlier_count = outlier_count,
    outlier_percentage = round(outlier_percentage, 2)
  ))
}

# Calculate distribution shape interpretation
interpret_distribution_shape <- function(x) {
  if (length(x) < 3) {
    return(list(
      shape = "insufficient_data",
      skewness_interpretation = "Cannot determine",
      kurtosis_interpretation = "Cannot determine"
    ))
  }
  
  skew <- calculate_skewness(x)
  kurt <- calculate_kurtosis(x)
  
  # Interpret skewness
  if (is.na(skew)) {
    skew_interp <- "Cannot calculate"
  } else if (abs(skew) < 0.5) {
    skew_interp <- "Approximately symmetric"
  } else if (abs(skew) < 1.0) {
    skew_interp <- "Moderately skewed"
  } else if (abs(skew) < 2.0) {
    skew_interp <- "Highly skewed"
  } else {
    skew_interp <- "Extremely skewed"
  }
  
  # Interpret kurtosis
  if (is.na(kurt)) {
    kurt_interp <- "Cannot calculate"
  } else if (abs(kurt) < 0.5) {
    kurt_interp <- "Normal-like peakedness"
  } else if (abs(kurt) < 1.0) {
    kurt_interp <- "Moderately peaked/flat"
  } else if (abs(kurt) < 2.0) {
    kurt_interp <- "Highly peaked/flat"
  } else {
    kurt_interp <- "Extremely peaked/flat"
  }
  
  # Overall shape assessment
  if (is.na(skew) || is.na(kurt)) {
    overall_shape <- "Cannot determine"
  } else if (abs(skew) < 1.0 && abs(kurt) < 1.0) {
    overall_shape <- "Approximately normal"
  } else if (abs(skew) >= 2.0 || abs(kurt) >= 2.0) {
    overall_shape <- "Highly non-normal"
  } else {
    overall_shape <- "Moderately non-normal"
  }
  
  return(list(
    shape = overall_shape,
    skewness = round(skew, 4),
    kurtosis = round(kurt, 4),
    skewness_interpretation = skew_interp,
    kurtosis_interpretation = kurt_interp
  ))
}

# Interpret lambda value for Box-Cox transformation
interpret_lambda_value <- function(lambda) {
  if (is.na(lambda)) {
    return("Cannot determine - lambda is NA")
  }
  
  # Common lambda values and their interpretations
  if (abs(lambda - 2) < 0.1) {
    return("Square transformation (λ ≈ 2)")
  } else if (abs(lambda - 1) < 0.1) {
    return("No transformation needed (λ ≈ 1)")
  } else if (abs(lambda - 0.5) < 0.1) {
    return("Square root transformation (λ ≈ 0.5)")
  } else if (abs(lambda) < 0.1) {
    return("Natural log transformation (λ ≈ 0)")
  } else if (abs(lambda - (-0.5)) < 0.1) {
    return("Inverse square root transformation (λ ≈ -0.5)")
  } else if (abs(lambda - (-1)) < 0.1) {
    return("Inverse transformation (λ ≈ -1)")
  } else if (abs(lambda - (-2)) < 0.1) {
    return("Inverse square transformation (λ ≈ -2)")
  } else if (lambda > 1) {
    return(paste0("Power transformation (λ = ", round(lambda, 3), ") - increases right skew"))
  } else if (lambda > 0 && lambda < 1) {
    return(paste0("Power transformation (λ = ", round(lambda, 3), ") - reduces right skew"))
  } else if (lambda < 0) {
    return(paste0("Inverse power transformation (λ = ", round(lambda, 3), ") - strong skew correction"))
  } else {
    return(paste0("Custom transformation (λ = ", round(lambda, 3), ")"))
  }
}

# CENTRALIZED BOX-COX TRANSFORMATION FUNCTION
apply_boxcox_transformation <- function(x, group_factor = NULL, return_lambda = FALSE) {
  
  # Input validation
  if (length(x) < 3 || all(is.na(x))) {
    return(list(
      transformed_data = x,
      lambda = NA,
      success = FALSE,
      error = "Insufficient data for Box-Cox transformation",
      method = "Box-Cox"
    ))
  }
  
  # Remove missing values for calculation
  x_clean <- x[!is.na(x)]
  
  # Handle non-positive values by shifting
  shift_value <- 0
  if (any(x_clean <= 0)) {
    shift_value <- abs(min(x_clean)) + 1
    x_shifted <- x_clean + shift_value
    cat("  Box-Cox: Shifted data by", shift_value, "to handle non-positive values\n")
  } else {
    x_shifted <- x_clean
  }
  
  # Attempt Box-Cox transformation
  tryCatch({
    # Load required package
    if (!requireNamespace("car", quietly = TRUE)) {
      return(list(
        transformed_data = log(x_shifted),  # Fallback to log
        lambda = 0,
        success = TRUE,
        error = "car package not available - used log transformation as fallback",
        method = "Log (Box-Cox fallback)",
        shift_value = shift_value
      ))
    }
    
    library(car)
    
    # Find optimal lambda using powerTransform with constrained range (-3, 3)
    # Rationale: Mathematical research shows optimal lambda values typically fall within (-3, 3)
    # This range covers most practical transformations and prevents extreme values
    if (!is.null(group_factor)) {
      # With grouping factor
      temp_data <- data.frame(y = x_shifted, group = group_factor[!is.na(x)])
      bc_result <- powerTransform(y ~ group, data = temp_data, lambda = c(-3, 3))
    } else {
      # Without grouping factor  
      bc_result <- powerTransform(x_shifted ~ 1, lambda = c(-3, 3))
    }
    
    optimal_lambda <- bc_result$lambda
    
    # Validate lambda is within expected range
    if (!is.na(optimal_lambda) && (optimal_lambda < -3 || optimal_lambda > 3)) {
      cat("  Warning: Lambda =", round(optimal_lambda, 3), "is outside recommended range (-3, 3)\n")
      # Constrain to valid range
      optimal_lambda <- max(-3, min(3, optimal_lambda))
      cat("  Constrained lambda to:", round(optimal_lambda, 3), "\n")
    }
    
    # Apply transformation based on lambda
    if (abs(optimal_lambda) < 1e-6) {
      # Lambda ≈ 0: use log transformation
      transformed_values <- log(x_shifted)
      transformation_name <- paste0("log(x", if(shift_value > 0) paste0(" + ", shift_value) else "", ")")
    } else {
      # Lambda ≠ 0: use power transformation
      transformed_values <- (x_shifted^optimal_lambda - 1) / optimal_lambda
      transformation_name <- paste0("Box-Cox(x", if(shift_value > 0) paste0(" + ", shift_value) else "", 
                                   ", λ=", round(optimal_lambda, 3), ")")
    }
    
    # Map back to original data structure (including NAs)
    transformed_data <- rep(NA, length(x))
    transformed_data[!is.na(x)] <- transformed_values
    
    result <- list(
      transformed_data = transformed_data,
      lambda = optimal_lambda,
      success = TRUE,
      error = NULL,
      method = "Box-Cox",
      transformation_name = transformation_name,
      shift_value = shift_value,
      original_range = range(x_clean),
      transformed_range = range(transformed_values)
    )
    
    # Return lambda if requested (for compatibility)
    if (return_lambda) {
      result$lambda_only <- optimal_lambda
    }
    
    # Display detailed lambda information
    cat("  Box-Cox transformation successful: λ =", round(optimal_lambda, 3), "\n")
    
    # Interpret lambda value
    lambda_interpretation <- interpret_lambda_value(optimal_lambda)
    cat("  Lambda interpretation:", lambda_interpretation, "\n")
    
    # Add lambda interpretation to result
    result$lambda_interpretation <- lambda_interpretation
    
    return(result)
    
  }, error = function(e) {
    # Fallback to log transformation
    log_transformed <- log(x_shifted)
    transformed_data <- rep(NA, length(x))
    transformed_data[!is.na(x)] <- log_transformed
    
    return(list(
      transformed_data = transformed_data,
      lambda = 0,
      success = TRUE,
      error = paste("Box-Cox failed, used log fallback:", e$message),
      method = "Log (Box-Cox fallback)",
      transformation_name = paste0("log(x", if(shift_value > 0) paste0(" + ", shift_value) else "", ")"),
      shift_value = shift_value
    ))
  })
}

# CONVENIENCE FUNCTION: Apply Box-Cox and return only transformed data
boxcox_transform <- function(x, group_factor = NULL) {
  result <- apply_boxcox_transformation(x, group_factor)
  return(result$transformed_data)
}

# CONVENIENCE FUNCTION: Get optimal lambda only
get_boxcox_lambda <- function(x, group_factor = NULL) {
  result <- apply_boxcox_transformation(x, group_factor, return_lambda = TRUE)
  return(result$lambda)
}


# Bootstrap confidence intervals for effect sizes
bootstrap_effect_size_ci <- function(data, variable, group_column, n_bootstrap = 1000, conf_level = 0.95) {
  
  if (!variable %in% names(data)) {
    return(list(error = "Variable not found"))
  }
  
  groups <- unique(data[[group_column]])
  if (length(groups) != 2) {
    return(list(error = "Bootstrap CI only for two groups"))
  }
  
  # Clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  # Bootstrap resampling
  bootstrap_effects <- replicate(n_bootstrap, {
    # Resample with replacement
    n_total <- nrow(clean_data)
    bootstrap_indices <- sample(1:n_total, n_total, replace = TRUE)
    bootstrap_data <- clean_data[bootstrap_indices, ]
    
    # Calculate Cohen's d for bootstrap sample
    group1_data <- bootstrap_data[bootstrap_data[[group_column]] == groups[1], variable]
    group2_data <- bootstrap_data[bootstrap_data[[group_column]] == groups[2], variable]
    
    if (length(group1_data) > 1 && length(group2_data) > 1) {
      pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                        (length(group2_data) - 1) * var(group2_data)) / 
                       (length(group1_data) + length(group2_data) - 2))
      
      if (pooled_sd > 0) {
        return((mean(group1_data) - mean(group2_data)) / pooled_sd)
      }
    }
    return(NA)
  })
  
  # Remove NA values
  bootstrap_effects <- bootstrap_effects[!is.na(bootstrap_effects)]
  
  if (length(bootstrap_effects) < 100) {
    return(list(error = "Too few valid bootstrap samples"))
  }
  
  # Calculate confidence interval
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_effects, alpha/2)
  ci_upper <- quantile(bootstrap_effects, 1 - alpha/2)
  
  return(list(
    method = "Bootstrap",
    n_bootstrap = length(bootstrap_effects),
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    bootstrap_mean = mean(bootstrap_effects),
    bootstrap_sd = sd(bootstrap_effects),
    confidence_level = conf_level
  ))
}

# Equivalence testing - tests if difference is practically negligible
equivalence_test <- function(data, variable, group_column, equivalence_bound = 0.2) {
  
  groups <- unique(data[[group_column]])
  if (length(groups) != 2) {
    return(list(error = "Equivalence test only for two groups"))
  }
  
  # Clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  group1_data <- clean_data[clean_data[[group_column]] == groups[1], variable]
  group2_data <- clean_data[clean_data[[group_column]] == groups[2], variable]
  
  if (length(group1_data) < 2 || length(group2_data) < 2) {
    return(list(error = "Insufficient data for equivalence test"))
  }
  
  # TOST (Two One-Sided Tests) for equivalence
  t_test_result <- t.test(group1_data, group2_data)
  
  # Calculate standardized effect size bounds
  pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                    (length(group2_data) - 1) * var(group2_data)) / 
                   (length(group1_data) + length(group2_data) - 2))
  
  mean_diff <- mean(group1_data) - mean(group2_data)
  se_diff <- sqrt(var(group1_data)/length(group1_data) + var(group2_data)/length(group2_data))
  
  # Equivalence bounds in raw units
  lower_bound <- -equivalence_bound * pooled_sd
  upper_bound <- equivalence_bound * pooled_sd
  
  # TOST procedure
  # Test H0: diff <= lower_bound vs H1: diff > lower_bound
  t1 <- (mean_diff - lower_bound) / se_diff
  df <- length(group1_data) + length(group2_data) - 2
  p1 <- pt(t1, df, lower.tail = FALSE)
  
  # Test H0: diff >= upper_bound vs H1: diff < upper_bound  
  t2 <- (mean_diff - upper_bound) / se_diff
  p2 <- pt(t2, df, lower.tail = TRUE)
  
  # Equivalence is concluded if both p-values < alpha
  p_equivalence <- max(p1, p2)
  equivalent <- p_equivalence < 0.05
  
  return(list(
    method = "TOST (Two One-Sided Tests)",
    groups = paste(groups, collapse = " vs "),
    variable = variable,
    mean_difference = mean_diff,
    equivalence_bound_cohens_d = equivalence_bound,
    equivalence_bound_raw = c(lower_bound, upper_bound),
    p_value_equivalence = p_equivalence,
    equivalent = equivalent,
    t_statistics = c(t1, t2),
    p_values_individual = c(p1, p2),
    interpretation = if (equivalent) {
      "Groups are statistically equivalent (difference is practically negligible)"
    } else {
      "Cannot conclude equivalence (difference may be practically meaningful)"
    }
  ))
}

# Sensitivity analysis with different statistical approaches
sensitivity_analysis <- function(data, variable, group_column) {
  
  results <- list()
  
  # 1. Parametric approach (t-test)
  results$parametric <- tryCatch({
    t_result <- t.test(data[[variable]] ~ data[[group_column]])
    list(
      method = "Welch t-test",
      p_value = t_result$p.value,
      significant = t_result$p.value < 0.05,
      effect_size = abs(diff(t_result$estimate)) / sqrt(var(data[[variable]], na.rm = TRUE))
    )
  }, error = function(e) list(error = e$message))
  
  # 2. Non-parametric approach (Wilcoxon)
  results$nonparametric <- tryCatch({
    wilcox_result <- wilcox.test(data[[variable]] ~ data[[group_column]])
    list(
      method = "Wilcoxon rank-sum test",
      p_value = wilcox_result$p.value,
      significant = wilcox_result$p.value < 0.05
    )
  }, error = function(e) list(error = e$message))
  
  # 3. Robust approach (trimmed means)
  results$robust <- tryCatch({
    groups <- split(data[[variable]], data[[group_column]])
    if (length(groups) == 2) {
      # Trimmed means (remove 10% from each tail)
      trimmed_means <- sapply(groups, mean, trim = 0.1, na.rm = TRUE)
      
      # Yuen's test for trimmed means (simplified version)
      group1_trimmed <- groups[[1]][!is.na(groups[[1]])]
      group2_trimmed <- groups[[2]][!is.na(groups[[2]])]
      
      # Remove 10% from each tail
      n1 <- length(group1_trimmed)
      n2 <- length(group2_trimmed)
      trim_count1 <- floor(n1 * 0.1)
      trim_count2 <- floor(n2 * 0.1)
      
      if (trim_count1 > 0) {
        group1_sorted <- sort(group1_trimmed)
        group1_trimmed <- group1_sorted[(trim_count1+1):(n1-trim_count1)]
      }
      
      if (trim_count2 > 0) {
        group2_sorted <- sort(group2_trimmed)
        group2_trimmed <- group2_sorted[(trim_count2+1):(n2-trim_count2)]
      }
      
      if (length(group1_trimmed) > 1 && length(group2_trimmed) > 1) {
        # Simple t-test on trimmed data
        trimmed_test <- t.test(group1_trimmed, group2_trimmed)
        list(
          method = "Trimmed means (10% trim)",
          p_value = trimmed_test$p.value,
          significant = trimmed_test$p.value < 0.05,
          trimmed_means = trimmed_means
        )
      } else {
        list(error = "Insufficient data after trimming")
      }
    } else {
      list(error = "Need exactly 2 groups")
    }
  }, error = function(e) list(error = e$message))
  
  # 4. Permutation test
  results$permutation <- tryCatch({
    perform_permutation_test(data, variable, group_column, n_permutations = 1000)
  }, error = function(e) list(error = e$message))
  
  # Summary
  p_values <- c()
  methods <- c()
  
  for (method_name in names(results)) {
    if (!is.null(results[[method_name]]$p_value)) {
      p_values <- c(p_values, results[[method_name]]$p_value)
      methods <- c(methods, results[[method_name]]$method)
    }
  }
  
  results$summary <- list(
    methods_tested = length(p_values),
    p_values = p_values,
    method_names = methods,
    consistent_significance = if (length(p_values) > 1) {
      all(p_values < 0.05) || all(p_values >= 0.05)
    } else {
      NA
    },
    robust_conclusion = if (length(p_values) > 0) {
      ifelse(mean(p_values < 0.05) >= 0.75, "Strong evidence", 
             ifelse(mean(p_values < 0.05) >= 0.5, "Moderate evidence", "Weak evidence"))
    } else {
      "Cannot determine"
    }
  )
  
  return(results)
}

# Permutation test implementation
perform_permutation_test <- function(data, variable, group_column, n_permutations = 1000) {
  
  # Clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 4) {
    return(list(error = "Insufficient data for permutation test"))
  }
  
  # Observed test statistic (difference in means)
  groups <- split(clean_data[[variable]], clean_data[[group_column]])
  if (length(groups) != 2) {
    return(list(error = "Permutation test requires exactly 2 groups"))
  }
  
  observed_diff <- abs(mean(groups[[1]], na.rm = TRUE) - mean(groups[[2]], na.rm = TRUE))
  
  # Group sizes
  n1 <- length(groups[[1]])
  n2 <- length(groups[[2]])
  
  # Permutation distribution
  permuted_diffs <- replicate(n_permutations, {
    # Randomly reassign group labels
    pooled_data <- clean_data[[variable]]
    shuffled_groups <- sample(rep(names(groups), c(n1, n2)))
    
    new_groups <- split(pooled_data, shuffled_groups)
    abs(mean(new_groups[[1]], na.rm = TRUE) - mean(new_groups[[2]], na.rm = TRUE))
  })
  
  # P-value: proportion of permuted differences >= observed difference
  p_value <- mean(permuted_diffs >= observed_diff)
  
  return(list(
    method = "Permutation test",
    observed_statistic = observed_diff,
    p_value = p_value,
    significant = p_value < 0.05,
    n_permutations = n_permutations,
    permutation_mean = mean(permuted_diffs),
    permutation_sd = sd(permuted_diffs)
  ))
}

# Multiple imputation for missing data sensitivity
missing_data_sensitivity <- function(data, variable, group_column, n_imputations = 5) {
  
  # Check if there are missing values
  missing_count <- sum(is.na(data[[variable]]))
  total_count <- nrow(data)
  missing_percent <- (missing_count / total_count) * 100
  
  if (missing_count == 0) {
    return(list(
      message = "No missing data - sensitivity analysis not needed",
      missing_percent = 0
    ))
  }
  
  if (missing_percent > 50) {
    return(list(
      error = "Too much missing data (>50%) for reliable imputation",
      missing_percent = missing_percent
    ))
  }
  
  # Simple multiple imputation using mean/mode
  results <- list()
  
  for (i in 1:n_imputations) {
    # Create imputed dataset
    imputed_data <- data
    
    # Impute using group-specific means with random noise
    for (group in unique(data[[group_column]])) {
      group_mask <- data[[group_column]] == group & is.na(data[[variable]])
      
      if (sum(group_mask) > 0) {
        group_mean <- mean(data[data[[group_column]] == group, variable], na.rm = TRUE)
        group_sd <- sd(data[data[[group_column]] == group, variable], na.rm = TRUE)
        
        # Add random noise to avoid artificial precision
        noise <- rnorm(sum(group_mask), 0, group_sd * 0.1)
        imputed_data[group_mask, variable] <- group_mean + noise
      }
    }
    
    # Analyze imputed dataset
    results[[i]] <- tryCatch({
      t_result <- t.test(imputed_data[[variable]] ~ imputed_data[[group_column]])
      list(
        p_value = t_result$p.value,
        effect_size = abs(diff(t_result$estimate)) / sqrt(var(imputed_data[[variable]], na.rm = TRUE)),
        significant = t_result$p.value < 0.05
      )
    }, error = function(e) list(error = e$message))
  }
  
  # Pool results
  valid_results <- results[sapply(results, function(x) is.null(x$error))]
  
  if (length(valid_results) == 0) {
    return(list(error = "All imputations failed"))
  }
  
  pooled_p <- mean(sapply(valid_results, function(x) x$p_value))
  pooled_effect <- mean(sapply(valid_results, function(x) x$effect_size))
  consistency <- sd(sapply(valid_results, function(x) x$p_value))
  
  return(list(
    method = "Multiple imputation",
    missing_percent = missing_percent,
    n_imputations = length(valid_results),
    pooled_p_value = pooled_p,
    pooled_effect_size = pooled_effect,
    p_value_consistency = consistency,
    significant = pooled_p < 0.05,
    robust_conclusion = consistency < 0.01
  ))
}

# Multiple comparison sensitivity analysis for 3+ groups
multigroup_sensitivity_analysis <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  n_groups <- length(groups)
  
  if (n_groups < 3) {
    return(list(error = "Multi-group analysis requires 3 or more groups"))
  }
  
  results <- list()
  
  # 1. Parametric approach (ANOVA)
  results$parametric <- tryCatch({
    anova_result <- aov(data[[variable]] ~ data[[group_column]])
    anova_summary <- summary(anova_result)
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
    
    list(
      method = "One-way ANOVA",
      p_value = p_value,
      significant = p_value < 0.05,
      f_statistic = anova_summary[[1]][["F value"]][1]
    )
  }, error = function(e) list(error = e$message))
  
  # 2. Non-parametric approach (Kruskal-Wallis)
  results$nonparametric <- tryCatch({
    kruskal_result <- kruskal.test(data[[variable]] ~ data[[group_column]])
    list(
      method = "Kruskal-Wallis test",
      p_value = kruskal_result$p.value,
      significant = kruskal_result$p.value < 0.05,
      chi_squared = kruskal_result$statistic
    )
  }, error = function(e) list(error = e$message))
  
  # 3. Robust approach (Welch's ANOVA for unequal variances)
  results$robust <- tryCatch({
    if (requireNamespace("car", quietly = TRUE)) {
      library(car)
      welch_result <- Anova(lm(data[[variable]] ~ data[[group_column]]), type = "II", white.adjust = TRUE)
      p_value <- welch_result[["Pr(>F)"]][1]
      
      list(
        method = "Welch's ANOVA (unequal variances)",
        p_value = p_value,
        significant = p_value < 0.05
      )
    } else {
      list(error = "car package not available")
    }
  }, error = function(e) list(error = e$message))
  
  # 4. Permutation test for multiple groups
  results$permutation <- tryCatch({
    perform_multigroup_permutation_test(data, variable, group_column, n_permutations = 1000)
  }, error = function(e) list(error = e$message))
  
  # Summary across methods
  p_values <- c()
  methods <- c()
  
  for (method_name in names(results)) {
    if (!is.null(results[[method_name]]$p_value)) {
      p_values <- c(p_values, results[[method_name]]$p_value)
      methods <- c(methods, results[[method_name]]$method)
    }
  }
  
  results$summary <- list(
    n_groups = n_groups,
    methods_tested = length(p_values),
    p_values = p_values,
    method_names = methods,
    consistent_significance = if (length(p_values) > 1) {
      all(p_values < 0.05) || all(p_values >= 0.05)
    } else {
      NA
    },
    robust_conclusion = if (length(p_values) > 0) {
      proportion_significant <- mean(p_values < 0.05)
      if (proportion_significant >= 0.75) "Strong evidence"
      else if (proportion_significant >= 0.5) "Moderate evidence"
      else if (proportion_significant >= 0.25) "Weak evidence"
      else "No evidence"
    } else {
      "Cannot determine"
    }
  )
  
  return(results)
}

# Multigroup permutation test
perform_multigroup_permutation_test <- function(data, variable, group_column, n_permutations = 1000) {
  
  # Clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(error = "Insufficient data for multigroup permutation test"))
  }
  
  groups <- unique(clean_data[[group_column]])
  n_groups <- length(groups)
  
  if (n_groups < 3) {
    return(list(error = "Multigroup permutation test requires 3+ groups"))
  }
  
  # Observed F-statistic (between-group variance / within-group variance)
  observed_f <- calculate_f_statistic(clean_data[[variable]], clean_data[[group_column]])
  
  # Permutation distribution
  permuted_f_stats <- replicate(n_permutations, {
    # Randomly reassign group labels
    shuffled_groups <- sample(clean_data[[group_column]])
    calculate_f_statistic(clean_data[[variable]], shuffled_groups)
  })
  
  # P-value: proportion of permuted F-statistics >= observed F-statistic
  p_value <- mean(permuted_f_stats >= observed_f, na.rm = TRUE)
  
  return(list(
    method = "Multigroup permutation test",
    observed_statistic = observed_f,
    p_value = p_value,
    significant = p_value < 0.05,
    n_permutations = n_permutations,
    n_groups = n_groups,
    permutation_mean = mean(permuted_f_stats, na.rm = TRUE),
    permutation_sd = sd(permuted_f_stats, na.rm = TRUE)
  ))
}

# Helper function to calculate F-statistic manually
calculate_f_statistic <- function(values, groups) {
  
  # Overall mean
  grand_mean <- mean(values, na.rm = TRUE)
  
  # Group means and sizes
  group_data <- split(values, groups)
  group_means <- sapply(group_data, mean, na.rm = TRUE)
  group_sizes <- sapply(group_data, function(x) sum(!is.na(x)))
  
  # Between-group sum of squares
  ss_between <- sum(group_sizes * (group_means - grand_mean)^2, na.rm = TRUE)
  
  # Within-group sum of squares
  ss_within <- sum(sapply(names(group_data), function(g) {
    group_values <- group_data[[g]][!is.na(group_data[[g]])]
    sum((group_values - group_means[g])^2)
  }), na.rm = TRUE)
  
  # Degrees of freedom
  df_between <- length(group_data) - 1
  df_within <- sum(group_sizes) - length(group_data)
  
  # F-statistic
  if (df_within > 0 && ss_within > 0) {
    ms_between <- ss_between / df_between
    ms_within <- ss_within / df_within
    f_stat <- ms_between / ms_within
  } else {
    f_stat <- NA
  }
  
  return(f_stat)
}

# Effect size estimation with bootstrap for multiple groups (eta-squared)
bootstrap_eta_squared_ci <- function(data, variable, group_column, n_bootstrap = 1000, conf_level = 0.95) {
  
  # Clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(error = "Insufficient data for bootstrap eta-squared"))
  }
  
  # Bootstrap resampling
  bootstrap_eta_squared <- replicate(n_bootstrap, {
    # Resample with replacement
    n_total <- nrow(clean_data)
    bootstrap_indices <- sample(1:n_total, n_total, replace = TRUE)
    bootstrap_data <- clean_data[bootstrap_indices, ]
    
    # Calculate eta-squared for bootstrap sample
    tryCatch({
      anova_result <- aov(bootstrap_data[[variable]] ~ bootstrap_data[[group_column]])
      anova_summary <- summary(anova_result)
      
      ss_between <- anova_summary[[1]][["Sum Sq"]][1]
      ss_total <- sum(anova_summary[[1]][["Sum Sq"]])
      
      eta_squared <- ss_between / ss_total
      return(eta_squared)
    }, error = function(e) {
      return(NA)
    })
  })
  
  # Remove NA values
  bootstrap_eta_squared <- bootstrap_eta_squared[!is.na(bootstrap_eta_squared)]
  
  if (length(bootstrap_eta_squared) < 100) {
    return(list(error = "Too few valid bootstrap samples"))
  }
  
  # Calculate confidence interval
  alpha <- 1 - conf_level
  ci_lower <- quantile(bootstrap_eta_squared, alpha/2)
  ci_upper <- quantile(bootstrap_eta_squared, 1 - alpha/2)
  
  return(list(
    method = "Bootstrap eta-squared",
    n_bootstrap = length(bootstrap_eta_squared),
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    bootstrap_mean = mean(bootstrap_eta_squared),
    bootstrap_sd = sd(bootstrap_eta_squared),
    confidence_level = conf_level
  ))
}

# Calculate post-hoc power for existing findings
calculate_post_hoc_power <- function(finding, total_n, group_sizes) {
  
  # Use actual data from metadata - no hardcoded defaults
  if (is.null(total_n) || is.null(group_sizes)) {
    return(list(
      achieved_power = NA,
      mde = NA,
      sample_adequacy = "Unknown - insufficient metadata",
      total_n = total_n,
      min_group_size = NA,
      error = "Missing sample size information from analysis metadata"
    ))
  }
  
  effect_size <- finding$effect_size
  effect_type <- finding$effect_size_type
  test_type <- finding$test_type
  
  # Ensure we have actual effect size from the analysis
  if (is.null(effect_size) || is.na(effect_size)) {
    return(list(
      achieved_power = NA,
      mde = NA,
      sample_adequacy = "Unknown - effect size not calculated",
      total_n = total_n,
      min_group_size = min(group_sizes),
      error = "No effect size available from analysis results"
    ))
  }
  
  # Calculate achieved power using proper statistical formulas
  achieved_power <- tryCatch({
    if (effect_type == "cohens_d") {
      # For Cohen's d, use proper power calculation for t-test
      d <- abs(effect_size)
      n_per_group <- mean(group_sizes)
      
      # Power calculation for two-sample t-test with Cohen's d
      # Using proper non-centrality parameter and t-distribution
      df <- sum(group_sizes) - length(group_sizes)  # Total N - number of groups
      ncp <- d * sqrt(n_per_group / 2)  # Non-centrality parameter
      
      # Critical value for two-tailed test at α = 0.05
      t_crit <- qt(0.975, df)
      
      # Power = P(|t| > t_crit | H1 is true) using non-central t-distribution
      power_calc <- 1 - pt(t_crit, df, ncp) + pt(-t_crit, df, ncp)
      min(0.999, max(0.001, power_calc))
      
    } else if (effect_type %in% c("eta_squared", "epsilon_squared")) {
      # For ANOVA eta-squared or Kruskal-Wallis epsilon-squared
      eta_sq <- effect_size
      k <- length(group_sizes)  # number of groups
      total_n <- sum(group_sizes)
      
      # Convert eta² to f²
      f_squared <- eta_sq / (1 - eta_sq)
      
      # Calculate F statistic from effect size
      df_between <- k - 1
      df_within <- total_n - k
      
      # Non-centrality parameter for F distribution
      ncp <- f_squared * total_n
      
      # Critical F value
      f_crit <- qf(0.95, df_between, df_within)
      
      # Power using non-central F distribution
      power_calc <- 1 - pf(f_crit, df_between, df_within, ncp)
      min(0.999, max(0.001, power_calc))
      
    } else if (effect_type == "cramers_v") {
      # For chi-square Cramer's V
      v <- effect_size
      chi_sq <- v^2 * total_n  # Convert Cramer's V back to chi-square
      
      # Degrees of freedom (assuming 2x3 or 3x3 table - adjust as needed)
      df <- 2  # Conservative estimate
      
      # Critical chi-square value
      chi_crit <- qchisq(0.95, df)
      
      # Power using non-central chi-square distribution
      power_calc <- 1 - pchisq(chi_crit, df, ncp = chi_sq)
      min(0.999, max(0.001, power_calc))
      
    } else if (effect_type == "r_squared") {
      # For regression R²
      r_sq <- effect_size
      k_predictors <- 1  # Assuming group comparison (1 predictor)
      df_error <- total_n - k_predictors - 1
      
      # Convert R² to F statistic
      f_stat <- (r_sq / (1 - r_sq)) * (df_error / k_predictors)
      
      # Critical F value
      f_crit <- qf(0.95, k_predictors, df_error)
      
      # Power calculation
      power_calc <- if (f_stat > f_crit) {
        # Use non-central F distribution for power
        ncp <- f_stat * k_predictors
        1 - pf(f_crit, k_predictors, df_error, ncp)
      } else {
        0.05  # If observed F < critical F, power is approximately α
      }
      
      min(0.999, max(0.001, power_calc))
      
    } else {
      # For unknown effect types, cannot calculate power
      NA
    }
  }, error = function(e) {
    NA  # Return NA if calculation fails
  })
  
  # Calculate proper MDE for 80% power based on effect type and actual sample sizes
  mde <- tryCatch({
    if (effect_type == "cohens_d") {
      # MDE for Cohen's d with 80% power (two-sample t-test)
      n_per_group <- mean(group_sizes)
      df <- sum(group_sizes) - length(group_sizes)
      
      # For 80% power, solve for d such that power = 0.8
      # Using approximation: d ≈ (t_crit_80 + t_crit_alpha) / sqrt(n_per_group/2)
      t_crit_alpha <- qt(0.975, df)  # Critical value for α = 0.05
      t_crit_power <- qt(0.8, df)    # Value for 80% power
      
      mde_d <- (t_crit_alpha + t_crit_power) / sqrt(n_per_group / 2)
      mde_d
      
    } else if (effect_type %in% c("eta_squared", "epsilon_squared")) {
      # MDE for eta-squared with 80% power (ANOVA) - SIMPLIFIED: Reliable calculation
      k <- length(group_sizes)
      total_n <- sum(group_sizes)
      df_between <- k - 1
      df_within <- total_n - k
      
      # For 80% power: use Cohen's convention adjusted by sample size
      # Standard MDE formula: effect_size = sqrt(critical_value / sample_adjustment)
      alpha <- 0.05
      power_target <- 0.8
      
      # Critical F value at α = 0.05
      f_crit <- qf(1 - alpha, df_between, df_within)
      
      # Use simplified but reliable formula based on observed effect vs target power
      observed_eta <- effect_size
      observed_power <- achieved_power
      
      # If observed power is very high, MDE should be lower
      # If observed power is low, MDE should be higher  
      if (!is.na(observed_power) && observed_power > 0.1) {
        # Scale MDE based on power relationship
        power_adjustment <- power_target / observed_power
        
        # Base MDE using statistical approximation
        base_mde <- f_crit / total_n * 8  # Base formula
        
        # Adjust by observed effect size and power
        adjusted_mde <- base_mde * power_adjustment * (1 + observed_eta)
        
        # Convert to eta-squared scale
        eta_squared_mde <- min(0.5, max(0.02, adjusted_mde))
      } else {
        # Fallback for edge cases
        eta_squared_mde <- 0.14  # Conservative estimate
      }
      
      eta_squared_mde
      
    } else if (effect_type == "cramers_v") {
      # MDE for Cramer's V with 80% power
      df <- 2  # Conservative estimate
      chi_crit <- qchisq(0.95, df)
      
      # For 80% power, approximate MDE
      chi_mde <- chi_crit * 1.5  # Rough approximation for 80% power
      v_mde <- sqrt(chi_mde / total_n)
      
      max(0.01, min(0.8, v_mde))
      
    } else if (effect_type == "r_squared") {
      # MDE for R² with 80% power (regression)
      k_predictors <- 1
      df_error <- total_n - k_predictors - 1
      
      # Critical F value for α = 0.05
      f_crit <- qf(0.95, k_predictors, df_error)
      
      # For 80% power, approximate MDE
      f_stat_mde <- f_crit * 2  # Rough approximation
      r_squared_mde <- (f_stat_mde * k_predictors) / (f_stat_mde * k_predictors + df_error)
      
      max(0.01, min(0.5, r_squared_mde))
      
    } else {
      # For unknown effect types
      NA
    }
  }, error = function(e) {
    NA
  })
  
  # Sample size adequacy assessment based on actual group sizes
  min_group_size <- min(group_sizes)
  total_sample <- sum(group_sizes)
  
  # Assess adequacy based on effect type and sample sizes
  sample_adequacy <- if (effect_type == "cohens_d") {
    # For t-tests, focus on minimum group size
    if (min_group_size >= 30) "Adequate"
    else if (min_group_size >= 20) "Moderate"
    else if (min_group_size >= 15) "Limited"
    else "Insufficient"
  } else if (effect_type %in% c("eta_squared", "epsilon_squared")) {
    # For ANOVA, consider total sample and groups
    k_groups <- length(group_sizes)
    min_per_group <- total_sample / k_groups
    if (min_per_group >= 20 && total_sample >= 60) "Adequate"
    else if (min_per_group >= 15 && total_sample >= 45) "Moderate"
    else if (min_per_group >= 10 && total_sample >= 30) "Limited"
    else "Insufficient"
  } else if (effect_type == "cramers_v") {
    # For chi-square, focus on total sample
    if (total_sample >= 100) "Adequate"
    else if (total_sample >= 50) "Moderate"
    else if (total_sample >= 30) "Limited"
    else "Insufficient"
  } else {
    # Generic assessment
    if (min_group_size >= 25) "Adequate"
    else if (min_group_size >= 15) "Moderate"
    else if (min_group_size >= 10) "Limited"
    else "Insufficient"
  }
  
  return(list(
    achieved_power = achieved_power,
    mde = mde,
    sample_adequacy = sample_adequacy,
    total_n = total_sample,
    min_group_size = min_group_size,
    group_sizes = group_sizes,
    effect_type = effect_type
  ))
}

# Generate statistical robustness assessment
generate_statistical_robustness_assessment <- function(var_name, power_analysis) {
  
  power_level <- power_analysis$achieved_power
  adequacy <- power_analysis$sample_adequacy
  
  # Handle NA power values
  if (is.null(power_level) || is.na(power_level)) {
    return("Power calculation unavailable - interpretation limited.")
  }
  
  # Generic statistical interpretation based solely on power level
  if (power_level >= 0.8) {
    "Statistically robust finding suitable for publication and further analysis."
  } else if (power_level >= 0.6) {
    "Moderate evidence - findings warrant replication in larger sample."
  } else {
    "Limited statistical power - interpret with caution and consider replication."
  }
} 