# Statistical assumptions dashboard
# Unified testing of statistical assumptions (normality, homogeneity of variance)
# Eliminates duplication across descriptive stats and comparative analysis modules


# Package dependencies
# Source statistical helper functions
source("modules/utils/statistical_helpers.R")

# Main function: Comprehensive assumptions testing dashboard
perform_assumptions_testing <- function(data, variables, group_column = NULL) {
  
  cat("=== STATISTICAL ASSUMPTIONS DASHBOARD ===\n")
  cat("Testing normality and variance assumptions...\n")
  
  # Filter to numeric variables only
  if (is.null(variables)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    # Remove group column if it exists
    if (!is.null(group_column) && group_column %in% numeric_vars) {
      numeric_vars <- numeric_vars[numeric_vars != group_column]
    }
  } else {
    numeric_vars <- variables[sapply(data[variables], is.numeric)]
  }
  
  if (length(numeric_vars) == 0) {
    cat("No numeric variables found for assumptions testing.\n")
    return(NULL)
  }
  
  cat("- Testing", length(numeric_vars), "numeric variables\n")
  if (!is.null(group_column)) {
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    cat("- Across", length(groups), "groups:", paste(groups, collapse = ", "), "\n")
  }
  
  # Initialize results structure
  assumptions_results <- list(
    normality_tests = list(),
    homogeneity_tests = list(),
    assumptions_summary = NULL,
    test_recommendations = list(),
    metadata = list(
      variables_tested = numeric_vars,
      group_column = group_column,
      groups = if (!is.null(group_column)) unique(data[[group_column]]) else NULL,
      test_date = Sys.time()
    )
  )
  
  # Step 1: Comprehensive normality testing
  cat("\n--- Step 1: Normality Testing ---\n")
  assumptions_results$normality_tests <- perform_comprehensive_normality_tests(data, numeric_vars, group_column)
  
  # Step 2: Homogeneity of variance testing (only if group column specified)
  if (!is.null(group_column)) {
    cat("\n--- Step 2: Homogeneity of Variance Testing ---\n")
    assumptions_results$homogeneity_tests <- perform_comprehensive_homogeneity_tests(data, numeric_vars, group_column)
  }
  
  # Step 3: Create assumptions summary table
  cat("\n--- Step 3: Generating Assumptions Summary ---\n")
  assumptions_results$assumptions_summary <- create_assumptions_summary_table(
    assumptions_results$normality_tests, 
    assumptions_results$homogeneity_tests,
    data, numeric_vars, group_column
  )
  
  # Step 4: Generate test recommendations
  cat("\n--- Step 4: Statistical Test Recommendations ---\n")
  assumptions_results$test_recommendations <- generate_test_recommendations(
    assumptions_results$normality_tests,
    assumptions_results$homogeneity_tests,
    data, numeric_vars, group_column
  )
  
  cat("Assumptions testing completed for", length(numeric_vars), "variables.\n")
  return(assumptions_results)
}

# Comprehensive normality testing with multiple methods
perform_comprehensive_normality_tests <- function(data, variables, group_column = NULL) {
  
  normality_results <- list()
  
  for (var in variables) {
    cat("Testing normality for", var, "...\n")
    
    var_data <- data[[var]][!is.na(data[[var]])]
    
    if (length(var_data) < 3) {
      normality_results[[var]] <- list(
        variable = var,
        overall_test = list(
          test = "insufficient_data",
          p_value = NA,
          normal = FALSE,
          borderline = FALSE,
          interpretation = "Insufficient data for normality testing"
        ),
        group_tests = NULL,
        descriptive_measures = NULL
      )
      next
    }
    
    # Overall normality test
    overall_normality <- perform_optimal_normality_test(var_data)
    
    # Group-wise normality tests (if group column specified)
    group_normality <- NULL
    if (!is.null(group_column)) {
      group_normality <- perform_group_normality_tests(data, var, group_column)
    }
    
    # Descriptive measures for normality assessment
    descriptive_measures <- calculate_normality_descriptives(var_data)
    
    normality_results[[var]] <- list(
      variable = var,
      overall_test = overall_normality,
      group_tests = group_normality,
      descriptive_measures = descriptive_measures
    )
  }
  
  return(normality_results)
}

# Optimal normality test selection based on sample size and data characteristics
perform_optimal_normality_test <- function(variable_data) {
  
  n <- length(variable_data)
  
  if (n < 3) {
    return(list(
      test = "insufficient_data",
      p_value = NA,
      normal = FALSE,
      borderline = FALSE,
      interpretation = "Insufficient data for normality testing"
    ))
  }
  
  # Choose appropriate test based on sample size and availability
  if (n <= 50) {
    # Shapiro-Wilk test for small samples (most powerful for small n)
    test_result <- shapiro.test(variable_data)
    test_name <- "Shapiro-Wilk"
  } else if (n <= 5000) {
    # Anderson-Darling test for medium samples (more powerful than KS)
    tryCatch({
      if (requireNamespace("nortest", quietly = TRUE)) {
        library(nortest)
        test_result <- nortest::ad.test(variable_data)
        test_name <- "Anderson-Darling"
      } else {
        # Fallback to Shapiro-Wilk for samples up to 5000
        test_result <- shapiro.test(variable_data)
        test_name <- "Shapiro-Wilk"
      }
    }, error = function(e) {
      test_result <- shapiro.test(variable_data)
      test_name <- "Shapiro-Wilk"
    })
  } else {
    # For very large samples, use subsample approach with Shapiro-Wilk
    sample_indices <- sample(1:n, 5000)
    test_result <- shapiro.test(variable_data[sample_indices])
    test_name <- "Shapiro-Wilk (subsample of 5000)"
  }
  
  # Task H: Enhanced normality classification with borderline flags
  p_value <- test_result$p.value
  is_normal <- p_value > 0.05
  is_borderline <- p_value > 0.01 & p_value <= 0.10  # Borderline range
  
  # Create detailed classification
  normality_flag <- determine_normality_flag(p_value)
  
  interpretation <- create_enhanced_normality_interpretation(test_name, p_value, normality_flag)
  
  return(list(
    test = test_name,
    statistic = test_result$statistic,
    p_value = p_value,
    normal = is_normal,
    borderline = is_borderline,
    normality_flag = normality_flag,
    interpretation = interpretation,
    sample_size = n
  ))
}

# Task H: Determine normality flag for automatic assumption flagging
determine_normality_flag <- function(p_value) {
  if (is.na(p_value)) {
    return("UNKNOWN")
  } else if (p_value > 0.10) {
    return("CLEARLY_NORMAL")
  } else if (p_value > 0.05) {
    return("NORMAL")
  } else if (p_value > 0.01) {
    return("BORDERLINE_NON_NORMAL")  # Task H: Key borderline flag
  } else if (p_value > 0.001) {
    return("NON_NORMAL")
  } else {
    return("CLEARLY_NON_NORMAL")
  }
}

# Task H: Create enhanced interpretation with borderline flags
create_enhanced_normality_interpretation <- function(test_name, p_value, normality_flag) {
  
  p_formatted <- format.pval(p_value, digits = 3)
  
  base_text <- paste0(test_name, " p = ", p_formatted)
  
  switch(normality_flag,
    "CLEARLY_NORMAL" = paste0("Clearly normal distribution (", base_text, ")"),
    "NORMAL" = paste0("Normal distribution (", base_text, ")"),
    "BORDERLINE_NON_NORMAL" = paste0("BORDERLINE normality - requires attention (", base_text, ")"),
    "NON_NORMAL" = paste0("Non-normal distribution (", base_text, ")"),
    "CLEARLY_NON_NORMAL" = paste0("Clearly non-normal distribution (", base_text, ")"),
    "UNKNOWN" = paste0("Unable to determine normality (", base_text, ")")
  )
}

# Group-wise normality testing
perform_group_normality_tests <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  group_results <- list()
  
  for (group in groups) {
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variable]
    group_data <- group_data[!is.na(group_data)]
    
    if (length(group_data) >= 3) {
      group_results[[as.character(group)]] <- perform_optimal_normality_test(group_data)
    } else {
      group_results[[as.character(group)]] <- list(
        test = "insufficient_data",
        p_value = NA,
        normal = FALSE,
        borderline = FALSE,
        interpretation = "Insufficient data for group normality testing"
      )
    }
  }
  
  return(group_results)
}

# Calculate descriptive measures for normality assessment
calculate_normality_descriptives <- function(variable_data) {
  
  if (length(variable_data) < 3) {
    return(NULL)
  }
  
  # Calculate skewness and kurtosis
  skewness_val <- calculate_skewness(variable_data)
  kurtosis_val <- calculate_kurtosis(variable_data)
  
  # Normality indicators based on descriptive measures
  skewness_normal <- abs(skewness_val) < 1.0  # Rule of thumb: |skewness| < 1
  kurtosis_normal <- abs(kurtosis_val) < 1.0   # Rule of thumb: |kurtosis| < 1 for normal
  
  return(list(
    skewness = skewness_val,
    kurtosis = kurtosis_val,
    skewness_interpretation = ifelse(skewness_normal, "Acceptable", 
                                   ifelse(abs(skewness_val) < 2, "Moderate", "Severe")),
    kurtosis_interpretation = ifelse(kurtosis_normal, "Acceptable",
                                   ifelse(abs(kurtosis_val) < 2, "Moderate", "Severe")),
    descriptive_normality = skewness_normal && kurtosis_normal
  ))
}

# Comprehensive homogeneity of variance testing
perform_comprehensive_homogeneity_tests <- function(data, variables, group_column) {
  
  if (is.null(group_column)) {
    return(NULL)
  }
  
  homogeneity_results <- list()
  
  for (var in variables) {
    cat("Testing homogeneity for", var, "...\n")
    
    # Prepare clean data
    clean_data <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
    
    if (nrow(clean_data) < 6) {
      homogeneity_results[[var]] <- list(
        variable = var,
        levene_test = list(
          test = "insufficient_data",
          p_value = NA,
          homogeneous = FALSE,
          interpretation = "Insufficient data for homogeneity testing"
        ),
        bartlett_test = NULL,
        fligner_test = NULL
      )
      next
    }
    
    # Levene's test (robust to non-normality)
    levene_result <- perform_levene_test(clean_data, var, group_column)
    
    # Bartlett's test (assumes normality but more powerful when assumptions met)
    bartlett_result <- perform_bartlett_test(clean_data, var, group_column)
    
    # Fligner-Killeen test (non-parametric alternative)
    fligner_result <- perform_fligner_test(clean_data, var, group_column)
    
    homogeneity_results[[var]] <- list(
      variable = var,
      levene_test = levene_result,
      bartlett_test = bartlett_result,
      fligner_test = fligner_result,
      recommendation = determine_homogeneity_recommendation(levene_result, bartlett_result, fligner_result)
    )
  }
  
  return(homogeneity_results)
}

# Levene's test implementation
perform_levene_test <- function(data, variable, group_column) {
  
  tryCatch({
    if (requireNamespace("car", quietly = TRUE)) {
      library(car)
      levene_result <- car::leveneTest(data[[variable]], data[[group_column]])
      
      is_homogeneous <- levene_result$`Pr(>F)`[1] > 0.05
      
      interpretation <- ifelse(is_homogeneous,
                                      paste("Variances appear homogeneous (Levene's test p =",
                                format.pval(levene_result$`Pr(>F)`[1], digits = 3), ")"),
        paste("Variances are heterogeneous (Levene's test p =",
                                format.pval(levene_result$`Pr(>F)`[1], digits = 3), ")"))
      
      return(list(
        test = "Levene",
        statistic = levene_result$`F value`[1],
        p_value = levene_result$`Pr(>F)`[1],
        homogeneous = is_homogeneous,
        interpretation = interpretation
      ))
    } else {
      return(list(
        test = "package_unavailable",
        p_value = NA,
        homogeneous = FALSE,
        interpretation = "car package not available for Levene's test"
      ))
    }
  }, error = function(e) {
    return(list(
      test = "error",
      p_value = NA,
      homogeneous = FALSE,
      interpretation = paste("Error in Levene's test:", e$message)
    ))
  })
}

# Bartlett's test implementation
perform_bartlett_test <- function(data, variable, group_column) {
  
  tryCatch({
    bartlett_result <- bartlett.test(data[[variable]], data[[group_column]])
    
    is_homogeneous <- bartlett_result$p.value > 0.05
    
    interpretation <- ifelse(is_homogeneous,
                                    paste("Variances appear homogeneous (Bartlett's test p =",
                                format.pval(bartlett_result$p.value, digits = 3), ")"),
        paste("Variances are heterogeneous (Bartlett's test p =",
                                format.pval(bartlett_result$p.value, digits = 3), ")"))
    
    return(list(
      test = "Bartlett",
      statistic = bartlett_result$statistic,
      p_value = bartlett_result$p.value,
      homogeneous = is_homogeneous,
      interpretation = interpretation
    ))
  }, error = function(e) {
    return(list(
      test = "error",
      p_value = NA,
      homogeneous = FALSE,
      interpretation = paste("Error in Bartlett's test:", e$message)
    ))
  })
}

# Fligner-Killeen test implementation
perform_fligner_test <- function(data, variable, group_column) {
  
  tryCatch({
    fligner_result <- fligner.test(data[[variable]], data[[group_column]])
    
    is_homogeneous <- fligner_result$p.value > 0.05
    
    interpretation <- ifelse(is_homogeneous,
                                    paste("Variances appear homogeneous (Fligner-Killeen test p =",
                                format.pval(fligner_result$p.value, digits = 3), ")"),
        paste("Variances are heterogeneous (Fligner-Killeen test p =",
                                format.pval(fligner_result$p.value, digits = 3), ")"))
    
    return(list(
      test = "Fligner-Killeen",
      statistic = fligner_result$statistic,
      p_value = fligner_result$p.value,
      homogeneous = is_homogeneous,
      interpretation = interpretation
    ))
  }, error = function(e) {
    return(list(
      test = "error",
      p_value = NA,
      homogeneous = FALSE,
      interpretation = paste("Error in Fligner-Killeen test:", e$message)
    ))
  })
}

# Determine homogeneity recommendation based on multiple tests
determine_homogeneity_recommendation <- function(levene_result, bartlett_result, fligner_result) {
  
  # TASK 4: SHOW REAL LEVENE STATISTICS & P-VALUES instead of generic "consensus"
  # Primary reliance on Levene's test (most robust)
  if (!is.null(levene_result) && !is.na(levene_result$p_value)) {
    primary_result <- paste0("Levene: F = ", round(levene_result$statistic, 3), 
                           ", p = ", format.pval(levene_result$p_value, digits = 3))
    
    # Add interpretation
    levene_interpretation <- if (levene_result$homogeneous) "homogeneous" else "heterogeneous"
    primary_result <- paste0(primary_result, " (", levene_interpretation, ")")
    
    # Add supporting evidence from other tests if available
    supporting_evidence <- c()
    
    if (!is.null(bartlett_result) && !is.na(bartlett_result$p_value)) {
      bartlett_interp <- if (bartlett_result$homogeneous) "homogeneous" else "heterogeneous"
      supporting_evidence <- c(supporting_evidence, 
                             paste0("Bartlett: χ² = ", round(bartlett_result$statistic, 3),
                                   ", p = ", format.pval(bartlett_result$p_value, digits = 3),
                                   " (", bartlett_interp, ")"))
    }
    
    if (!is.null(fligner_result) && !is.na(fligner_result$p_value)) {
      fligner_interp <- if (fligner_result$homogeneous) "homogeneous" else "heterogeneous"
      supporting_evidence <- c(supporting_evidence,
                             paste0("Fligner: χ² = ", round(fligner_result$statistic, 3),
                                   ", p = ", format.pval(fligner_result$p_value, digits = 3),
                                   " (", fligner_interp, ")"))
    }
    
    # Combine primary and supporting evidence
    if (length(supporting_evidence) > 0) {
      return(paste0(primary_result, "; ", paste(supporting_evidence, collapse = "; ")))
    } else {
      return(primary_result)
    }
  }
  
  # Fallback to original consensus approach if Levene not available
  homogeneous_count <- 0
  total_valid_tests <- 0
  
  if (!is.na(levene_result$p_value)) {
    total_valid_tests <- total_valid_tests + 1
    if (levene_result$homogeneous) homogeneous_count <- homogeneous_count + 1
  }
  
  if (!is.na(bartlett_result$p_value)) {
    total_valid_tests <- total_valid_tests + 1
    if (bartlett_result$homogeneous) homogeneous_count <- homogeneous_count + 1
  }
  
  if (!is.na(fligner_result$p_value)) {
    total_valid_tests <- total_valid_tests + 1
    if (fligner_result$homogeneous) homogeneous_count <- homogeneous_count + 1
  }
  
  if (total_valid_tests == 0) {
    return("Unable to determine - no valid tests")
  }
  
  consensus_ratio <- homogeneous_count / total_valid_tests
  
  if (consensus_ratio >= 0.67) {
    return("Homogeneous (consensus)")
  } else if (consensus_ratio >= 0.33) {
    return("Mixed results - proceed with caution")
  } else {
    return("Heterogeneous (consensus)")
  }
}

# Create comprehensive assumptions summary table
create_assumptions_summary_table <- function(normality_results, homogeneity_results, data, variables, group_column) {
  
  summary_rows <- list()
  
  for (var in variables) {
    # Basic variable information
    var_data <- data[[var]][!is.na(data[[var]])]
    n_total <- length(var_data)
    n_missing <- sum(is.na(data[[var]]))
    
    # Task H: Enhanced normality information with borderline flags
    norm_result <- normality_results[[var]]
    overall_normal <- norm_result$overall_test$normal
    overall_p <- norm_result$overall_test$p_value
    overall_flag <- norm_result$overall_test$normality_flag
    overall_borderline <- norm_result$overall_test$borderline
    
    # Group normality summary with borderline detection
    group_normal_summary <- "N/A"
    if (!is.null(norm_result$group_tests)) {
      group_normal_count <- sum(sapply(norm_result$group_tests, function(x) x$normal), na.rm = TRUE)
      group_borderline_count <- sum(sapply(norm_result$group_tests, function(x) x$borderline), na.rm = TRUE)
      total_groups <- length(norm_result$group_tests)
      
      if (group_borderline_count > 0) {
        group_normal_summary <- paste0(group_normal_count, "/", total_groups, " normal, ", 
                                     group_borderline_count, " borderline")
      } else {
        group_normal_summary <- paste0(group_normal_count, "/", total_groups, " groups normal")
      }
    }
    
    # Homogeneity information
    homogeneous <- "N/A"
    homogeneity_p <- NA
    if (!is.null(homogeneity_results) && !is.null(homogeneity_results[[var]])) {
      homo_result <- homogeneity_results[[var]]
      homogeneous <- homo_result$recommendation
      # Use Levene's test p-value as primary
      if (!is.na(homo_result$levene_test$p_value)) {
        homogeneity_p <- homo_result$levene_test$p_value
      }
    }
    
    # Descriptive measures
    skewness <- round(norm_result$descriptive_measures$skewness, 3)
    kurtosis <- round(norm_result$descriptive_measures$kurtosis, 3)
    
    summary_rows[[var]] <- data.frame(
      Variable = var,
      N_Total = n_total,
      N_Missing = n_missing,
      Overall_Normal = overall_normal,
      Normality_P = format.pval(overall_p, digits = 3),
      Normality_Flag = overall_flag,  # Task H: Add normality flag
      Borderline = overall_borderline,  # Task H: Add borderline indicator
      Group_Normality = group_normal_summary,
      Homogeneity = homogeneous,
      Homogeneity_P = format.pval(homogeneity_p, digits = 3),
      Skewness = skewness,
      Kurtosis = kurtosis,
      stringsAsFactors = FALSE
    )
  }
  
  summary_table <- do.call(rbind, summary_rows)
  return(summary_table)
}

# Task H: Enhanced test recommendations with borderline normality flags
generate_test_recommendations <- function(normality_results, homogeneity_results, data, variables, group_column) {
  
  if (is.null(group_column)) {
    return(list(message = "No group comparisons - test recommendations not applicable"))
  }
  
  recommendations <- list()
  
  for (var in variables) {
    norm_result <- normality_results[[var]]
    homo_result <- if (!is.null(homogeneity_results)) homogeneity_results[[var]] else NULL
    
    # Task H: Enhanced normality assessment with borderline flags
    overall_normal <- norm_result$overall_test$normal
    overall_borderline <- norm_result$overall_test$borderline
    overall_flag <- norm_result$overall_test$normality_flag
    
    # Group-wise analysis with borderline detection
    group_normal_count <- 0
    group_borderline_count <- 0
    total_groups <- 0
    group_flags <- c()
    
    if (!is.null(norm_result$group_tests)) {
      for (group_test in norm_result$group_tests) {
        total_groups <- total_groups + 1
        if (!is.na(group_test$normal) && group_test$normal) {
          group_normal_count <- group_normal_count + 1
        }
        if (!is.na(group_test$borderline) && group_test$borderline) {
          group_borderline_count <- group_borderline_count + 1
        }
        if (!is.null(group_test$normality_flag)) {
          group_flags <- c(group_flags, group_test$normality_flag)
        }
      }
    }
    
    mostly_normal <- (group_normal_count / total_groups) >= 0.7
    has_borderline <- overall_borderline || group_borderline_count > 0
    
    # Determine homogeneity status
    variances_homogeneous <- TRUE
    if (!is.null(homo_result)) {
      homogeneity_recommendation <- homo_result$recommendation
      variances_homogeneous <- grepl("Homogeneous", homogeneity_recommendation)
    }
    
    # Task H: Enhanced recommendation logic with borderline handling
    if (overall_normal && mostly_normal && variances_homogeneous && !has_borderline) {
      recommended_test <- "One-way ANOVA"
      post_hoc <- "Tukey HSD"
      rationale <- "Normal distribution and homogeneous variances"
      assumption_flag <- "CLEAR"
    } else if (overall_normal && mostly_normal && variances_homogeneous && has_borderline) {
      recommended_test <- "One-way ANOVA with sensitivity check"
      post_hoc <- "Tukey HSD + Kruskal-Wallis verification"
      rationale <- "Normal distribution with borderline cases - verify with non-parametric"
      assumption_flag <- "BORDERLINE_NORMAL"
    } else if ((overall_normal || mostly_normal) && !variances_homogeneous) {
      recommended_test <- "Welch's ANOVA"
      post_hoc <- "Games-Howell"
      rationale <- "Normal distribution but heterogeneous variances"
      assumption_flag <- "VARIANCE_VIOLATION"
    } else if (has_borderline && !overall_normal) {
      recommended_test <- "Kruskal-Wallis test (borderline normality)"
      post_hoc <- "Dunn's test with Bonferroni correction"
      rationale <- "Borderline normality detected - non-parametric approach recommended"
      assumption_flag <- "BORDERLINE_NON_NORMAL"
    } else {
      recommended_test <- "Kruskal-Wallis test"
      post_hoc <- "Dunn's test with Bonferroni correction"
      rationale <- "Non-normal distribution or other assumption violations"
      assumption_flag <- "NON_NORMAL"
    }
    
    recommendations[[var]] <- list(
      variable = var,
      primary_test = recommended_test,
      post_hoc_test = post_hoc,
      rationale = rationale,
      normality_met = overall_normal && mostly_normal,
      homogeneity_met = variances_homogeneous,
      sample_size = length(data[[var]][!is.na(data[[var]])]),
      # Task H: New borderline flags
      has_borderline = has_borderline,
      overall_flag = overall_flag,
      group_flags = group_flags,
      assumption_flag = assumption_flag,
      borderline_action = if (has_borderline) "Perform sensitivity analysis with both parametric and non-parametric tests" else "Standard analysis"
    )
  }
  
  return(recommendations)
}

