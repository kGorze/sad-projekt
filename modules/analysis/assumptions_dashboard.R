# Assumptions Dashboard Module
# Unified testing of statistical assumptions (normality, homogeneity of variance)
# Eliminates duplication across descriptive stats and comparative analysis modules

# Load required libraries with error handling
# NOTE: Packages are now loaded centrally in config.R - no individual loading needed

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
  
  is_normal <- test_result$p.value > 0.05
  
  interpretation <- ifelse(is_normal, 
                          paste("Data appears normally distributed (", test_name, " p =", 
                                round(test_result$p.value, 4), ")"),
                          paste("Data deviates from normal distribution (", test_name, " p =", 
                                round(test_result$p.value, 4), ")"))
  
  return(list(
    test = test_name,
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    normal = is_normal,
    interpretation = interpretation,
    sample_size = n
  ))
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
                                    round(levene_result$`Pr(>F)`[1], 4), ")"),
                              paste("Variances are heterogeneous (Levene's test p =", 
                                    round(levene_result$`Pr(>F)`[1], 4), ")"))
      
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
                                  round(bartlett_result$p.value, 4), ")"),
                            paste("Variances are heterogeneous (Bartlett's test p =", 
                                  round(bartlett_result$p.value, 4), ")"))
    
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
                                  round(fligner_result$p.value, 4), ")"),
                            paste("Variances are heterogeneous (Fligner-Killeen test p =", 
                                  round(fligner_result$p.value, 4), ")"))
    
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
  
  # Count how many tests suggest homogeneity
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
    
    # Normality information
    norm_result <- normality_results[[var]]
    overall_normal <- norm_result$overall_test$normal
    overall_p <- norm_result$overall_test$p_value
    
    # Group normality summary
    group_normal_summary <- "N/A"
    if (!is.null(norm_result$group_tests)) {
      group_normal_count <- sum(sapply(norm_result$group_tests, function(x) x$normal), na.rm = TRUE)
      total_groups <- length(norm_result$group_tests)
      group_normal_summary <- paste0(group_normal_count, "/", total_groups, " groups normal")
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
      Normality_P = round(overall_p, 4),
      Group_Normality = group_normal_summary,
      Homogeneity = homogeneous,
      Homogeneity_P = round(homogeneity_p, 4),
      Skewness = skewness,
      Kurtosis = kurtosis,
      stringsAsFactors = FALSE
    )
  }
  
  summary_table <- do.call(rbind, summary_rows)
  return(summary_table)
}

# Generate statistical test recommendations based on assumptions
generate_test_recommendations <- function(normality_results, homogeneity_results, data, variables, group_column) {
  
  if (is.null(group_column)) {
    return(list(message = "No group comparisons - test recommendations not applicable"))
  }
  
  recommendations <- list()
  
  for (var in variables) {
    norm_result <- normality_results[[var]]
    homo_result <- if (!is.null(homogeneity_results)) homogeneity_results[[var]] else NULL
    
    # Determine normality status
    overall_normal <- norm_result$overall_test$normal
    group_normal_count <- 0
    total_groups <- 0
    
    if (!is.null(norm_result$group_tests)) {
      group_normal_count <- sum(sapply(norm_result$group_tests, function(x) x$normal), na.rm = TRUE)
      total_groups <- length(norm_result$group_tests)
    }
    
    mostly_normal <- (group_normal_count / total_groups) >= 0.7
    
    # Determine homogeneity status
    variances_homogeneous <- TRUE
    if (!is.null(homo_result)) {
      homogeneity_recommendation <- homo_result$recommendation
      variances_homogeneous <- grepl("Homogeneous", homogeneity_recommendation)
    }
    
    # Generate recommendations
    if (overall_normal && mostly_normal && variances_homogeneous) {
      recommended_test <- "One-way ANOVA"
      post_hoc <- "Tukey HSD"
      rationale <- "Normal distribution and homogeneous variances"
    } else if ((overall_normal || mostly_normal) && !variances_homogeneous) {
      recommended_test <- "Welch's ANOVA"
      post_hoc <- "Games-Howell"
      rationale <- "Normal distribution but heterogeneous variances"
    } else {
      recommended_test <- "Kruskal-Wallis test"
      post_hoc <- "Dunn's test with Bonferroni correction"
      rationale <- "Non-normal distribution or other assumption violations"
    }
    
    recommendations[[var]] <- list(
      variable = var,
      primary_test = recommended_test,
      post_hoc_test = post_hoc,
      rationale = rationale,
      normality_met = overall_normal && mostly_normal,
      homogeneity_met = variances_homogeneous,
      sample_size = length(data[[var]][!is.na(data[[var]])])
    )
  }
  
  return(recommendations)
}

# Helper functions now loaded from modules/utils/statistical_helpers.R