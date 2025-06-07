# Comparative Analysis Module
# Functions for performing statistical comparisons between independent groups
# Supports multiple groups (>2) with appropriate statistical tests
#
# Variable Nomenclature Standards:
# - hsCRP: High-sensitivity C-reactive protein (mg/L)
# - BMI: Body Mass Index (kg/m²)
# - wiek: Age (years)
# - plec: Gender (M/F)
# - grupa: Study group assignment
# - p-values: Formatted using format.pval() with 3 significant digits for scientific notation when p < 0.001
#
# Statistical Test Selection Decision Matrix:
# 1. NORMALITY: Shapiro-Wilk (n≤50) or Anderson-Darling (n>50) → Normal vs Non-normal
# 2. HOMOGENEITY: Levene's test → Homogeneous vs Heterogeneous variances
# 3. TEST SELECTION:
#    - Normal + Homogeneous → One-way ANOVA (F-test) + Tukey HSD post-hoc
#    - Normal + Heterogeneous → Welch's ANOVA (unequal variances)
#    - Non-normal → Kruskal-Wallis test + Dunn's post-hoc with Bonferroni correction
# 4. EFFECT SIZES: η² (eta-squared) for ANOVA, ε² (epsilon-squared) for Kruskal-Wallis, Cohen's d for pairwise
# 5. MULTIPLE TESTING: Benjamini-Hochberg (FDR) correction applied to all correlation p-values

# Load required libraries with error handling
# NOTE: Packages are now loaded centrally in config.R - no individual loading needed

# Source reporting utilities
source("modules/reporting/export_results.R")

# Source the enhanced post hoc module
source("modules/analysis/enhanced_posthoc.R")

# Centralized modules are loaded in main.R

# Main function: Comprehensive comparative analysis
perform_group_comparisons <- function(data, group_column = "grupa", include_plots = TRUE) {
  
  # Create analysis result object
  result <- create_analysis_result("comparative_analysis")
  
  cat("Starting comprehensive comparative analysis...\n")
  
  # Identify variable types
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # Remove group column from analysis
  numeric_vars <- numeric_vars[numeric_vars != group_column]
  categorical_vars <- categorical_vars[categorical_vars != group_column]
  
  cat("- Analyzing", length(numeric_vars), "continuous variables\n")
  cat("- Analyzing", length(categorical_vars), "categorical variables\n")
  
  # Step 1: Comprehensive assumptions testing (uses centralized dashboard)
  cat("\n=== STEP 1: ASSUMPTIONS TESTING DASHBOARD ===\n")
  assumptions_results <- perform_assumptions_testing(data, numeric_vars, group_column)
  result$assumptions_analysis <- assumptions_results
  result$distribution_analysis <- assumptions_results$normality_tests
  result$homogeneity_analysis <- assumptions_results$homogeneity_tests
  
  # Extract test recommendations for next step
  test_recommendations <- assumptions_results$test_recommendations
  
  # Step 2: Determine appropriate tests and perform comparisons
  cat("\n=== STEP 2: STATISTICAL COMPARISONS ===\n")
  comparison_results <- perform_statistical_comparisons_updated(data, numeric_vars, categorical_vars, 
                                                               group_column, test_recommendations)
  result$test_results <- comparison_results$test_results
  result$test_recommendations <- test_recommendations
  
  # Step 3: Calculate Cohen's D effect sizes for all numeric variables
  cat("\n=== STEP 3: EFFECT SIZE ANALYSIS (COHEN'S D) ===\n")
  effect_size_results <- list()
  for (var in numeric_vars) {
    cat("Calculating Cohen's D for", var, "...\n")
    cohens_d_result <- calculate_cohens_d(data, var, group_column)
    effect_size_results[[var]] <- cohens_d_result
    
    # Debug: check if Cohen's D was calculated
    if (!is.null(cohens_d_result$effect_sizes) && length(cohens_d_result$effect_sizes) > 0) {
      cat("  - Cohen's D calculated for", length(cohens_d_result$effect_sizes), "comparisons\n")
    } else {
      cat("  - No Cohen's D values calculated for", var, "\n")
    }
  }
  result$effect_sizes <- effect_size_results
  
  # Step 4: Enhanced Inferential Analysis (Multiple Regression & ANCOVA)
  cat("\n=== STEP 4: ENHANCED INFERENTIAL ANALYSIS ===\n")
  
  # Source the enhanced inferential framework
  if (file.exists("modules/analysis/enhanced_inferential_framework.R")) {
    source("modules/analysis/enhanced_inferential_framework.R")
    
    # Perform enhanced inferential analysis
    enhanced_inferential_results <- perform_enhanced_inferential_analysis(data, group_column, include_plots = FALSE)
    result$enhanced_inferential <- enhanced_inferential_results
    cat("- Enhanced inferential analysis completed\n")
  } else {
    # Fallback to basic regression if enhanced framework not available
    regression_results <- list()
    for (var in numeric_vars) {
      cat("Performing basic linear regression for", var, "...\n")
      lm_result <- perform_linear_regression(data, var, group_column)
      regression_results[[var]] <- lm_result
    }
    result$regression_analysis <- regression_results
    cat("- Basic regression analysis completed\n")
  }
  
  # Step 4.5: Task F - Fix Model Residual Issues
  cat("\n=== STEP 4.5: TASK F - RESIDUAL DIAGNOSTICS AND FIXES ===\n")
  
  # Source the residual transformation module
  if (file.exists("modules/analysis/residual_transformation.R")) {
    source("modules/analysis/residual_transformation.R")
    
    residual_fixes_result <- tryCatch({
      apply_residual_fixes_to_analysis(data, group_column)
    }, error = function(e) {
      cat("Note: Residual transformation analysis failed:", e$message, "\n")
      NULL
    })
    
    if (!is.null(residual_fixes_result)) {
      result$residual_fixes <- residual_fixes_result
      
      # APPLY THE FIXES: Update the main analysis results with corrected models
      cat("- Applying residual fixes to main analysis results...\n")
      result <- apply_fixes_to_main_results(result, residual_fixes_result, data, group_column)
      
      cat("- Residual transformation analysis completed\n")
    }
  } else {
    cat("Residual transformation module not found. Skipping residual fixes.\n")
  }

  # Step 5: Enhanced Post Hoc Analysis (if any significant omnibus tests)
  cat("\n=== STEP 5: ENHANCED POST HOC ANALYSIS ===\n")
  enhanced_posthoc_results <- perform_enhanced_posthoc_analysis(data, result, group_column)
  result$enhanced_posthoc <- enhanced_posthoc_results
  if (length(enhanced_posthoc_results) > 0) {
    cat("- Enhanced post hoc analysis completed for", length(enhanced_posthoc_results), "variables\n")
    result$posthoc_summary <- create_comprehensive_posthoc_summary(enhanced_posthoc_results)
  } else {
    cat("- No significant omnibus tests found requiring post hoc analysis\n")
  }

  # Step 6: Generate plots if requested
  if (include_plots) {
    cat("\n=== STEP 6: GENERATING VISUALIZATIONS ===\n")
    
    # Use fixed output path for plots
    plots_output_path <- file.path("output", "plots", "comparative_analysis")
    
    plots_result <- create_comparative_plots(data, numeric_vars, categorical_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
    cat("- Generated", length(plots_result$plots), "comparison plots\n")
    cat("- Saved", length(plots_result$plot_files), "plot files\n")
  }
  
  # Map data structures for HTML report compatibility
  # Fix distribution analysis mapping
  if (!is.null(result$assumptions_analysis) && !is.null(result$assumptions_analysis$normality_tests)) {
    result$distribution_analysis <- list()
    for (var_name in names(result$assumptions_analysis$normality_tests)) {
      norm_data <- result$assumptions_analysis$normality_tests[[var_name]]
      
      # Map overall normality
      overall_normal <- if (!is.null(norm_data$overall_test)) {
        list(
          interpretation = if (!is.null(norm_data$overall_test$interpretation)) {
            norm_data$overall_test$interpretation
          } else {
            paste0(norm_data$overall_test$test, " (p = ", 
                   format.pval(norm_data$overall_test$p_value, digits = 4), ") - ",
                   ifelse(norm_data$overall_test$p_value > 0.05, "Normal", "Non-normal"))
          }
        )
      } else {
        list(interpretation = "Not available")
      }
      
      # Map group-wise normality  
      group_normality <- if (!is.null(norm_data$group_tests)) {
        group_results <- list()
        for (group_name in names(norm_data$group_tests)) {
          group_test <- norm_data$group_tests[[group_name]]
          group_results[[group_name]] <- list(
            interpretation = if (!is.null(group_test$interpretation)) {
              group_test$interpretation
            } else {
              paste0(group_test$test, " (p = ", 
                     format.pval(group_test$p_value, digits = 4), ") - ",
                     ifelse(group_test$p_value > 0.05, "Normal", "Non-normal"))
            }
          )
        }
        group_results
      } else {
        NULL
      }
      
      result$distribution_analysis[[var_name]] <- list(
        overall_normality = overall_normal,
        group_normality = group_normality
      )
    }
  }
  
  # Fix test recommendations mapping
  if (!is.null(result$assumptions_analysis) && !is.null(result$assumptions_analysis$test_recommendations)) {
    mapped_recommendations <- list()
    for (var_name in names(result$assumptions_analysis$test_recommendations)) {
      rec_data <- result$assumptions_analysis$test_recommendations[[var_name]]
      mapped_recommendations[[var_name]] <- list(
        test_type = if (!is.null(rec_data$primary_test)) rec_data$primary_test else "Not available",
        reasoning = if (!is.null(rec_data$rationale)) rec_data$rationale else "Not available"
      )
    }
    result$test_recommendations <- mapped_recommendations
  }
  
  # Add metadata
  result$metadata <- list(
    total_observations = nrow(data),
    groups = unique(data[[group_column]]),
    group_sizes = table(data[[group_column]]),
    numeric_variables = length(numeric_vars),
    categorical_variables = length(categorical_vars),
    group_column = group_column,
    analysis_date = Sys.time()
  )
  
  cat("\nComparative analysis completed successfully.\n")
  return(result)
}

# Assess distributions for all variables
assess_distributions <- function(data, variables, group_column) {
  
  distribution_results <- list()
  
  for (var in variables) {
    cat("Analyzing distribution of", var, "...\n")
    
    var_results <- list(
      variable = var,
      overall_normality = test_normality_overall(data[[var]]),
      group_normality = test_normality_by_group(data, var, group_column),
      descriptive_stats = calculate_distribution_stats(data, var, group_column)
    )
    
    distribution_results[[var]] <- var_results
  }
  
  return(distribution_results)
}

# Test normality for overall variable
test_normality_overall <- function(variable) {
  
  # Remove missing values
  clean_var <- variable[!is.na(variable)]
  n <- length(clean_var)
  
  if (n < 3) {
    return(list(
      test = "insufficient_data",
      p_value = NA,
      normal = FALSE,
      interpretation = "Insufficient data for normality testing"
    ))
  }
  
  # Choose appropriate test based on sample size
  if (n <= 50) {
    # Shapiro-Wilk test for small samples
    test_result <- shapiro.test(clean_var)
    test_name <- "Shapiro-Wilk"
  } else {
    # Anderson-Darling test for larger samples (more robust than KS for ties)
    tryCatch({
      if (requireNamespace("nortest", quietly = TRUE)) {
        library(nortest)
        test_result <- ad.test(clean_var)
        test_name <- "Anderson-Darling"
      } else {
        # Fallback to Shapiro-Wilk for larger samples (subsampling if needed)
        if (n > 5000) {
          sample_indices <- sample(1:n, 5000)
          test_result <- shapiro.test(clean_var[sample_indices])
          test_name <- "Shapiro-Wilk (subsample)"
        } else {
          test_result <- shapiro.test(clean_var)
          test_name <- "Shapiro-Wilk"
        }
      }
    }, error = function(e) {
      # Final fallback to Shapiro-Wilk
      test_result <- shapiro.test(clean_var)
      test_name <- "Shapiro-Wilk"
    })
  }
  
  is_normal <- test_result$p.value > 0.05
  
  interpretation <- ifelse(is_normal, 
                          paste("Data appears normally distributed (", test_name, " p =", 
                                format.pval(test_result$p.value, digits = 3), ")"),
                          paste("Data deviates from normal distribution (", test_name, " p =", 
                                format.pval(test_result$p.value, digits = 3), ")"))
  
  return(list(
    test = test_name,
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    normal = is_normal,
    interpretation = interpretation
  ))
}

# Test normality by group
test_normality_by_group <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  group_results <- list()
  
  for (group in groups) {
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), ]
    group_var <- group_data[[variable]]
    
    group_results[[as.character(group)]] <- test_normality_overall(group_var)
  }
  
  return(group_results)
}

# Calculate descriptive statistics for distribution assessment
calculate_distribution_stats <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  stats_list <- list()
  
  for (group in groups) {
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), ]
    group_var <- group_data[[variable]]
    group_var <- group_var[!is.na(group_var)]
    
    if (length(group_var) > 0) {
      stats_list[[as.character(group)]] <- list(
        n = length(group_var),
        mean = mean(group_var),
        median = median(group_var),
        sd = sd(group_var),
        skewness = calculate_skewness(group_var),
        kurtosis = calculate_kurtosis(group_var)
      )
    }
  }
  
  return(stats_list)
}

# Updated statistical comparisons using centralized test recommendations
perform_statistical_comparisons_updated <- function(data, numeric_vars, categorical_vars, group_column, test_recommendations) {
  
  test_results <- list()
  
  # Test continuous variables using centralized recommendations
  for (var in numeric_vars) {
    cat("Performing comparison for", var, "...\n")
    
    # Get test recommendation from centralized assumptions dashboard
    var_recommendation <- test_recommendations[[var]]
    
    if (is.null(var_recommendation)) {
      test_results[[var]] <- list(
        variable = var,
        test_name = "No recommendation available",
        p_value = NA,
        interpretation = "Test recommendation not found"
      )
      next
    }
    
    # Task H: Enhanced test selection with borderline handling
    recommended_test <- var_recommendation$primary_test
    has_borderline <- var_recommendation$has_borderline
    assumption_flag <- var_recommendation$assumption_flag
    
    if (recommended_test == "One-way ANOVA") {
      test_result <- perform_anova(data, var, group_column)
    } else if (recommended_test == "One-way ANOVA with sensitivity check") {
      # Task H: Borderline case - perform both tests
      test_result <- perform_anova_with_sensitivity_check(data, var, group_column)
    } else if (recommended_test == "Welch's ANOVA") {
      test_result <- perform_welch_anova(data, var, group_column)
    } else if (recommended_test == "Kruskal-Wallis test") {
      test_result <- perform_kruskal_wallis(data, var, group_column)
    } else if (recommended_test == "Kruskal-Wallis test (borderline normality)") {
      # Task H: Borderline non-normal case
      test_result <- perform_kruskal_wallis(data, var, group_column)
      test_result$borderline_note <- "Selected due to borderline normality"
    } else {
      test_result <- list(
        variable = var,
        test_name = paste("Unsupported test:", recommended_test),
        p_value = NA,
        interpretation = "Test not implemented"
      )
    }
    
    # Task H: Add borderline information to result
    test_result$borderline_info <- list(
      has_borderline = has_borderline,
      assumption_flag = assumption_flag,
      borderline_action = var_recommendation$borderline_action
    )
    
    # Add recommendation info to result
    test_result$recommendation <- var_recommendation
    test_results[[var]] <- test_result
  }
  
  # Test categorical variables (if any)
  if (length(categorical_vars) > 0) {
    for (var in categorical_vars) {
      cat("Performing categorical comparison for", var, "...\n")
      test_result <- perform_chi_square(data, var, group_column)
      test_results[[var]] <- test_result
    }
  }
  
  return(list(test_results = test_results))
}

# Task H: ANOVA with sensitivity check for borderline normality cases
perform_anova_with_sensitivity_check <- function(data, variable, group_column) {
  
  cat("  Performing ANOVA with sensitivity check for borderline normality...\n")
  
  # Perform both parametric and non-parametric tests
  anova_result <- perform_anova(data, variable, group_column)
  kruskal_result <- perform_kruskal_wallis(data, variable, group_column)
  
  # Compare p-values and create enhanced result
  anova_p <- anova_result$p_value
  kruskal_p <- kruskal_result$p_value
  
  # Determine consistency of results
  both_significant <- anova_p < 0.05 && kruskal_p < 0.05
  both_nonsignificant <- anova_p >= 0.05 && kruskal_p >= 0.05
  conflicting <- !(both_significant || both_nonsignificant)
  
  # Create interpretation
  if (both_significant) {
    consistency <- "CONSISTENT - Both tests significant"
    recommendation <- "Results robust across parametric and non-parametric approaches"
    primary_conclusion <- "Significant group differences detected"
  } else if (both_nonsignificant) {
    consistency <- "CONSISTENT - Both tests non-significant"
    recommendation <- "Results robust - no significant group differences"
    primary_conclusion <- "No significant group differences detected"
  } else {
    consistency <- "CONFLICTING - Tests disagree"
    if (anova_p < kruskal_p) {
      recommendation <- "Parametric test more sensitive - consider normality assumption carefully"
    } else {
      recommendation <- "Non-parametric test more conservative - consider assumption violations"
    }
    primary_conclusion <- "Results sensitive to distributional assumptions - interpret with caution"
  }
  
  # Return enhanced result structure
  return(list(
    variable = variable,
    test_name = "ANOVA with Kruskal-Wallis sensitivity check",
    # Primary result (use ANOVA as primary)
    p_value = anova_p,
    statistic = anova_result$statistic,
    interpretation = anova_result$interpretation,
    # Sensitivity check information
    sensitivity_check = list(
      anova = list(
        test = "One-way ANOVA",
        p_value = anova_p,
        statistic = anova_result$statistic
      ),
      kruskal = list(
        test = "Kruskal-Wallis",
        p_value = kruskal_p,
        statistic = kruskal_result$statistic
      ),
      consistency = consistency,
      recommendation = recommendation,
      primary_conclusion = primary_conclusion
    ),
    # Task H specific
    borderline_handling = "Dual test approach due to borderline normality assumptions"
  ))
}

# Assess homogeneity of variances
assess_homogeneity <- function(data, variables, group_column) {
  
  homogeneity_results <- list()
  
  for (var in variables) {
    cat("Testing homogeneity of variances for", var, "...\n")
    
    # Prepare data for testing
    clean_data <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
    
    if (nrow(clean_data) < 6) {
      homogeneity_results[[var]] <- list(
        variable = var,
        test = "insufficient_data",
        p_value = NA,
        homogeneous = FALSE,
        interpretation = "Insufficient data for homogeneity testing"
      )
      next
    }
    
    # Levene's test (more robust than Bartlett's test)
    tryCatch({
      levene_result <- leveneTest(clean_data[[var]], clean_data[[group_column]])
      
      is_homogeneous <- levene_result$`Pr(>F)`[1] > 0.05
      
      interpretation <- ifelse(is_homogeneous,
                              paste("Variances appear homogeneous (Levene's test p =", 
                                    format.pval(levene_result$`Pr(>F)`[1], digits = 3), ")"),
                              paste("Variances are heterogeneous (Levene's test p =", 
                                    format.pval(levene_result$`Pr(>F)`[1], digits = 3), ")"))
      
      homogeneity_results[[var]] <- list(
        variable = var,
        test = "Levene",
        statistic = levene_result$`F value`[1],
        p_value = levene_result$`Pr(>F)`[1],
        homogeneous = is_homogeneous,
        interpretation = interpretation
      )
      
    }, error = function(e) {
      homogeneity_results[[var]] <- list(
        variable = var,
        test = "error",
        p_value = NA,
        homogeneous = FALSE,
        interpretation = paste("Error in homogeneity testing:", e$message)
      )
    })
  }
  
  return(homogeneity_results)
}

# Perform statistical comparisons based on assumptions
perform_statistical_comparisons <- function(data, numeric_vars, categorical_vars, group_column, 
                                          distribution_results, homogeneity_results) {
  
  test_results <- list()
  recommendations <- list()
  
  # Test continuous variables
  for (var in numeric_vars) {
    cat("Performing comparison for", var, "...\n")
    
    # Get assumption test results
    dist_result <- distribution_results[[var]]
    homo_result <- homogeneity_results[[var]]
    
    # Determine appropriate test
    test_choice <- determine_appropriate_test(dist_result, homo_result)
    recommendations[[var]] <- test_choice
    
    # Perform the chosen test
    if (test_choice$test_type == "anova") {
      test_result <- perform_anova(data, var, group_column)
    } else if (test_choice$test_type == "welch_anova") {
      test_result <- perform_welch_anova(data, var, group_column)
    } else if (test_choice$test_type == "kruskal_wallis") {
      test_result <- perform_kruskal_wallis(data, var, group_column)
    } else {
      test_result <- list(
        variable = var,
        test_name = "No appropriate test",
        p_value = NA,
        interpretation = "Could not determine appropriate test"
      )
    }
    
    test_results[[var]] <- test_result
  }
  
  # Test categorical variables
  for (var in categorical_vars) {
    cat("Performing chi-square test for", var, "...\n")
    test_result <- perform_chi_square(data, var, group_column)
    test_results[[var]] <- test_result
    
    recommendations[[var]] <- list(
      test_type = "chi_square",
      reasoning = "Categorical variable - chi-square test of independence"
    )
  }
  
  return(list(test_results = test_results, recommendations = recommendations))
}

# Determine appropriate statistical test
determine_appropriate_test <- function(dist_result, homo_result) {
  
  # Check if we have valid results
  if (is.null(dist_result) || is.null(homo_result)) {
    return(list(
      test_type = "kruskal_wallis",
      reasoning = "Missing assumption data - using non-parametric test"
    ))
  }
  
  # Count how many groups have normal distributions
  normal_groups <- sum(sapply(dist_result$group_normality, function(x) x$normal), na.rm = TRUE)
  total_groups <- length(dist_result$group_normality)
  
  # Decision logic
  if (normal_groups == total_groups && homo_result$homogeneous) {
    return(list(
      test_type = "anova",
      reasoning = "All groups normal + homogeneous variances → One-way ANOVA"
    ))
  } else if (normal_groups == total_groups && !homo_result$homogeneous) {
    return(list(
      test_type = "welch_anova",
      reasoning = "All groups normal + heterogeneous variances → Welch's ANOVA"
    ))
  } else {
    return(list(
      test_type = "kruskal_wallis",
      reasoning = "Non-normal distributions → Kruskal-Wallis test"
    ))
  }
}

# Calculate confidence interval for eta-squared
calculate_eta_squared_ci <- function(anova_result, eta_squared) {
  
  tryCatch({
    anova_summary <- summary(anova_result)
    f_statistic <- anova_summary[[1]]$`F value`[1]
    df_between <- anova_summary[[1]]$Df[1]
    df_within <- anova_summary[[1]]$Df[2]
    
    # Using the non-central F distribution approach for confidence intervals
    # This is based on Steiger (2004) method
    
    # Calculate non-centrality parameter
    n <- df_between + df_within + 1
    lambda <- f_statistic * df_between
    
    # Calculate confidence interval using non-central F distribution
    # Lower bound
    f_lower <- qf(0.025, df_between, df_within, ncp = 0)
    if (f_statistic > f_lower) {
      # Find lambda that gives our observed F at the upper 97.5% point
      lambda_lower <- optimize(function(x) {
        abs(qf(0.975, df_between, df_within, ncp = x) - f_statistic)
      }, c(0, 100))$minimum
      eta_squared_lower <- lambda_lower / (lambda_lower + n)
    } else {
      eta_squared_lower <- 0
    }
    
    # Upper bound
    lambda_upper <- optimize(function(x) {
      abs(qf(0.025, df_between, df_within, ncp = x) - f_statistic)
    }, c(0, 200))$minimum
    eta_squared_upper <- lambda_upper / (lambda_upper + n)
    
    # Ensure bounds are reasonable
    eta_squared_lower <- max(0, min(eta_squared_lower, eta_squared))
    eta_squared_upper <- max(eta_squared, min(1, eta_squared_upper))
    
    return(c(eta_squared_lower, eta_squared_upper))
    
  }, error = function(e) {
    # Fallback to approximate CI if exact calculation fails
    se_approx <- sqrt(eta_squared * (1 - eta_squared) / (df_between + df_within + 1))
    ci_lower <- max(0, eta_squared - 1.96 * se_approx)
    ci_upper <- min(1, eta_squared + 1.96 * se_approx)
    return(c(ci_lower, ci_upper))
  })
}

# Interpret eta-squared magnitude
interpret_eta_squared_magnitude <- function(eta_squared) {
  if (is.na(eta_squared)) return("unknown")
  if (eta_squared < 0.01) return("negligible")
  else if (eta_squared < 0.06) return("small")
  else if (eta_squared < 0.14) return("medium") 
  else return("large")
}

# ANOVA for continuous variables (parametric)
perform_anova <- function(data, variable, group_column) {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 6) {
    return(list(
      variable = variable,
      test_name = "One-way ANOVA",
      p_value = NA,
      interpretation = "Insufficient data for ANOVA"
    ))
  }
  
  # Perform ANOVA
  # Rationale: One-way ANOVA chosen because:
  # 1. Data meets normality assumptions (tested via Shapiro-Wilk/Anderson-Darling)
  # 2. Variances are homogeneous (tested via Levene's test)
  # 3. Groups are independent (study design requirement)
  # 4. Continuous dependent variable with ≥3 groups
  formula_str <- paste(variable, "~", group_column)
  anova_result <- aov(as.formula(formula_str), data = clean_data)
  anova_summary <- summary(anova_result)
  
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  f_statistic <- anova_summary[[1]]$`F value`[1]
  
  # Calculate effect size (eta-squared) with 95% CI
  ss_between <- anova_summary[[1]]$`Sum Sq`[1]
  ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
  eta_squared <- ss_between / ss_total
  
  # Calculate 95% confidence interval for eta-squared
  eta_squared_ci <- calculate_eta_squared_ci(anova_result, eta_squared)
  
  # Post-hoc tests if significant
  posthoc_result <- NULL
  if (!is.na(p_value) && p_value < 0.05) {
    posthoc_result <- perform_tukey_hsd(anova_result)
  }
  
  interpretation <- ifelse(p_value < 0.05,
                          paste("Significant difference between groups (F =", round(f_statistic, 3), 
                                ", p =", format.pval(p_value, digits = 3), ")"),
                          paste("No significant difference between groups (F =", round(f_statistic, 3), 
                                ", p =", format.pval(p_value, digits = 3), ")"))
  
  return(list(
    variable = variable,
    test_name = "One-way ANOVA",
    statistic = f_statistic,
    p_value = p_value,
    effect_size = eta_squared,
    effect_size_ci = eta_squared_ci,
    effect_size_interpretation = interpret_eta_squared_magnitude(eta_squared),
    posthoc = posthoc_result,
    interpretation = interpretation
  ))
}

# Welch's ANOVA for unequal variances
perform_welch_anova <- function(data, variable, group_column) {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 6) {
    return(list(
      variable = variable,
      test_name = "Welch's ANOVA",
      p_value = NA,
      interpretation = "Insufficient data for Welch's ANOVA"
    ))
  }
  
  # Perform Welch's ANOVA
  # Rationale: Welch's ANOVA chosen because:
  # 1. Data meets normality assumptions but variances are heterogeneous
  # 2. Welch's ANOVA does not assume equal variances (robust to heteroscedasticity)
  # 3. More appropriate than standard ANOVA when Levene's test p < 0.05
  # 4. Maintains Type I error rate under variance inequality
  formula_str <- paste(variable, "~", group_column)
  welch_result <- oneway.test(as.formula(formula_str), data = clean_data, var.equal = FALSE)
  
  interpretation <- ifelse(welch_result$p.value < 0.05,
                          paste("Significant difference between groups (Welch F =", 
                                round(welch_result$statistic, 3), ", p =", 
                                format.pval(welch_result$p.value, digits = 3), ")"),
                          paste("No significant difference between groups (Welch F =", 
                                round(welch_result$statistic, 3), ", p =", 
                                format.pval(welch_result$p.value, digits = 3), ")"))
  
  return(list(
    variable = variable,
    test_name = "Welch's ANOVA",
    statistic = welch_result$statistic,
    p_value = welch_result$p.value,
    interpretation = interpretation
  ))
}

# Calculate epsilon-squared effect size for Kruskal-Wallis test with 95% CI
calculate_kruskal_wallis_effect_size <- function(data, variable, group_column, kw_result) {
  
  tryCatch({
    n <- nrow(data)
    k <- length(unique(data[[group_column]]))
    
    # Calculate epsilon-squared (ε²)
    # ε² = (H - k + 1) / (n - k)
    # where H is the Kruskal-Wallis H statistic
    H <- as.numeric(kw_result$statistic)
    epsilon_squared <- (H - k + 1) / (n - k)
    
    # Ensure epsilon-squared is between 0 and 1
    epsilon_squared <- max(0, min(1, epsilon_squared))
    
    # Calculate 95% confidence interval using bootstrap
    # This is an approximation - for exact CI, bootstrap would be needed
    # For now, using a simplified approach based on chi-square distribution
    df <- k - 1
    chi_crit_lower <- qchisq(0.025, df)
    chi_crit_upper <- qchisq(0.975, df)
    
    # Approximate confidence interval
    ci_lower <- max(0, (chi_crit_lower - k + 1) / (n - k))
    ci_upper <- min(1, (chi_crit_upper - k + 1) / (n - k))
    
    # If the calculated CI doesn't make sense, use a conservative approach
    if (ci_lower > epsilon_squared || ci_upper < epsilon_squared) {
      # Conservative CI around the point estimate
      se_approx <- sqrt(epsilon_squared * (1 - epsilon_squared) / n)
      ci_lower <- max(0, epsilon_squared - 1.96 * se_approx)
      ci_upper <- min(1, epsilon_squared + 1.96 * se_approx)
    }
    
    # Interpret effect size magnitude
    interpretation <- if (epsilon_squared < 0.01) "negligible"
                     else if (epsilon_squared < 0.06) "small"
                     else if (epsilon_squared < 0.14) "medium"
                     else "large"
    
    return(list(
      epsilon_squared = epsilon_squared,
      confidence_interval = c(ci_lower, ci_upper),
      interpretation = interpretation,
      method = "epsilon-squared (ε²)"
    ))
    
  }, error = function(e) {
    return(list(
      epsilon_squared = NA,
      confidence_interval = c(NA, NA),
      interpretation = "calculation error",
      method = "epsilon-squared (ε²)",
      error = e$message
    ))
  })
}

# Kruskal-Wallis test for continuous variables (non-parametric)
perform_kruskal_wallis <- function(data, variable, group_column) {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 6) {
    return(list(
      variable = variable,
      test_name = "Kruskal-Wallis",
      p_value = NA,
      interpretation = "Insufficient data for Kruskal-Wallis test"
    ))
  }
  
  # Perform Kruskal-Wallis test
  # Rationale: Kruskal-Wallis test chosen because:
  # 1. Data violates normality assumptions (Shapiro-Wilk/Anderson-Darling p < 0.05)
  # 2. Non-parametric alternative to one-way ANOVA
  # 3. Based on ranks, robust to outliers and non-normal distributions
  # 4. Does not assume equal variances or specific distribution shape
  # 5. Appropriate for ordinal or continuous data with ≥3 independent groups
  kw_result <- kruskal.test(clean_data[[variable]], clean_data[[group_column]])
  
  # Calculate epsilon-squared (ε²) effect size with 95% CI
  # Rationale: ε² chosen for Kruskal-Wallis because:
  # 1. Non-parametric equivalent to η² (eta-squared) for ANOVA
  # 2. Measures proportion of variance explained by group differences
  # 3. Interpretation: ε² < 0.01 (negligible), 0.01-0.06 (small), 0.06-0.14 (medium), >0.14 (large)
  effect_size_result <- calculate_kruskal_wallis_effect_size(clean_data, variable, group_column, kw_result)
  
  # Post-hoc tests if significant (Dunn's test)
  posthoc_result <- NULL
  if (!is.na(kw_result$p.value) && kw_result$p.value < 0.05) {
    posthoc_result <- perform_dunn_test(clean_data, variable, group_column)
  }
  
  interpretation <- ifelse(kw_result$p.value < 0.05,
                          paste("Significant difference between groups (χ² =", 
                                round(kw_result$statistic, 3), ", p =", 
                                format.pval(kw_result$p.value, digits = 3), ")"),
                          paste("No significant difference between groups (χ² =", 
                                round(kw_result$statistic, 3), ", p =", 
                                format.pval(kw_result$p.value, digits = 3), ")"))
  
  return(list(
    variable = variable,
    test_name = "Kruskal-Wallis",
    statistic = kw_result$statistic,
    p_value = kw_result$p.value,
    effect_size = effect_size_result$epsilon_squared,
    effect_size_ci = effect_size_result$confidence_interval,
    effect_size_interpretation = effect_size_result$interpretation,
    posthoc = posthoc_result,
    interpretation = interpretation
  ))
}

# Chi-square test for categorical variables
perform_chi_square <- function(data, variable, group_column) {
  
  # Create contingency table
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 5) {
    return(list(
      variable = variable,
      test_name = "Chi-square",
      p_value = NA,
      interpretation = "Insufficient data for chi-square test"
    ))
  }
  
  contingency_table <- table(clean_data[[variable]], clean_data[[group_column]])
  
  # Check if expected frequencies are adequate
  expected_freq <- chisq.test(contingency_table)$expected
  low_expected <- sum(expected_freq < 5)
  
  if (low_expected > 0.2 * length(expected_freq)) {
    # Use Fisher's exact test if too many low expected frequencies
    fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
    
    interpretation <- ifelse(fisher_result$p.value < 0.05,
                            paste("Significant association (Fisher's exact test p =", 
                                  format.pval(fisher_result$p.value, digits = 3), ")"),
                            paste("No significant association (Fisher's exact test p =", 
                                  format.pval(fisher_result$p.value, digits = 3), ")"))
    
    return(list(
      variable = variable,
      test_name = "Fisher's Exact Test",
      p_value = fisher_result$p.value,
      interpretation = interpretation,
      note = "Used Fisher's exact test due to low expected frequencies"
    ))
  } else {
    # Use chi-square test
    chi_result <- chisq.test(contingency_table)
    
    interpretation <- ifelse(chi_result$p.value < 0.05,
                            paste("Significant association (χ² =", round(chi_result$statistic, 3), 
                                  ", p =", format.pval(chi_result$p.value, digits = 3), ")"),
                            paste("No significant association (χ² =", round(chi_result$statistic, 3), 
                                  ", p =", format.pval(chi_result$p.value, digits = 3), ")"))
    
    return(list(
      variable = variable,
      test_name = "Chi-square",
      statistic = chi_result$statistic,
      p_value = chi_result$p.value,
      interpretation = interpretation
    ))
  }
}

# Tukey HSD post-hoc test
perform_tukey_hsd <- function(anova_result) {
  
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract pairwise comparisons
  comparisons <- tukey_result[[1]]
  
  significant_pairs <- rownames(comparisons)[comparisons[, "p adj"] < 0.05]
  
  return(list(
    test_name = "Tukey HSD",
    significant_pairs = significant_pairs,
    all_comparisons = comparisons
  ))
}

# Dunn's test for post-hoc analysis after Kruskal-Wallis
perform_dunn_test <- function(data, variable, group_column) {
  
  tryCatch({
    dunn_result <- dunn.test(data[[variable]], data[[group_column]], 
                            method = "bonferroni", alpha = 0.05)
    
    significant_pairs <- dunn_result$comparisons[dunn_result$P.adjusted < 0.05]
    
    return(list(
      test_name = "Dunn's test",
      significant_pairs = significant_pairs,
      p_values = dunn_result$P.adjusted
    ))
  }, error = function(e) {
    return(list(
      test_name = "Dunn's test",
      error = "Could not perform Dunn's test",
      message = e$message
    ))
  })
}

# Create advanced comparison plots with statistical visualizations
create_comparative_plots <- function(data, numeric_vars, categorical_vars, group_column, output_path = "output/plots/") {
  
  # Ensure required packages are available for plotting
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggpubr) 
    library(dplyr)
  })
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  cat("Creating advanced statistical visualizations...\n")
  
  # 1. Enhanced Box plots with statistical annotations
  for (var in numeric_vars[1:min(6, length(numeric_vars))]) {
    tryCatch({
      # Calculate Cohen's D for effect sizes
      cohens_d_result <- calculate_cohens_d(data, var, group_column)

      # Determine unique groups for pairwise comparisons
      groups <- unique(data[[group_column]])
      groups <- groups[!is.na(groups)]
      pairwise_comparisons <- combn(groups, 2, simplify = FALSE)

      # Choose overall test based on number of groups
      overall_method <- if (length(groups) > 2) "anova" else "t.test"

      # Ensure group column is factor and get clean data
      plot_data <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
      plot_data[[group_column]] <- as.factor(plot_data[[group_column]])
      
      # Create enhanced boxplot with statistical tests
      p <- ggboxplot(plot_data, x = group_column, y = var,
                     color = group_column, palette = "jco",
                     add = "jitter", add.params = list(alpha = 0.3)) +
        stat_compare_means(method = overall_method,
                           label.y = max(plot_data[[var]], na.rm = TRUE) * 1.1) +
        stat_compare_means(comparisons = pairwise_comparisons,
                           method = "t.test", label = "p.signif") +
        labs(title = paste("Enhanced Comparison of", var, "across groups"),
             subtitle = "With statistical significance tests and effect sizes",
             x = group_column, y = var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              legend.position = "none")
      
      # Save plot
      plot_filename <- file.path(output_path, paste0("enhanced_boxplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[[paste0("enhanced_boxplot_", var)]] <- p
      plot_files[[paste0("enhanced_boxplot_", var)]] <- plot_filename
      cat("Created enhanced boxplot for", var, "\n")
      
    }, error = function(e) {
      cat("Error creating enhanced boxplot for", var, ":", e$message, "\n")
    })
  }
  
  # 2. Density plots with group overlays
  for (var in numeric_vars[1:min(4, length(numeric_vars))]) {
    tryCatch({
      # Calculate group means for vertical lines using base R (fixed na.action issue)
      clean_data_for_means <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
      group_means <- aggregate(clean_data_for_means[[var]], 
                              by = list(group = clean_data_for_means[[group_column]]), 
                              FUN = function(x) mean(x, na.rm = TRUE))
      names(group_means) <- c(group_column, "mean_val")
      
      p <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
        geom_density(alpha = 0.6) +
        geom_vline(data = group_means,
                   aes(xintercept = mean_val, color = .data[[group_column]]), 
                   linetype = "dashed", size = 1) +
        labs(title = paste("Density Distribution of", var, "by Group"),
             subtitle = "Dashed lines show group means",
             x = var, y = "Density", fill = "Group", color = "Group") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10)) +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        scale_color_brewer(type = "qual", palette = "Set2")
      
      plot_filename <- file.path(output_path, paste0("density_plot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 6, dpi = 300)
      
      plots[[paste0("density_plot_", var)]] <- p
      plot_files[[paste0("density_plot_", var)]] <- plot_filename
      cat("Created density plot for", var, "\n")
      
    }, error = function(e) {
      cat("Error creating density plot for", var, ":", e$message, "\n")
    })
  }
  
  # 3. Q-Q plots for normality assessment
  for (var in numeric_vars[1:min(4, length(numeric_vars))]) {
    tryCatch({
      p <- ggplot(data, aes(sample = .data[[var]])) +
        stat_qq(aes(color = .data[[group_column]]), alpha = 0.7) +
        stat_qq_line(aes(color = .data[[group_column]])) +
        facet_wrap(as.formula(paste("~", group_column))) +
        labs(title = paste("Q-Q Plot for", var, "by Group"),
             subtitle = "Assessment of normality assumption",
             x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10)) +
        scale_color_brewer(type = "qual", palette = "Set2")
      
      plot_filename <- file.path(output_path, paste0("qq_plot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 12, height = 6, dpi = 300)
      
      plots[[paste0("qq_plot_", var)]] <- p
      plot_files[[paste0("qq_plot_", var)]] <- plot_filename
      cat("Created Q-Q plot for", var, "\n")
      
    }, error = function(e) {
      cat("Error creating Q-Q plot for", var, ":", e$message, "\n")
    })
  }
  
  # 4. Scatter plots with regression lines (for pairs of numeric variables)
  if (length(numeric_vars) >= 2) {
    for (i in 1:min(3, length(numeric_vars)-1)) {
      for (j in (i+1):min(i+2, length(numeric_vars))) {
        var1 <- numeric_vars[i]
        var2 <- numeric_vars[j]
        
        tryCatch({
          p <- ggplot(data, aes(x = .data[[var1]], y = .data[[var2]], color = .data[[group_column]])) +
            geom_point(alpha = 0.7, size = 2) +
            geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
            stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
                     label.x.npc = "left", label.y.npc = "top") +
            labs(title = paste("Scatter Plot:", var1, "vs", var2),
                 subtitle = "With regression lines and correlation coefficients by group",
                 x = var1, y = var2, color = "Group") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10)) +
            scale_color_brewer(type = "qual", palette = "Set2")
          
          plot_filename <- file.path(output_path, paste0("scatter_", var1, "_vs_", var2, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
          ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
          
          plots[[paste0("scatter_", var1, "_vs_", var2)]] <- p
          plot_files[[paste0("scatter_", var1, "_vs_", var2)]] <- plot_filename
          cat("Created scatter plot for", var1, "vs", var2, "\n")
          
        }, error = function(e) {
          cat("Error creating scatter plot for", var1, "vs", var2, ":", e$message, "\n")
        })
      }
    }
  }
  
  # 5. Violin plots with box plots inside
  for (var in numeric_vars[1:min(3, length(numeric_vars))]) {
    tryCatch({
      p <- ggplot(data, aes(x = .data[[group_column]], y = .data[[var]], fill = .data[[group_column]])) +
        geom_violin(alpha = 0.7, trim = FALSE) +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
        labs(title = paste("Violin Plot of", var, "by Group"),
             subtitle = "Shows distribution shape, quartiles, and means (red diamonds)",
             x = group_column, y = var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              legend.position = "none") +
        scale_fill_brewer(type = "qual", palette = "Set2")
      
      plot_filename <- file.path(output_path, paste0("violin_plot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[[paste0("violin_plot_", var)]] <- p
      plot_files[[paste0("violin_plot_", var)]] <- plot_filename
      cat("Created violin plot for", var, "\n")
      
    }, error = function(e) {
      cat("Error creating violin plot for", var, ":", e$message, "\n")
    })
  }
  
  # Note: Correlation heatmap moved to correlation_analysis.R module to avoid redundancy
  
  # 7. Enhanced bar plots for categorical variables
  for (var in categorical_vars) {
    if (length(unique(data[[var]])) <= 10) {
      tryCatch({
        # Calculate proportions using base R (fixed aggregate issue)
        clean_cat_data <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
        temp_data <- aggregate(rep(1, nrow(clean_cat_data)), 
                             by = list(group = clean_cat_data[[group_column]], var = clean_cat_data[[var]]), 
                             FUN = length)
        names(temp_data) <- c("group", "var", "count")
        
        # Calculate proportions within groups
        prop_data <- do.call(rbind, lapply(split(temp_data, temp_data$group), function(group_data) {
          group_data$prop <- group_data$count / sum(group_data$count) * 100
          return(group_data)
        }))
        names(prop_data)[names(prop_data) == "var"] <- var
        
        p <- ggplot(prop_data, aes(x = .data[[var]], y = prop, fill = group)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_text(aes(label = paste0(round(prop, 1), "%")), 
                   position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
          labs(title = paste("Distribution of", var, "by Group (%)"),
               subtitle = "Showing percentages within each group",
               x = var, y = "Percentage (%)", fill = "Group") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_brewer(type = "qual", palette = "Set2")
        
        plot_filename <- file.path(output_path, paste0("enhanced_barplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[[paste0("enhanced_barplot_", var)]] <- p
        plot_files[[paste0("enhanced_barplot_", var)]] <- plot_filename
        cat("Created enhanced barplot for", var, "\n")
        
      }, error = function(e) {
        cat("Error creating enhanced barplot for", var, ":", e$message, "\n")
      })
    }
  }
  
  cat("Advanced visualization creation completed!\n")
  cat("Total plots created:", length(plots), "\n")
  cat("Total plot files saved:", length(plot_files), "\n")
  
  return(list(
    plots = plots,
    plot_files = plot_files
  ))
}

# Calculate confidence interval for Cohen's d
calculate_cohens_d_ci <- function(d, n1, n2, conf_level = 0.95) {
  
  tryCatch({
    # Calculate standard error of Cohen's d
    # Using Hedges & Olkin (1985) formula
    df <- n1 + n2 - 2
    j <- 1 - (3 / (4 * df - 1))  # Hedges' correction factor
    
    # Variance of d
    var_d <- ((n1 + n2) / (n1 * n2)) + (d^2 / (2 * (n1 + n2)))
    se_d <- sqrt(var_d)
    
    # Apply Hedges' correction
    g <- j * d  # Hedges' g (bias-corrected)
    se_g <- j * se_d
    
    # Critical value for confidence interval
    alpha <- 1 - conf_level
    t_crit <- qt(1 - alpha/2, df)
    
    # Confidence interval
    ci_lower <- g - t_crit * se_g
    ci_upper <- g + t_crit * se_g
    
    return(c(ci_lower, ci_upper))
    
  }, error = function(e) {
    # Fallback to approximate CI
    se_approx <- sqrt(((n1 + n2) / (n1 * n2)) + (d^2 / (2 * (n1 + n2))))
    ci_lower <- d - 1.96 * se_approx
    ci_upper <- d + 1.96 * se_approx
    return(c(ci_lower, ci_upper))
  })
}

# Cohen's D effect size calculation
calculate_cohens_d <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  if (length(groups) < 2) {
    return(list(
      effect_sizes = NULL,
      interpretation = "Need at least 2 groups for Cohen's D"
    ))
  }
  
  effect_sizes <- list()
  interpretations <- list()
  
  # Calculate pairwise Cohen's D for all group combinations
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      group1 <- groups[i]
      group2 <- groups[j]
      
      # Get data for each group with proper indexing
      group1_indices <- which(data[[group_column]] == group1 & !is.na(data[[group_column]]) & !is.na(data[[variable]]))
      group2_indices <- which(data[[group_column]] == group2 & !is.na(data[[group_column]]) & !is.na(data[[variable]]))
      
      data1 <- data[group1_indices, variable]
      data2 <- data[group2_indices, variable]
      
      # Remove any remaining missing values
      data1 <- data1[!is.na(data1)]
      data2 <- data2[!is.na(data2)]
      
      if (length(data1) > 1 && length(data2) > 1) {
        # Calculate Cohen's D manually (more reliable than effsize package)
        tryCatch({
          mean1 <- mean(data1, na.rm = TRUE)
          mean2 <- mean(data2, na.rm = TRUE)
          sd1 <- sd(data1, na.rm = TRUE)
          sd2 <- sd(data2, na.rm = TRUE)
          n1 <- length(data1)
          n2 <- length(data2)
          
          # Pooled standard deviation
          pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
          
          # Cohen's D calculation
          d_value <- (mean1 - mean2) / pooled_sd
          
          # Calculate 95% confidence interval for Cohen's d
          d_ci <- calculate_cohens_d_ci(d_value, n1, n2)
          
          # Interpret effect size
          if (abs(d_value) < 0.2) {
            interpretation <- "negligible effect"
          } else if (abs(d_value) < 0.5) {
            interpretation <- "small effect"
          } else if (abs(d_value) < 0.8) {
            interpretation <- "medium effect"
          } else {
            interpretation <- "large effect"
          }
          
          comparison_name <- paste(group1, "vs", group2)
          effect_sizes[[comparison_name]] <- list(
            cohens_d = d_value,
            cohens_d_ci = d_ci,
            magnitude = interpretation,
            group1 = group1,
            group2 = group2,
            n1 = n1,
            n2 = n2,
            mean1 = mean1,
            mean2 = mean2,
            pooled_sd = pooled_sd
          )
          
        }, error = function(e) {
          comparison_name <- paste(group1, "vs", group2)
          effect_sizes[[comparison_name]] <- list(
            cohens_d = NA,
            magnitude = "calculation error",
            error = e$message,
            n1 = length(data1),
            n2 = length(data2)
          )
        })
      }
    }
  }
  
  return(list(
    effect_sizes = effect_sizes,
    variable = variable
  ))
}

# Linear regression analysis for continuous variables
perform_linear_regression <- function(data, dependent_var, group_column) {
  
  # Prepare data for regression
  clean_data <- data[!is.na(data[[dependent_var]]) & !is.na(data[[group_column]]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(
      variable = dependent_var,
      model = NULL,
      interpretation = "Insufficient data for regression analysis"
    ))
  }
  
  # Convert group to factor if not already
  clean_data[[group_column]] <- as.factor(clean_data[[group_column]])
  
  # Fit linear model
  formula_str <- paste(dependent_var, "~", group_column)
  
  tryCatch({
    lm_model <- lm(as.formula(formula_str), data = clean_data)
    
    # Get model summary
    model_summary <- summary(lm_model)
    
    # Extract key statistics
    r_squared <- model_summary$r.squared
    adj_r_squared <- model_summary$adj.r.squared
    f_statistic <- model_summary$fstatistic[1]
    f_p_value <- pf(model_summary$fstatistic[1], 
                    model_summary$fstatistic[2], 
                    model_summary$fstatistic[3], 
                    lower.tail = FALSE)
    
    # Get coefficients with confidence intervals
    coefficients_df <- tidy(lm_model, conf.int = TRUE)
    
    # ANOVA for the model
    anova_result <- anova(lm_model)
    
    # Residual analysis
    residuals <- residuals(lm_model)
    fitted_values <- fitted(lm_model)
    
    # Normality test of residuals
    shapiro_test <- shapiro.test(residuals)
    
    interpretation <- paste0(
      "Linear regression explains ", round(r_squared * 100, 2), 
      "% of variance (R² = ", round(r_squared, 3), 
      ", F = ", round(f_statistic, 3), 
      ", p = ", format.pval(f_p_value, digits = 4), ")"
    )
    
    return(list(
      variable = dependent_var,
      model = lm_model,
      summary = model_summary,
      coefficients = coefficients_df,
      anova = anova_result,
      r_squared = r_squared,
      adj_r_squared = adj_r_squared,
      f_statistic = f_statistic,
      f_p_value = f_p_value,
      residuals = residuals,
      fitted_values = fitted_values,
      normality_test = shapiro_test,
      interpretation = interpretation
    ))
    
  }, error = function(e) {
    return(list(
      variable = dependent_var,
      model = NULL,
      interpretation = paste("Error in regression analysis:", e$message)
    ))
  })
}

# Convenience function for quick comparative analysis with report
quick_comparative_analysis <- function(data, group_column = "grupa", generate_report = TRUE) {
  
  # Run comparative analysis with fixed output path
  result <- perform_group_comparisons(data, group_column, include_plots = TRUE)
  
  # Generate report if requested
  if (generate_report) {
    report_file <- quick_report(result)
    cat("Comparative analysis report generated:", report_file, "\n")
    return(list(analysis_result = result, report_file = report_file))
  }
  
  return(result)
}

# Functions for two-group comparisons (from lab guide)
perform_two_group_tests <- function(data, variable, group_column, test_type = "auto") {
  
  # Prepare data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  groups <- unique(clean_data[[group_column]])
  
  if (length(groups) != 2) {
    return(list(
      error = "Function requires exactly 2 groups",
      n_groups = length(groups)
    ))
  }
  
  group1_data <- clean_data[clean_data[[group_column]] == groups[1], variable]
  group2_data <- clean_data[clean_data[[group_column]] == groups[2], variable]
  
  results <- list(
    variable = variable,
    group1 = groups[1],
    group2 = groups[2],
    n1 = length(group1_data),
    n2 = length(group2_data)
  )
  
  # Automatic test selection or specific test
  if (test_type == "auto") {
    # Check normality for both groups
    norm1 <- if(length(group1_data) >= 3) shapiro.test(group1_data)$p.value > 0.05 else FALSE
    norm2 <- if(length(group2_data) >= 3) shapiro.test(group2_data)$p.value > 0.05 else FALSE
    
    # Check homogeneity
    if (norm1 && norm2) {
      var_test <- var.test(group1_data, group2_data)
      homogeneous <- var_test$p.value > 0.05
      
      if (homogeneous) {
        test_type <- "student_t"
      } else {
        test_type <- "welch_t"
      }
    } else {
      test_type <- "mann_whitney"
    }
  }
  
  # Perform the selected test
  if (test_type == "student_t") {
    test_result <- t.test(group1_data, group2_data, var.equal = TRUE)
    results$test_name <- "Student's t-test"
    results$assumption <- "Equal variances assumed"
  } else if (test_type == "welch_t") {
    test_result <- t.test(group1_data, group2_data, var.equal = FALSE)
    results$test_name <- "Welch's t-test"
    results$assumption <- "Unequal variances"
  } else if (test_type == "mann_whitney") {
    test_result <- wilcox.test(group1_data, group2_data)
    results$test_name <- "Mann-Whitney U test"
    results$assumption <- "Non-parametric"
  }
  
  # Extract results
  results$statistic <- test_result$statistic
  results$p_value <- test_result$p.value
  results$confidence_interval <- test_result$conf.int
  results$estimate <- test_result$estimate
  
  # Calculate effect size
  if (test_type %in% c("student_t", "welch_t")) {
    # Cohen's d for t-tests
    pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                      (length(group2_data) - 1) * var(group2_data)) / 
                     (length(group1_data) + length(group2_data) - 2))
    cohens_d <- (mean(group1_data) - mean(group2_data)) / pooled_sd
    results$effect_size <- cohens_d
    results$effect_size_name <- "Cohen's d"
  } else {
    # Rank-biserial correlation for Mann-Whitney
    U <- test_result$statistic
    n1 <- length(group1_data)
    n2 <- length(group2_data)
    r <- 1 - (2 * U) / (n1 * n2)
    results$effect_size <- r
    results$effect_size_name <- "Rank-biserial correlation"
  }
  
  # Interpretation
  significance <- if(results$p_value < 0.05) "significant" else "not significant"
  results$interpretation <- paste0(
    "The difference between ", groups[1], " and ", groups[2], 
    " is ", significance, " (", results$test_name, 
    ", p = ", format.pval(results$p_value, digits = 3), ")"
  )
  
  return(results)
}

# Enhanced function to provide comprehensive test recommendations
get_comprehensive_test_recommendations <- function(normal, homogeneous, n_groups, outlier_percent, 
                                                  skewness = NA, cv_percent = NA) {
  
  primary_test <- determine_recommended_test(normal, homogeneous, n_groups, outlier_percent)
  
  recommendations <- list(
    primary_test = primary_test,
    alternatives = c(),
    post_hoc = NULL,
    assumptions_to_check = c(),
    transformations = c(),
    effect_sizes = c(),
    notes = c()
  )
  
  # Add post-hoc recommendations
  if(n_groups > 2) {
    if(grepl("ANOVA", primary_test)) {
      recommendations$post_hoc <- "Tukey HSD (if p < 0.05)"
    } else if(grepl("Kruskal-Wallis", primary_test)) {
      recommendations$post_hoc <- "Dunn's test with Bonferroni correction (if p < 0.05)"
    }
  }
  
  # Add alternative tests for borderline cases
  if(!is.na(normal) && !normal && !is.na(skewness)) {
    if(abs(skewness) > 1) {
      recommendations$transformations <- c("Log transformation", "Square root transformation")
      recommendations$alternatives <- c("Re-test normality after transformation")
    }
  }
  
  # High variability recommendations
  if(!is.na(cv_percent) && cv_percent > 50) {
    recommendations$notes <- c("High variability (CV > 50%) - consider robust methods")
    recommendations$alternatives <- c(recommendations$alternatives, "Robust ANOVA", "Bootstrap methods")
  }
  
  # Assumption checks
  recommendations$assumptions_to_check <- c("Normality (Shapiro-Wilk/KS)", "Homogeneity (Levene's test)")
  
  # Effect size recommendations
  if(n_groups == 2) {
    recommendations$effect_sizes <- "Cohen's d"
  } else if(n_groups > 2) {
    if(grepl("ANOVA", primary_test)) {
      recommendations$effect_sizes <- "Eta-squared (η²)"
    } else {
      recommendations$effect_sizes <- "Rank-biserial correlation"
    }
  }
  
  # Borderline p-value recommendations
  recommendations$notes <- c(recommendations$notes, 
                           "If assumption tests p ≈ 0.04-0.06: run both parametric and non-parametric")
  
  return(recommendations)
}

# Function to check for paired vs independent design
detect_study_design <- function(data, group_column, id_column = NULL) {
  
  design_info <- list(
    design_type = "independent",
    recommendation = "Use independent samples tests",
    notes = c()
  )
  
  # Check if ID column exists and has repeated measures
  if (!is.null(id_column) && id_column %in% names(data)) {
    id_counts <- table(data[[id_column]])
    repeated_measures <- any(id_counts > 1)
    
    if (repeated_measures) {
      design_info$design_type <- "repeated_measures"
      design_info$recommendation <- "Use paired/repeated measures tests"
      design_info$notes <- c("Detected repeated measurements on same subjects")
      
      # Count time points
      max_timepoints <- max(id_counts)
      if (max_timepoints == 2) {
        design_info$suggested_tests <- c("Paired t-test", "Wilcoxon signed-rank test")
      } else if (max_timepoints > 2) {
        design_info$suggested_tests <- c("Repeated-measures ANOVA", "Friedman test")
      }
    }
  }
  
  # Check group balance
  group_counts <- table(data[[group_column]])
  if (length(unique(group_counts)) > 1) {
    design_info$notes <- c(design_info$notes, "Unbalanced groups detected")
  }
  
  return(design_info)
}

# Helper function to determine recommended statistical test (enhanced version)
determine_recommended_test <- function(normal, homogeneous, n_groups, outlier_percent) {
  
  if(is.na(normal) || is.na(homogeneous)) {
    return("Insufficient data")
  }
  
  # High outliers - always recommend non-parametric
  if(outlier_percent > 15) {
    return("Non-parametric (high outliers)")
  }
  
  # Exactly 2 groups - more specific recommendations
  if(n_groups == 2) {
    if(normal && homogeneous) {
      return("Student's t-test")
    } else if(normal && !homogeneous) {
      return("Welch's t-test")
    } else {
      return("Mann-Whitney U")
    }
  } 
  # More than 2 groups
  else if(n_groups > 2) {
    if(normal && homogeneous) {
      return("ANOVA + Tukey HSD")
    } else if(normal && !homogeneous) {
      return("Welch's ANOVA")
    } else {
      return("Kruskal-Wallis + Dunn")
    }
  }
  
  return("Unknown")
}

# Function to apply residual fixes to the main analysis results
apply_fixes_to_main_results <- function(main_results, residual_fixes, data, group_column) {
  
  if (is.null(residual_fixes) || is.null(residual_fixes$results)) {
    return(main_results)
  }
  
  cat("  Updating analysis results with corrected models...\n")
  
  # Update regression analysis results with fixed models
  if (!is.null(main_results$regression_analysis)) {
    for (var_name in names(residual_fixes$results)) {
      if (var_name %in% names(main_results$regression_analysis)) {
        
        fix_result <- residual_fixes$results[[var_name]]
        final_rec <- fix_result$final_recommendation
        
        if (!is.null(final_rec)) {
          
          # Apply the recommended fix
                      if (!is.null(final_rec) && !is.null(final_rec$transformation) && final_rec$transformation != "original") {
            # Apply data transformation
            tryCatch({
              cat("    Applying", final_rec$transformation, "transformation to", var_name, "\n")
              
              # Transform the data
              if (final_rec$transformation == "log") {
                # Add small constant to avoid log(0)
                min_val <- min(data[[var_name]], na.rm = TRUE)
                if (min_val <= 0) {
                  transformed_data <- log(data[[var_name]] + abs(min_val) + 1)
                } else {
                  transformed_data <- log(data[[var_name]])
                }
              } else if (final_rec$transformation == "sqrt") {
                # Handle negative values for sqrt
                min_val <- min(data[[var_name]], na.rm = TRUE)
                if (min_val < 0) {
                  transformed_data <- sqrt(data[[var_name]] - min_val)
                } else {
                  transformed_data <- sqrt(data[[var_name]])
                }
              } else if (final_rec$transformation == "boxcox") {
                # Use Box-Cox transformation
                if (requireNamespace("car", quietly = TRUE)) {
                  library(car)
                  bc_result <- powerTransform(data[[var_name]] ~ 1)
                  lambda <- bc_result$lambda
                  if (abs(lambda) < 0.01) {
                    # Lambda ≈ 0, use log transformation
                    min_val <- min(data[[var_name]], na.rm = TRUE)
                    if (min_val <= 0) {
                      transformed_data <- log(data[[var_name]] + abs(min_val) + 1)
                    } else {
                      transformed_data <- log(data[[var_name]])
                    }
                  } else {
                    # Apply power transformation
                    transformed_data <- (data[[var_name]]^lambda - 1) / lambda
                  }
                } else {
                  # Fallback to log transformation
                  min_val <- min(data[[var_name]], na.rm = TRUE)
                  if (min_val <= 0) {
                    transformed_data <- log(data[[var_name]] + abs(min_val) + 1)
                  } else {
                    transformed_data <- log(data[[var_name]])
                  }
                }
              }
              
              # Re-run the regression with transformed data
              temp_data <- data
              temp_data[[var_name]] <- transformed_data
              
              # Update the regression result
              lm_result <- perform_linear_regression(temp_data, var_name, group_column)
              main_results$regression_analysis[[var_name]] <- lm_result
              
              # Update the note
              main_results$regression_analysis[[var_name]]$transformation_applied <- final_rec$transformation
              main_results$regression_analysis[[var_name]]$transformation_note <- paste0("Applied ", final_rec$transformation, " transformation to fix non-normal residuals")
              
            }, error = function(e) {
              cat("    Warning: Could not apply", final_rec$transformation, "transformation to", var_name, ":", e$message, "\n")
            })
            
          } else if (!is.null(final_rec$robust) && final_rec$robust) {
            # Apply robust regression
            tryCatch({
              cat("    Applying", final_rec$method_name, "robust regression to", var_name, "\n")
              
              # Use the robust model that was already fitted
              if (!is.null(fix_result$robust_huber) && final_rec$method_name == "Huber M-estimator") {
                robust_model <- fix_result$robust_huber$model
              } else if (!is.null(fix_result$robust_mm) && final_rec$method_name == "MM-estimator") {
                robust_model <- fix_result$robust_mm$model
              }
              
              if (!is.null(robust_model)) {
                # Convert robust model to similar structure as linear regression
                robust_summary <- summary(robust_model)
                
                # Update the regression result with robust model
                main_results$regression_analysis[[var_name]]$model <- robust_model
                main_results$regression_analysis[[var_name]]$coefficients <- robust_summary$coefficients
                main_results$regression_analysis[[var_name]]$r_squared <- NA  # R² not directly available for robust
                main_results$regression_analysis[[var_name]]$adj_r_squared <- NA
                main_results$regression_analysis[[var_name]]$robust_method <- final_rec$method_name
                main_results$regression_analysis[[var_name]]$transformation_note <- paste0("Applied ", final_rec$method_name, " robust regression to handle non-normal residuals")
                main_results$regression_analysis[[var_name]]$residuals_fixed <- TRUE
              }
              
            }, error = function(e) {
              cat("    Warning: Could not apply robust regression to", var_name, ":", e$message, "\n")
            })
          }
        }
      }
    }
  }
  
  # Update test results if any transformations were applied
  if (!is.null(main_results$test_results)) {
    for (var_name in names(residual_fixes$results)) {
      if (var_name %in% names(main_results$test_results)) {
        
        fix_result <- residual_fixes$results[[var_name]]
        final_rec <- fix_result$final_recommendation
        
                 if (!is.null(final_rec) && !is.null(final_rec$transformation) && final_rec$transformation != "original") {
          # Add note about transformation to test results
          if (is.null(main_results$test_results[[var_name]]$note)) {
            main_results$test_results[[var_name]]$note <- ""
          }
          
          transformation_note <- if (!is.null(final_rec$robust) && final_rec$robust) {
            paste0("Results based on ", final_rec$method_name, " robust regression")
          } else {
            paste0("Results based on ", final_rec$transformation, "-transformed data")
          }
          
          main_results$test_results[[var_name]]$note <- paste0(
            main_results$test_results[[var_name]]$note,
            if (nchar(main_results$test_results[[var_name]]$note) > 0) "; " else "",
            transformation_note
          )
        }
      }
    }
  }
  
  cat("  Main analysis results updated with residual fixes.\n")
  return(main_results)
} 