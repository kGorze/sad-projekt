# Data preprocessing and quality improvement

# Advanced data repair package loading
load_packages_safely <- function() {
  # Use centralized package loading from config.R
  return(load_required_packages(c("VIM", "mice", "Hmisc"), quiet = TRUE))
}

# Load packages at startup
has_advanced_packages <- load_packages_safely()

# Main data repair function
repair_dataset <- function(data, missing_threshold = 0.1, outlier_method = "iqr", 
                          missing_method = "regression", outlier_action = "winsorize") {
  
  cat("\n=== STARTING DATA REPAIR PROCESS ===\n")
  
  # Store original data for comparison
  original_data <- data
  repair_log <- list()
  
  # Step 1: Missing data analysis
  cat("\n1. MISSING DATA ANALYSIS\n")
  cat("======================================\n")
  
  missing_analysis <- analyze_missing_patterns(data)
  repair_log$missing_analysis <- missing_analysis
  
      # Step 2: Missing data handling
  if (any(missing_analysis$missing_percentages > missing_threshold * 100) || 
      (missing_threshold == 0 && sum(missing_analysis$missing_counts) > 0)) {
    
    # Task G: If missing_threshold is 0, perform sensitivity analysis
    if (missing_threshold == 0 && sum(missing_analysis$missing_counts) > 0) {
      cat("\n2. TASK G: MISSING DATA SENSITIVITY ANALYSIS\n")
      cat("===========================================\n")
      
      # Perform multiple imputation sensitivity analysis
      sensitivity_results <- perform_missing_data_sensitivity_analysis(data, missing_analysis)
      repair_log$sensitivity_analysis <- sensitivity_results
      
      # Use the best performing method
      data <- sensitivity_results$recommended_data
      repair_log$missing_treatment <- list(
        method = sensitivity_results$recommended_method,
        columns_imputed = names(which(missing_analysis$missing_counts > 0)),
        sensitivity_results = sensitivity_results$comparison,
        recommendations_followed = missing_analysis$recommendations
      )
      
    } else {
      cat("\n2. MISSING DATA HANDLING\n")
      cat("====================================\n")
      
      # Choose method based on analysis
      chosen_method <- choose_optimal_imputation_method(missing_analysis, missing_method)
      cat(sprintf("Selected imputation method: %s\n", chosen_method))
      
      data_imputed <- handle_missing_data(data, method = chosen_method, threshold = missing_threshold)
      repair_log$missing_treatment <- list(
        method = chosen_method,
        columns_imputed = names(which(missing_analysis$missing_counts > 0)),
        recommendations_followed = missing_analysis$recommendations
      )
      data <- data_imputed$data
      repair_log$imputation_details <- data_imputed$details
    }
  } else {
    cat("\n2. MISSING DATA CHECK\n")
    cat("====================\n")
    cat("Missing data below threshold - minimal intervention needed\n")
  }
  
  # Step 3: Detect outliers
  cat("\n3. DETECTING OUTLIERS\n")
  cat("=====================\n")
  
  outlier_results <- detect_outliers(data, method = outlier_method)
  repair_log$outlier_detection <- outlier_results
  
  # Step 4: Handle outliers
  if (length(outlier_results$outliers) > 0) {
    cat("\n4. HANDLING OUTLIERS\n")
    cat("====================\n")
    
    data <- handle_outliers(data, outlier_results$outliers, method = outlier_action)
    repair_log$outlier_treatment <- list(
      method = outlier_action,
      outliers_treated = length(outlier_results$outliers)
    )
  } else {
    cat("\n4. OUTLIER CHECK\n")
    cat("================\n")
    cat("No significant outliers detected\n")
  }
  
  # Step 5: Final validation
  cat("\n5. FINAL DATA VALIDATION\n")
  cat("========================\n")
  
  final_validation <- validate_repaired_data(data)
  repair_log$final_validation <- final_validation
  
  # Step 6: Generate comprehensive report
  repair_report <- generate_cleaning_report(original_data, data, repair_log)
  
  cat("\n=== DATA REPAIR COMPLETED ===\n")
  
  return(list(
    data = data,
    repair_log = repair_log,
    repair_report = repair_report
  ))
}

# Analyze missing data patterns
analyze_missing_patterns <- function(data) {
  cat("=== MISSING DATA ANALYSIS ===\n")
  
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_percentages <- round(100 * missing_counts / nrow(data), 2)
  
  # Basic missing data summary
  cat("\nMissing data summary:\n")
  for (col in names(missing_counts)) {
    if (missing_counts[col] > 0) {
      cat(sprintf("- %-10s: %3d missing (%.1f%%)\n", 
                  col, missing_counts[col], missing_percentages[col]))
    }
  }
  
  if (sum(missing_counts) == 0) {
    cat("No missing values detected.\n")
    return(list(
      missing_counts = missing_counts,
      missing_percentages = missing_percentages,
      total_missing = 0,
      pattern_analysis = "No missing data",
      mechanism_assessment = "N/A",
      recommendations = "No action needed"
    ))
  }
  
  # 1. MISSING DATA PATTERN ANALYSIS
  cat("\n1. MISSING DATA PATTERN ANALYSIS\n")
  cat("--------------------------------\n")
  
  pattern_analysis <- detect_missing_patterns(data)
  
  # 2. MISSING DATA MECHANISM ASSESSMENT
  cat("\n2. MISSING DATA MECHANISM ASSESSMENT\n")
  cat("------------------------------------\n")
  
  mechanism_assessment <- assess_missing_mechanisms(data)
  
  # 3. CONTEXT-SPECIFIC ANALYSIS (Medical Data)
  cat("\n3. MEDICAL DATA CONTEXT ANALYSIS\n")
  cat("--------------------------------\n")
  
  medical_context <- analyze_medical_missing_context(data)
  
  # 4. RECOMMENDATIONS
  cat("\n4. RECOMMENDATIONS\n")
  cat("------------------------------\n")
  
  recommendations <- generate_missing_data_recommendations(
    pattern_analysis, mechanism_assessment, medical_context, data
  )
  
  for (rec in recommendations) {
    cat(sprintf("• %s\n", rec))
  }
  
  return(list(
    missing_counts = missing_counts,
    missing_percentages = missing_percentages,
    total_missing = sum(missing_counts),
    pattern_analysis = pattern_analysis,
    mechanism_assessment = mechanism_assessment,
    medical_context = medical_context,
    recommendations = recommendations
  ))
}

# Detect missing data patterns (with complete error handling)
detect_missing_patterns <- function(data) {
  
  # Create missing indicator matrix
  missing_matrix <- is.na(data)
  
  # Pattern 1: Completely missing variables
  completely_missing <- names(data)[sapply(data, function(x) all(is.na(x)))]
  
  # Pattern 2: Variables with high missingness (>50%)
  high_missing <- names(data)[sapply(data, function(x) sum(is.na(x))/length(x) > 0.5)]
  
  # Pattern 3: Systematic patterns (same rows missing across variables)
  rows_multiple_missing <- c()
  high_corr_pairs <- matrix(ncol = 2, nrow = 0)
  
  if (ncol(missing_matrix) > 1) {
    # Check for rows with multiple missing values
    rows_multiple_missing <- which(rowSums(missing_matrix) > 1)
    
    # Check for correlated missingness between variables (with complete error handling)
    tryCatch({
      if (sum(missing_matrix) > 0) {  # Only if there are missing values
        # Check if we have enough variability to compute correlations
        col_sums <- colSums(missing_matrix)
        varying_cols <- col_sums > 0 & col_sums < nrow(missing_matrix)
        
        if (sum(varying_cols) > 1) {  # Need at least 2 columns with some but not all missing
          varying_matrix <- missing_matrix[, varying_cols, drop = FALSE]
          
          # Check for standard deviation > 0 for each column
          col_sds <- sapply(varying_matrix, function(x) {
            x_numeric <- as.numeric(x)
            if (all(is.na(x_numeric))) return(0)
            sd(x_numeric, na.rm = TRUE)
          })
          
          # Only proceed if all standard deviations are valid and > 0
          if (all(!is.na(col_sds)) && all(col_sds > 0)) {
            missing_correlations <- cor(varying_matrix * 1, use = "pairwise.complete.obs")
            
            # Check if correlation matrix is valid
            if (!any(is.na(missing_correlations)) && is.matrix(missing_correlations)) {
              # Find high correlations (excluding diagonal and NAs)
              high_corr_indices <- which(abs(missing_correlations) > 0.3 & 
                                         missing_correlations != 1 & 
                                         !is.na(missing_correlations), arr.ind = TRUE)
              
              # Remove duplicates (keep only upper triangle)
              if (nrow(high_corr_indices) > 0) {
                high_corr_pairs <- high_corr_indices[high_corr_indices[,1] < high_corr_indices[,2], , drop = FALSE]
              }
            }
          }
        }
      }
    }, error = function(e) {
      # Only show warning if it's not a known statistical issue
      if (!grepl("standard deviation is zero|missing value where TRUE/FALSE needed", e$message)) {
        cat("Warning: Could not compute missing data correlations -", e$message, "\n")
      }
    }, warning = function(w) {
      # Suppress known statistical warnings
      if (!grepl("standard deviation is zero", w$message)) {
        cat("Warning in correlation computation:", w$message, "\n")
      }
    })
  }
  
  # Pattern 4: Missing data by groups
  group_patterns <- list()
  if ("grupa" %in% names(data)) {
    for (group in unique(data$grupa)) {
      group_data <- data[data$grupa == group, ]
      group_missing <- sapply(group_data, function(x) sum(is.na(x)))
      if (any(group_missing > 0)) {
        group_patterns[[as.character(group)]] <- group_missing[group_missing > 0]
      }
    }
  }
  
  # Summarize patterns
  patterns <- list(
    completely_missing = completely_missing,
    high_missing = high_missing,
    multiple_missing_rows = length(rows_multiple_missing),
    correlated_missing = nrow(high_corr_pairs),
    group_patterns = group_patterns
  )
  
  # Report findings
  cat("Pattern Detection Results:\n")
  
  if (length(completely_missing) > 0) {
        cat(sprintf("WARNING: %d variables completely missing: %s\n",
                length(completely_missing), paste(completely_missing, collapse = ", ")))
  }
  
  if (length(high_missing) > 0) {
    cat(sprintf("WARNING: %d variables with >50%% missing: %s\n", 
                length(high_missing), paste(high_missing, collapse = ", ")))
  }
  
  if (length(rows_multiple_missing) > 0) {
    cat(sprintf("WARNING: %d subjects with multiple missing values\n", length(rows_multiple_missing)))
  }
  
  if (nrow(high_corr_pairs) > 0) {
    cat(sprintf("WARNING: Correlated missingness detected between %d variable pairs\n", nrow(high_corr_pairs)))
  }
  
  if (length(group_patterns) > 0) {
    cat("WARNING: Group-specific missing patterns detected:\n")
    for (group in names(group_patterns)) {
      missing_vars <- names(group_patterns[[group]])
      cat(sprintf("  - Group %s: %s\n", group, paste(missing_vars, collapse = ", ")))
    }
  }
  
  if (length(completely_missing) == 0 && length(high_missing) == 0 && 
      length(rows_multiple_missing) == 0 && nrow(high_corr_pairs) == 0 && 
      length(group_patterns) == 0) {
    cat(" No concerning missing data patterns detected\n")
  }
  
  return(patterns)
}

# Assess missing data mechanisms (MCAR, MAR, MNAR)
assess_missing_mechanisms <- function(data) {
  
  mechanisms <- list()
  
  # Test for MCAR (Missing Completely At Random)
  cat("Testing for Missing Completely At Random (MCAR):\n")
  
  # Little's MCAR test (simplified version)
  mcar_assessment <- test_mcar_simplified(data)
  mechanisms$mcar <- mcar_assessment
  
  # Test for MAR (Missing At Random) patterns
  cat("\nTesting for Missing At Random (MAR) patterns:\n")
  
  mar_assessment <- test_mar_patterns(data)
  mechanisms$mar <- mar_assessment
  
  # Assess potential MNAR (Missing Not At Random)
  cat("\nAssessing Missing Not At Random (MNAR) likelihood:\n")
  
  mnar_assessment <- assess_mnar_likelihood(data)
  mechanisms$mnar <- mnar_assessment
  
  return(mechanisms)
}

# Simplified MCAR test
test_mcar_simplified <- function(data) {
  
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) < 2) {
    cat("• Insufficient numeric variables for MCAR testing\n")
    return(list(conclusion = "Insufficient data", p_value = NA))
  }
  
  # Check if missing patterns are random across variables
  missing_matrix <- is.na(numeric_data)
  
  # Simple randomness test: compare missing proportions across groups
  if ("grupa" %in% names(data)) {
    group_missing_props <- list()
    
    for (col in names(numeric_data)) {
      if (sum(is.na(numeric_data[[col]])) > 0) {
        tryCatch({
          props <- tapply(is.na(numeric_data[[col]]), data$grupa, mean)
          group_missing_props[[col]] <- props
          
          # Chi-square test for independence (with proper error handling)
          if (length(unique(props)) > 1) {
            missing_by_group <- table(data$grupa, is.na(numeric_data[[col]]))
            
            # Check if we have sufficient observations for chi-square test
            if (all(missing_by_group >= 1) && min(missing_by_group) > 0) {
              # Use Fisher's exact test for small samples
              if (any(missing_by_group < 5)) {
                # For small samples, use a more lenient approach
                max_prop_diff <- max(props) - min(props)
                if (max_prop_diff > 0.2) {  # 20% difference threshold
                  cat(sprintf(" %s: Large differences in missing proportions by group\n", col))
                  cat("  → Suggests NOT completely random\n")
                  return(list(conclusion = "Not MCAR", p_value = NA))
                }
              } else {
                chi_test <- chisq.test(missing_by_group)
                cat(sprintf(" %s: Missing patterns differ by group (p=%.3f)\n", 
                            col, chi_test$p.value))
                
                if (chi_test$p.value < 0.05) {
                  cat("  → Suggests NOT completely random\n")
                  return(list(conclusion = "Not MCAR", p_value = chi_test$p.value))
                }
              }
            }
          }
        }, error = function(e) {
          cat(sprintf("• %s: MCAR test failed - %s\n", col, e$message))
        })
      }
    }
  }
  
  cat("• Missing patterns appear consistent with MCAR\n")
  return(list(conclusion = "Likely MCAR", p_value = NA))
}

# Test for MAR patterns
test_mar_patterns <- function(data) {
  
  # Look for relationships between missingness and observed variables
  mar_evidence <- list()
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  for (col in numeric_cols) {
    if (sum(is.na(data[[col]])) > 0) {
      
      # Test relationship with age (with proper error handling)
      if ("wiek" %in% names(data) && col != "wiek") {
        tryCatch({
          missing_indicator <- is.na(data[[col]])
          age_complete <- data$wiek[!is.na(data$wiek)]
          missing_complete <- missing_indicator[!is.na(data$wiek)]
          
          if (length(unique(missing_complete)) > 1 && length(age_complete) > 10) {
            # T-test: do missing vs non-missing have different ages?
            age_missing <- age_complete[missing_complete]
            age_present <- age_complete[!missing_complete]
            
            # Check if we have enough observations for both groups
            if (length(age_missing) >= 2 && length(age_present) >= 2) {
              t_test <- t.test(age_missing, age_present)
              
              if (t_test$p.value < 0.1) {  # More lenient threshold
                cat(sprintf(" %s missingness related to age (p=%.3f)\n", col, t_test$p.value))
                cat(sprintf("  Mean age when missing: %.1f vs present: %.1f\n", 
                            mean(age_missing), mean(age_present)))
                mar_evidence[[col]] <- "age_related"
              }
            } else {
              cat(sprintf(" %s: Insufficient observations for age comparison\n", col))
            }
          }
        }, error = function(e) {
          cat(sprintf(" %s: Age relationship test failed - %s\n", col, e$message))
        })
      }
      
      # Test relationship with gender (with proper error handling)
      if ("plec" %in% names(data)) {
        tryCatch({
          missing_by_gender <- table(data$plec, is.na(data[[col]]))
          
          # Check if we have sufficient observations in all cells
          if (all(missing_by_gender > 0) && min(missing_by_gender) >= 2) {
            chi_test <- chisq.test(missing_by_gender)
            
            if (chi_test$p.value < 0.1) {
              cat(sprintf(" %s missingness related to gender (p=%.3f)\n", col, chi_test$p.value))
              mar_evidence[[col]] <- "gender_related"
            }
          } else {
            cat(sprintf(" %s: Insufficient data for gender analysis\n", col))
          }
        }, error = function(e) {
          cat(sprintf(" %s: Gender relationship test failed - %s\n", col, e$message))
        })
      }
      
      # Test relationship with group (with proper error handling)
      if ("grupa" %in% names(data)) {
        tryCatch({
          missing_by_group <- table(data$grupa, is.na(data[[col]]))
          
          # Check if we have sufficient observations in all cells
          if (all(missing_by_group > 0) && min(missing_by_group) >= 2) {
            chi_test <- chisq.test(missing_by_group)
            
            if (chi_test$p.value < 0.1) {
              cat(sprintf(" %s missingness related to group (p=%.3f)\n", col, chi_test$p.value))
              mar_evidence[[col]] <- "group_related"
            }
          } else {
            cat(sprintf(" %s: Insufficient data for group analysis\n", col))
          }
        }, error = function(e) {
          cat(sprintf(" %s: Group relationship test failed - %s\n", col, e$message))
        })
      }
    }
  }
  
  if (length(mar_evidence) == 0) {
    cat(" No clear MAR patterns detected\n")
  }
  
  return(mar_evidence)
}

# Assess MNAR likelihood
assess_mnar_likelihood <- function(data) {
  
  cat("Medical context suggests potential MNAR scenarios:\n")
  
  mnar_risks <- list()
  
  # Medical parameters that might be MNAR
  medical_params <- c("hsCRP", "ERY", "PLT", "HGB", "HCT", "MCHC", "MON", "LEU")
  
  for (param in medical_params) {
    if (param %in% names(data) && sum(is.na(data[[param]])) > 0) {
      
      missing_pct <- 100 * sum(is.na(data[[param]])) / nrow(data)
      
      # High missing percentage might indicate selective reporting
      if (missing_pct > 20) {
        cat(sprintf("• %s (%.1f%% missing): High missingness - possible selective reporting\n", 
                    param, missing_pct))
        mnar_risks[[param]] <- "high_missingness"
      }
      
      # Check for extreme values in non-missing data (might indicate threshold effects)
      if (is.numeric(data[[param]])) {
        observed_values <- data[[param]][!is.na(data[[param]])]
        
        if (length(observed_values) > 5) {
          # Check for potential ceiling/floor effects
          q99 <- quantile(observed_values, 0.99)
          q01 <- quantile(observed_values, 0.01)
          
          extreme_high <- sum(observed_values >= q99)
          extreme_low <- sum(observed_values <= q01)
          
          if (extreme_high > length(observed_values) * 0.05) {
            cat(sprintf(" %s: Many high values present - missing might be low values\n", param))
            mnar_risks[[param]] <- "threshold_effect"
          }
        }
      }
    }
  }
  
  # Check for patterns suggesting systematic exclusions
  if ("grupa" %in% names(data)) {
    for (group in unique(data$grupa)) {
      group_data <- data[data$grupa == group, ]
      group_missing <- sapply(group_data, function(x) sum(is.na(x)))
      total_group_missing <- sum(group_missing)
      
      if (total_group_missing > nrow(group_data) * 0.3) {
        cat(sprintf(" Group %s has high overall missingness - possible systematic exclusion\n", group))
        mnar_risks[[paste0("group_", group)]] <- "systematic_exclusion"
      }
    }
  }
  
  if (length(mnar_risks) == 0) {
    cat("• Low likelihood of MNAR based on available evidence\n")
  }
  
  return(mnar_risks)
}

# Analyze medical-specific missing data context
analyze_medical_missing_context <- function(data) {
  
  medical_context <- list()
  
  # 1. Lab test availability patterns
  lab_tests <- c("hsCRP", "ERY", "PLT", "HGB", "HCT", "MCHC", "MON", "LEU")
  available_tests <- lab_tests[lab_tests %in% names(data)]
  
  if (length(available_tests) > 0) {
    cat("Laboratory test completion rates:\n")
    
    for (test in available_tests) {
      completion_rate <- 100 * (1 - sum(is.na(data[[test]])) / nrow(data))
      cat(sprintf("• %-8s: %.1f%% complete\n", test, completion_rate))
      
      # Medical interpretation
      if (completion_rate < 80) {
        if (test == "hsCRP") {
          cat("  → hsCRP often ordered only when inflammation suspected\n")
          medical_context[[test]] <- "conditional_ordering"
        } else if (test %in% c("ERY", "HGB", "HCT")) {
          cat("  → Basic hematology - low completion suggests systematic issue\n")
          medical_context[[test]] <- "systematic_issue"
        } else {
          cat("  → May indicate selective testing based on clinical indication\n")
          medical_context[[test]] <- "selective_testing"
        }
      }
    }
  }
  
  # 2. Missing data by disease group (clinical context)
  if ("grupa" %in% names(data)) {
    cat("\nMissing data patterns by clinical group:\n")
    
    for (group in unique(data$grupa)) {
      group_data <- data[data$grupa == group, ]
      
      cat(sprintf("\nGroup %s (n=%d):\n", group, nrow(group_data)))
      
      for (test in available_tests) {
        if (test %in% names(group_data)) {
          missing_count <- sum(is.na(group_data[[test]]))
          missing_pct <- 100 * missing_count / nrow(group_data)
          
          if (missing_pct > 5) {  # Report if >5% missing
            cat(sprintf("   %-8s: %d missing (%.1f%%)\n", test, missing_count, missing_pct))
            
            # Clinical interpretation
            if (group == "KONTROLA" && missing_pct > 10) {
              cat("    → Unexpected in healthy controls - possible data collection issue\n")
            } else if (group %in% c("CHOR1", "CHOR2") && missing_pct > 20) {
              cat("    → High missingness in disease group - possible severity-related\n")
            }
          }
        }
      }
    }
  }
  
  # 3. Age-related missing patterns (elderly patients might have different test patterns)
  if ("wiek" %in% names(data)) {
    cat("\nAge-related missing data analysis:\n")
    
    for (test in available_tests) {
      if (test %in% names(data) && sum(is.na(data[[test]])) > 0) {
        complete_cases <- !is.na(data[[test]])
        
        if (sum(complete_cases) > 0 && sum(!complete_cases) > 0) {
          age_complete <- mean(data$wiek[complete_cases], na.rm = TRUE)
          age_missing <- mean(data$wiek[!complete_cases], na.rm = TRUE)
          
          age_diff <- age_complete - age_missing
          
          if (abs(age_diff) > 3) {  # More than 3 years difference
            cat(sprintf(" %-8s: Mean age complete %.1f vs missing %.1f (diff: %.1f years)\n", 
                        test, age_complete, age_missing, age_diff))
            
            if (age_diff > 0) {
              cat("  → Younger patients more likely to have missing values\n")
            } else {
              cat("  → Older patients more likely to have missing values\n")
            }
          }
        }
      }
    }
  }
  
  return(medical_context)
}

# Generate intelligent recommendations
generate_missing_data_recommendations <- function(patterns, mechanisms, medical_context, data) {
  
  recommendations <- c()
  
  # Based on missing percentages
  total_missing_pct <- 100 * sum(sapply(data, function(x) sum(is.na(x)))) / (nrow(data) * ncol(data))
  
  if (total_missing_pct < 1) {
    recommendations <- c(recommendations, "Low missing data (<1%) - simple listwise deletion acceptable")
  } else if (total_missing_pct < 5) {
    recommendations <- c(recommendations, "Moderate missing data (<5%) - mean/median imputation or regression imputation recommended")
  } else {
    recommendations <- c(recommendations, "High missing data (>5%) - advanced imputation (MICE) strongly recommended")
  }
  
  # Based on mechanism assessment
  if (!is.null(mechanisms$mcar) && mechanisms$mcar$conclusion == "Likely MCAR") {
    recommendations <- c(recommendations, "Data appears MCAR - listwise deletion will not introduce bias")
  } else if (length(mechanisms$mar) > 0) {
    recommendations <- c(recommendations, "MAR patterns detected - use predictive imputation including related variables")
  }
  
  if (length(mechanisms$mnar) > 0) {
    recommendations <- c(recommendations, "WARNING: Possible MNAR patterns - consider sensitivity analysis with different assumptions")
  }
  
  # Medical context recommendations
  if (length(medical_context) > 0) {
    has_conditional <- any(sapply(medical_context, function(x) x == "conditional_ordering"))
    has_systematic <- any(sapply(medical_context, function(x) x == "systematic_issue"))
    
    if (has_conditional) {
      recommendations <- c(recommendations, "Some tests appear conditionally ordered - consider clinical context in imputation")
    }
    
    if (has_systematic) {
      recommendations <- c(recommendations, "WARNING: Systematic issues detected in basic tests - investigate data collection procedures")
    }
  }
  
  # Group-specific recommendations
  if ("grupa" %in% names(data)) {
    group_sizes <- table(data$grupa)
    small_groups <- names(group_sizes)[group_sizes < 20]
    
    if (length(small_groups) > 0) {
      recommendations <- c(recommendations, 
                          sprintf("Small group sizes (%s) - be cautious with group-specific imputation", 
                                  paste(small_groups, collapse = ", ")))
    }
  }
  
  # Variable-specific recommendations
  high_missing_vars <- names(data)[sapply(data, function(x) sum(is.na(x))/length(x) > 0.2)]
  
  if (length(high_missing_vars) > 0) {
    recommendations <- c(recommendations, 
                        sprintf("Consider excluding variables with >20%% missing: %s", 
                                paste(high_missing_vars, collapse = ", ")))
  }
  
  return(recommendations)
}

# Choose optimal imputation method based on analysis
choose_optimal_imputation_method <- function(missing_analysis, default_method) {
  
  # If MCAR and low missing, use simple methods
  if (!is.null(missing_analysis$mechanism_assessment$mcar) && 
      missing_analysis$mechanism_assessment$mcar$conclusion == "Likely MCAR" &&
      missing_analysis$total_missing < 10) {
    return("mean_median")
  }
  
  # If MAR patterns detected, use predictive methods
  if (length(missing_analysis$mechanism_assessment$mar) > 0) {
    return("regression")
  }
  
  # If high missing and complex patterns, use advanced methods
  if (missing_analysis$total_missing > 20) {
    if (has_advanced_packages) {
      return("mice")
    } else {
      return("regression")
    }
  }
  
  # Default to user preference
  return(default_method)
}

# Handle missing data with different strategies
handle_missing_data <- function(data, method = "mice", threshold = 0.1) {
  
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  cols_with_missing <- names(missing_counts[missing_counts > 0])
  
  if (length(cols_with_missing) == 0) {
    cat("No missing data to handle.\n")
    return(list(data = data, details = "No missing data"))
  }
  
  cat("Handling missing data using method:", method, "\n")
  
  switch(method,
    "listwise" = {
      # Complete case analysis
      original_rows <- nrow(data)
      data_clean <- data[complete.cases(data), ]
      removed_rows <- original_rows - nrow(data_clean)
      
      cat(sprintf("Listwise deletion: removed %d rows (%.1f%%)\n", 
                  removed_rows, 100 * removed_rows / original_rows))
      
      return(list(
        data = data_clean,
        details = list(method = "listwise", rows_removed = removed_rows)
      ))
    },
    
    "mean_median" = {
      # Mean/median imputation
      data_imputed <- data
      imputation_details <- list()
      
      for (col in cols_with_missing) {
        if (is.numeric(data[[col]])) {
          if (col %in% c("wiek")) {
            # Use median for age (more robust)
            impute_value <- median(data[[col]], na.rm = TRUE)
            method_used <- "median"
          } else {
            # Use mean for other numeric variables
            impute_value <- mean(data[[col]], na.rm = TRUE)
            method_used <- "mean"
          }
          
          data_imputed[[col]][is.na(data_imputed[[col]])] <- impute_value
          imputation_details[[col]] <- list(method = method_used, value = impute_value)
          
          cat(sprintf("Imputed %s with %s: %.3f\n", col, method_used, impute_value))
        }
      }
      
      return(list(
        data = data_imputed,
        details = imputation_details
      ))
    },
    
    "regression" = {
      # Linear regression imputation
      data_imputed <- data
      imputation_details <- list()
      
      for (col in cols_with_missing) {
        if (is.numeric(data[[col]])) {
          # Use other numeric variables as predictors
          numeric_cols <- sapply(data, is.numeric)
          predictor_cols <- names(data)[numeric_cols & names(data) != col]
          predictor_cols <- predictor_cols[!sapply(data[predictor_cols], function(x) all(is.na(x)))]
          
          if (length(predictor_cols) > 0) {
            # Create formula for regression
            formula_str <- paste(col, "~", paste(predictor_cols, collapse = " + "))
            
            tryCatch({
              # Fit regression model on complete cases
              complete_data <- data[complete.cases(data[c(col, predictor_cols)]), ]
              
              if (nrow(complete_data) > 10) {  # Need sufficient data
                lm_model <- lm(as.formula(formula_str), data = complete_data)
                
                # Predict missing values
                missing_indices <- is.na(data[[col]])
                if (sum(missing_indices) > 0) {
                  predicted_values <- predict(lm_model, newdata = data[missing_indices, ])
                  data_imputed[[col]][missing_indices] <- predicted_values
                  
                  imputation_details[[col]] <- list(
                    method = "regression",
                    predictors = predictor_cols,
                    r_squared = summary(lm_model)$r.squared
                  )
                  
                  cat(sprintf("Imputed %s using regression (R² = %.3f)\n", 
                              col, summary(lm_model)$r.squared))
                }
              } else {
                # Fallback to mean imputation
                impute_value <- mean(data[[col]], na.rm = TRUE)
                data_imputed[[col]][is.na(data_imputed[[col]])] <- impute_value
                imputation_details[[col]] <- list(method = "mean_fallback", value = impute_value)
                cat(sprintf("Insufficient data for regression, used mean for %s\n", col))
              }
            }, error = function(e) {
              # Fallback to mean imputation
              impute_value <- mean(data[[col]], na.rm = TRUE)
              data_imputed[[col]][is.na(data_imputed[[col]])] <- impute_value
              imputation_details[[col]] <- list(method = "mean_fallback", value = impute_value)
              cat(sprintf("Regression failed for %s, used mean imputation\n", col))
            })
          }
        }
      }
      
      return(list(
        data = data_imputed,
        details = imputation_details
      ))
    },
    
    "mice" = {
      # Multiple imputation using MICE
      if (has_advanced_packages) {
        cat("Performing multiple imputation with MICE...\n")
        
        # Prepare data for MICE (only numeric columns)
        numeric_cols <- sapply(data, is.numeric)
        mice_data <- data[, numeric_cols, drop = FALSE]
        
        tryCatch({
          # Perform MICE imputation
          mice_result <- mice::mice(mice_data, m = 5, method = 'pmm', 
                                  printFlag = FALSE, seed = 123)
          
          # Use the first imputed dataset
          imputed_data <- mice::complete(mice_result, 1)
          
          # Replace numeric columns in original data
          for (col in names(imputed_data)) {
            data[[col]] <- imputed_data[[col]]
          }
          
          cat("MICE imputation completed successfully\n")
          
          return(list(
            data = data,
            details = list(method = "mice", imputations = 5)
          ))
          
        }, error = function(e) {
          cat("MICE failed, falling back to regression imputation\n")
          return(handle_missing_data(data, method = "regression", threshold = threshold))
        })
      } else {
        cat("MICE package not available, using regression imputation\n")
        return(handle_missing_data(data, method = "regression", threshold = threshold))
      }
    }
  )
}

# Detect outliers using various methods
detect_outliers <- function(data, method = "iqr", threshold = 1.5) {
  
  numeric_cols <- sapply(data, is.numeric)
  outliers <- list()
  outlier_summary <- list()
  
  cat("Detecting outliers using method:", method, "\n")
  
  for (col in names(data)[numeric_cols]) {
    col_data <- data[[col]][!is.na(data[[col]])]
    
    if (length(col_data) == 0) next
    
    switch(method,
      "iqr" = {
        q1 <- quantile(col_data, 0.25)
        q3 <- quantile(col_data, 0.75)
        iqr <- q3 - q1
        lower_bound <- q1 - threshold * iqr
        upper_bound <- q3 + threshold * iqr
        
        outlier_indices <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
        outliers[[col]] <- outlier_indices
        
        outlier_summary[[col]] <- list(
          method = "IQR",
          count = length(outlier_indices),
          bounds = c(lower_bound, upper_bound),
          percentage = round(100 * length(outlier_indices) / length(col_data), 2)
        )
      },
      
      "zscore" = {
        z_scores <- abs(scale(col_data))
        outlier_indices <- which(abs(scale(data[[col]])) > threshold)
        outliers[[col]] <- outlier_indices
        
        outlier_summary[[col]] <- list(
          method = "Z-score",
          count = length(outlier_indices),
          threshold = threshold,
          percentage = round(100 * length(outlier_indices) / length(col_data), 2)
        )
      },
      
      "modified_zscore" = {
        median_val <- median(col_data)
        mad_val <- mad(col_data)
        modified_z <- 0.6745 * (data[[col]] - median_val) / mad_val
        outlier_indices <- which(abs(modified_z) > threshold)
        outliers[[col]] <- outlier_indices
        
        outlier_summary[[col]] <- list(
          method = "Modified Z-score",
          count = length(outlier_indices),
          threshold = threshold,
          percentage = round(100 * length(outlier_indices) / length(col_data), 2)
        )
      }
    )
    
    if (length(outlier_indices) > 0) {
      cat(sprintf("- %-10s: %d outliers (%.1f%%)\n", 
                  col, length(outlier_indices), 
                  100 * length(outlier_indices) / length(col_data)))
    }
  }
  
  # Flatten outlier list
  all_outliers <- unique(unlist(outliers))
  
  return(list(
    outliers = all_outliers,
    outliers_by_column = outliers,
    summary = outlier_summary,
    method = method
  ))
}

# Handle outliers (remove or transform)
handle_outliers <- function(data, outliers, method = "winsorize") {
  
  if (length(outliers) == 0) {
    cat("No outliers to handle.\n")
    return(data)
  }
  
  cat("Handling", length(outliers), "outliers using method:", method, "\n")
  
  switch(method,
    "remove" = {
      # Remove outlier rows
      data_clean <- data[-outliers, ]
      cat("Removed", length(outliers), "outlier rows\n")
      return(data_clean)
    },
    
    "winsorize" = {
      # Winsorize at 5th and 95th percentiles
      numeric_cols <- sapply(data, is.numeric)
      
      for (col in names(data)[numeric_cols]) {
        if (sum(!is.na(data[[col]])) > 0) {
          p05 <- quantile(data[[col]], 0.05, na.rm = TRUE)
          p95 <- quantile(data[[col]], 0.95, na.rm = TRUE)
          
          # Cap values at percentiles
          original_outliers <- sum(data[[col]] < p05 | data[[col]] > p95, na.rm = TRUE)
          data[[col]][data[[col]] < p05] <- p05
          data[[col]][data[[col]] > p95] <- p95
          
          if (original_outliers > 0) {
            cat(sprintf("Winsorized %d values in %s\n", original_outliers, col))
          }
        }
      }
      
      return(data)
    },
    
    "log_transform" = {
      # Log transform positive values
      numeric_cols <- sapply(data, is.numeric)
      
      for (col in names(data)[numeric_cols]) {
        if (sum(!is.na(data[[col]])) > 0 && all(data[[col]] > 0, na.rm = TRUE)) {
          data[[paste0(col, "_log")]] <- log(data[[col]])
          cat(sprintf("Created log-transformed version of %s\n", col))
        }
      }
      
      return(data)
    }
  )
}

# Validate repaired data
validate_repaired_data <- function(data) {
  missing_after <- sapply(data, function(x) sum(is.na(x)))
  
  validation_results <- list(
    rows = nrow(data),
    columns = ncol(data),
    missing_values = sum(missing_after),
    data_types = sapply(data, class),
    numeric_columns = sum(sapply(data, is.numeric)),
    factor_columns = sum(sapply(data, is.factor))
  )
  
  cat("Post-repair validation:\n")
  cat(sprintf("- Rows: %d\n", validation_results$rows))
  cat(sprintf("- Columns: %d\n", validation_results$columns))
  cat(sprintf("- Missing values: %d\n", validation_results$missing_values))
  cat(sprintf("- Numeric columns: %d\n", validation_results$numeric_columns))
  cat(sprintf("- Factor columns: %d\n", validation_results$factor_columns))
  
  return(validation_results)
}

# Generate data cleaning report
generate_cleaning_report <- function(original_data, cleaned_data, changes_made) {
  
  report <- list(
    original_dimensions = dim(original_data),
    cleaned_dimensions = dim(cleaned_data),
    original_missing = sum(sapply(original_data, function(x) sum(is.na(x)))),
    cleaned_missing = sum(sapply(cleaned_data, function(x) sum(is.na(x)))),
    changes_made = changes_made,
    timestamp = Sys.time()
  )
  
  cat("\n=== DATA CLEANING REPORT ===\n")
  cat("Timestamp:", format(report$timestamp), "\n")
  cat("Original dimensions:", report$original_dimensions[1], "x", report$original_dimensions[2], "\n")
  cat("Cleaned dimensions:", report$cleaned_dimensions[1], "x", report$cleaned_dimensions[2], "\n")
  cat("Missing values reduced from", report$original_missing, "to", report$cleaned_missing, "\n")
  
  if (!is.null(changes_made$missing_treatment)) {
    cat("Missing data treatment:", changes_made$missing_treatment$method, "\n")
  }
  
  if (!is.null(changes_made$outlier_treatment)) {
    cat("Outlier treatment:", changes_made$outlier_treatment$method, "\n")
    cat("Outliers treated:", changes_made$outlier_treatment$outliers_treated, "\n")
  }
  
  cat("=== END REPORT ===\n\n")
  
  return(report)
}

# Task G: Missing Data Sensitivity Analysis
perform_missing_data_sensitivity_analysis <- function(data, missing_analysis) {
  
  cat("=== TASK G: MISSING DATA SENSITIVITY ANALYSIS ===\n")
  cat("Performing comprehensive imputation comparison as per project requirements\n\n")
  
  # Get columns with missing data
  missing_columns <- names(which(missing_analysis$missing_counts > 0))
  
  if (length(missing_columns) == 0) {
    cat("No missing data found - sensitivity analysis not needed\n")
    return(list(
      recommended_data = data,
      recommended_method = "none",
      comparison = "No missing data"
    ))
  }
  
  cat("Columns with missing data:", paste(missing_columns, collapse = ", "), "\n")
  cat("Total missing values:", sum(missing_analysis$missing_counts), "\n\n")
  
  # Methods to test (as specified in project plan)
  methods <- list(
    "mean_median" = "mean/median imputation",
    "regression" = "regression imputation", 
    "multiple_mice" = "multiple imputation (MICE)"
  )
  
  results <- list()
  imputed_datasets <- list()
  
  # 1. MEAN/MEDIAN IMPUTATION
  cat("1. TESTING MEAN/MEDIAN IMPUTATION\n")
  cat("----------------------------------\n")
  tryCatch({
    data_mean_median <- data
    for (col in missing_columns) {
      if (is.numeric(data[[col]])) {
        # Use median for skewed data, mean for normal data
        if (abs(e1071::skewness(data[[col]], na.rm = TRUE)) > 1) {
          impute_value <- median(data[[col]], na.rm = TRUE)
          method_used <- "median"
        } else {
          impute_value <- mean(data[[col]], na.rm = TRUE)
          method_used <- "mean"
        }
        missing_count <- sum(is.na(data_mean_median[[col]]))
        data_mean_median[[col]][is.na(data_mean_median[[col]])] <- impute_value
        cat(sprintf("  %s: Imputed %d values using %s (%.3f)\n", 
                   col, missing_count, method_used, impute_value))
      }
    }
    
    imputed_datasets[["mean_median"]] <- data_mean_median
    results[["mean_median"]] <- list(
      success = TRUE,
      method = "Mean/Median",
      missing_after = sum(sapply(data_mean_median, function(x) sum(is.na(x)))),
      note = "Simple univariate imputation"
    )
    cat("  ✓ Mean/median imputation completed\n\n")
    
  }, error = function(e) {
    cat("  ✗ Mean/median imputation failed:", e$message, "\n\n")
    results[["mean_median"]] <- list(success = FALSE, error = e$message)
  })
  
  # 2. REGRESSION IMPUTATION
  cat("2. TESTING REGRESSION IMPUTATION\n")
  cat("---------------------------------\n")
  tryCatch({
    data_regression <- handle_missing_data(data, method = "regression", threshold = 0)$data
    
    imputed_datasets[["regression"]] <- data_regression
    results[["regression"]] <- list(
      success = TRUE,
      method = "Regression",
      missing_after = sum(sapply(data_regression, function(x) sum(is.na(x)))),
      note = "Predictive imputation using other variables"
    )
    cat("  ✓ Regression imputation completed\n\n")
    
  }, error = function(e) {
    cat("  ✗ Regression imputation failed:", e$message, "\n\n")
    results[["regression"]] <- list(success = FALSE, error = e$message)
  })
  
  # 3. MULTIPLE IMPUTATION (MICE)
  cat("3. TESTING MULTIPLE IMPUTATION (MICE)\n")
  cat("-------------------------------------\n")
  tryCatch({
    if (requireNamespace("mice", quietly = TRUE)) {
      library(mice)
      
      # Run MICE with 5 imputations
      mice_result <- mice(data, m = 5, method = 'pmm', printFlag = FALSE, seed = 123)
      
      # Complete the first imputation for comparison
      data_mice <- complete(mice_result, 1)
      
      imputed_datasets[["multiple_mice"]] <- data_mice
      results[["multiple_mice"]] <- list(
        success = TRUE,
        method = "Multiple Imputation (MICE)",
        missing_after = sum(sapply(data_mice, function(x) sum(is.na(x)))),
        note = "Multiple imputation with predictive mean matching",
        mice_object = mice_result
      )
      cat("  ✓ Multiple imputation (MICE) completed with 5 imputations\n\n")
      
    } else {
      cat("  WARNING: MICE package not available - skipping multiple imputation\n\n")
      results[["multiple_mice"]] <- list(
        success = FALSE, 
        error = "MICE package not available"
      )
    }
    
  }, error = function(e) {
    cat("  ✗ Multiple imputation failed:", e$message, "\n\n")
    results[["multiple_mice"]] <- list(success = FALSE, error = e$message)
  })
  
  # 4. SENSITIVITY ANALYSIS COMPARISON
  cat("4. SENSITIVITY ANALYSIS COMPARISON\n")
  cat("==================================\n")
  
  successful_methods <- names(results)[sapply(results, function(x) x$success)]
  
  if (length(successful_methods) == 0) {
    cat("No imputation methods succeeded - returning original data\n")
    return(list(
      recommended_data = data,
      recommended_method = "none",
      comparison = results
    ))
  }
  
  # Compare distributions and correlations
  comparison_results <- list()
  
  for (method in successful_methods) {
    cat(sprintf("Analyzing %s method:\n", results[[method]]$method))
    
    imputed_data <- imputed_datasets[[method]]
    
    # Calculate impact metrics
    comparison_metrics <- list()
    
    for (col in missing_columns) {
      if (is.numeric(data[[col]])) {
        original_mean <- mean(data[[col]], na.rm = TRUE)
        original_sd <- sd(data[[col]], na.rm = TRUE)
        imputed_mean <- mean(imputed_data[[col]], na.rm = TRUE)
        imputed_sd <- sd(imputed_data[[col]], na.rm = TRUE)
        
        comparison_metrics[[col]] <- list(
          original_mean = original_mean,
          imputed_mean = imputed_mean,
          mean_change = abs(imputed_mean - original_mean),
          original_sd = original_sd,
          imputed_sd = imputed_sd,
          sd_change = abs(imputed_sd - original_sd)
        )
        
        cat(sprintf("  %s: Mean %.3f→%.3f (Δ=%.3f), SD %.3f→%.3f (Δ=%.3f)\n",
                   col, original_mean, imputed_mean, 
                   comparison_metrics[[col]]$mean_change,
                   original_sd, imputed_sd,
                   comparison_metrics[[col]]$sd_change))
      }
    }
    
    comparison_results[[method]] <- comparison_metrics
    cat("\n")
  }
  
  # 5. RECOMMENDATION
  cat("5. RECOMMENDATION\n")
  cat("=================\n")
  
  # Choose method with smallest distributional impact
  if ("multiple_mice" %in% successful_methods) {
    recommended_method <- "multiple_mice"
    recommendation_reason <- "Multiple imputation provides most robust estimates"
  } else if ("regression" %in% successful_methods) {
    recommended_method <- "regression" 
    recommendation_reason <- "Regression imputation uses available information"
  } else {
    recommended_method <- "mean_median"
    recommendation_reason <- "Simple imputation as fallback"
  }
  
  cat(sprintf("Recommended method: %s\n", results[[recommended_method]]$method))
  cat(sprintf("Reason: %s\n", recommendation_reason))
  
  return(list(
    recommended_data = imputed_datasets[[recommended_method]],
    recommended_method = results[[recommended_method]]$method,
    comparison = results,
    sensitivity_metrics = comparison_results,
    summary = list(
      methods_tested = length(results),
      methods_successful = length(successful_methods),
      total_missing_original = sum(missing_analysis$missing_counts),
      recommended_approach = recommendation_reason
    )
  ))
}
