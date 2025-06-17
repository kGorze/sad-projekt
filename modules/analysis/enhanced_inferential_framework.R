# Enhanced inferential framework
# Multiple linear regression and ANCOVA models with covariate adjustments
# Implements advanced statistical modeling with interaction terms

# Package dependencies
suppressPackageStartupMessages({
  if (requireNamespace("dplyr", quietly = TRUE)) library(dplyr)
  if (requireNamespace("broom", quietly = TRUE)) library(broom)
  if (requireNamespace("car", quietly = TRUE)) library(car)
  if (requireNamespace("emmeans", quietly = TRUE)) library(emmeans)
  if (requireNamespace("performance", quietly = TRUE)) library(performance)
  if (requireNamespace("effectsize", quietly = TRUE)) library(effectsize)
})

# Source required modules
source("modules/reporting/export_results.R")
source("modules/analysis/assumptions_dashboard.R")
source("modules/utils/statistical_helpers.R")

# Main function: Enhanced inferential analysis with covariates
perform_enhanced_inferential_analysis <- function(data, group_column = "grupa", dependent_vars = NULL, 
                                                 covariates = NULL, include_interactions = "auto", 
                                                 include_plots = TRUE, shared_assumptions = NULL) {
  
  # Create analysis result object
  result <- create_analysis_result("enhanced_inferential")
  
  cat("Starting enhanced inferential analysis...\n")
  
  # Identify dependent variables (continuous/numeric only)
  if (is.null(dependent_vars)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    dependent_vars <- numeric_vars[numeric_vars != group_column]
  } else {
    dependent_vars <- dependent_vars[sapply(data[dependent_vars], is.numeric)]
  }
  
  if (length(dependent_vars) == 0) {
    cat("ERROR: No numeric dependent variables found for inferential analysis\n")
    return(NULL)
  }
  
  # Identify potential covariates automatically if not specified
  if (is.null(covariates)) {
    cat("\n=== COVARIATE IDENTIFICATION ===\n")
    potential_covariates <- identify_covariates(data, dependent_vars, group_column)
  } else {
    cat("\n=== USING SPECIFIED COVARIATES ===\n")
    potential_covariates <- covariates[covariates %in% names(data)]
    potential_covariates <- potential_covariates[sapply(data[potential_covariates], is.numeric)]
    cat("Specified covariates validated:", paste(potential_covariates, collapse = ", "), "\n")
  }
  
  # Center continuous variables around their means to improve interpretation
  centered_data <- data
  covariate_centering_info <- list()
  centered_covariates <- potential_covariates  # Initialize with original names
  
  if (length(potential_covariates) > 0) {
    cat("\n=== COVARIATE CENTERING ===\n")
    for (covariate in potential_covariates) {
      if (covariate %in% names(data) && is.numeric(data[[covariate]])) {
        original_mean <- mean(data[[covariate]], na.rm = TRUE)
        centered_var_name <- paste0(covariate, "_centered")
        centered_data[[centered_var_name]] <- data[[covariate]] - original_mean
        covariate_centering_info[[covariate]] <- list(
          original_mean = original_mean,
          centered_variable = centered_var_name
        )
        cat("- Centered", covariate, "around mean =", round(original_mean, 2), "\n")
        
        # Update the covariate list to use centered names
        centered_covariates[centered_covariates == covariate] <- centered_var_name
      }
    }
  }
  
  cat("\n=== ANALYSIS CONFIGURATION ===\n")
  cat("- Total observations:", nrow(data), "\n")
  cat("- Groups:", paste(unique(data[[group_column]]), collapse = ", "), "\n")
  cat("- Dependent variables (", length(dependent_vars), "):", paste(dependent_vars, collapse = ", "), "\n")
  cat("- Covariates identified:", length(potential_covariates), "\n")
  if (length(potential_covariates) > 0) {
    cat("- Original covariates:", paste(potential_covariates, collapse = ", "), "\n")
    cat("- Centered covariates:", paste(centered_covariates, collapse = ", "), "\n")
  }
  
  # Use shared assumptions analysis (no duplication)
  if (!is.null(shared_assumptions)) {
    cat("\n=== STEP 0: USING SHARED ASSUMPTIONS ANALYSIS ===\n")
    cat("- Reusing comprehensive assumptions testing (eliminates duplication)\n")
    result$assumptions_analysis <- shared_assumptions
    display_assumption_summary(shared_assumptions)
  } else {
    cat("\n=== STEP 0: ASSUMPTIONS TESTING SKIPPED ===\n")
    cat("- No shared assumptions provided - proceeding with inferential analysis\n")
    cat("- Recommendation: Run descriptive stats first for comprehensive assumptions testing\n")
  }
  
  # Step 1: Multiple Linear Regression with Covariates
  cat("\n=== STEP 1: MULTIPLE LINEAR REGRESSION WITH COVARIATES ===\n")
  mlr_results <- tryCatch({
    perform_multiple_linear_regression(centered_data, dependent_vars, group_column, centered_covariates)
  }, error = function(e) {
    cat("Warning: Multiple regression analysis failed:", e$message, "\n")
    list()
  })
  result$multiple_regression <- mlr_results
  
  # Step 2: ANCOVA Models
  cat("\n=== STEP 2: ANCOVA MODELS ===\n")
  ancova_results <- tryCatch({
    perform_ancova_analysis(centered_data, dependent_vars, group_column, centered_covariates)
  }, error = function(e) {
    cat("Warning: ANCOVA analysis failed:", e$message, "\n")
    list()
  })
  result$ancova_analysis <- ancova_results
  
  # Step 3: Interaction Terms Analysis (conditional)
  interaction_results <- list()
  should_include_interactions <- decide_interaction_inclusion(include_interactions, centered_data, centered_covariates)
  
  if (should_include_interactions) {
    cat("\n=== STEP 3: INTERACTION TERMS ANALYSIS ===\n")
    interaction_results <- tryCatch({
      perform_interaction_analysis(centered_data, dependent_vars, group_column, centered_covariates)
    }, error = function(e) {
      cat("Warning: Interaction analysis failed:", e$message, "\n")
      list()
    })
  } else {
    cat("\n=== STEP 3: SKIPPING INTERACTION ANALYSIS ===\n")
    cat("Reason: ", attr(should_include_interactions, "reason"), "\n")
  }
  result$interaction_analysis <- interaction_results
  
  # Step 4: Model Selection and Comparison
  cat("\n=== STEP 4: MODEL SELECTION AND COMPARISON ===\n")
  model_comparison <- perform_model_comparison(centered_data, dependent_vars, group_column, centered_covariates)
  result$model_comparison <- model_comparison
  
  # Step 5: Effect Sizes and Confidence Intervals
  cat("\n=== STEP 5: EFFECT SIZES AND CONFIDENCE INTERVALS ===\n")
  effect_sizes <- calculate_enhanced_effect_sizes(mlr_results, ancova_results)
  result$effect_sizes <- effect_sizes
  
  # Generate plots if requested
  if (include_plots) {
    cat("\n=== STEP 6: GENERATING VISUALIZATIONS ===\n")
    plots_output_path <- file.path("output", "plots", "enhanced_inferential")
    plots_result <- create_inferential_plots(centered_data, result, dependent_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
  }
  
  # Add metadata
  result$metadata <- list(
    total_observations = nrow(data),
    groups = unique(data[[group_column]]),
    group_sizes = table(data[[group_column]]),
    dependent_variables = length(dependent_vars),
    covariates = length(potential_covariates),
    covariates_used = if(length(potential_covariates) > 0) centered_covariates else character(0),
    covariate_centering = covariate_centering_info,
    group_column = group_column,
    analysis_date = Sys.time()
  )
  
  cat("\nEnhanced inferential analysis completed successfully.\n")
  return(result)
}

# Decide whether to include interaction analysis
decide_interaction_inclusion <- function(include_interactions, data, covariates) {
  
  if (include_interactions == TRUE) {
    return(structure(TRUE, reason = "Explicitly requested"))
  }
  
  if (include_interactions == FALSE) {
    return(structure(FALSE, reason = "Explicitly disabled"))
  }
  
  # Auto decision logic
  n_total <- nrow(data)
  n_covariates <- length(covariates)
  
  # Criteria for automatic inclusion
  sufficient_sample_size <- n_total >= 75  # Rule of thumb: 15+ obs per parameter
  has_covariates <- n_covariates > 0
  adequate_power <- n_total >= (n_covariates * 20)  # Conservative power estimate
  
  if (!has_covariates) {
    return(structure(FALSE, reason = "No covariates available"))
  }
  
  if (!sufficient_sample_size) {
    return(structure(FALSE, reason = paste0("Insufficient sample size (n=", n_total, 
                                          ", recommended n>=75 for interaction analysis)")))
  }
  
  if (!adequate_power) {
    return(structure(FALSE, reason = paste0("Inadequate power (n=", n_total, 
                                          " with ", n_covariates, " covariates)")))
  }
  
  # Include interactions if all criteria met
  return(structure(TRUE, reason = paste0("Auto-included: adequate sample size (n=", n_total, 
                                       ") with ", n_covariates, " covariate(s)")))
}

# Identify potential covariates (universal approach)
identify_covariates <- function(data, dependent_vars, group_column) {
  
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  numeric_vars <- numeric_vars[!numeric_vars %in% dependent_vars]
  numeric_vars <- numeric_vars[numeric_vars != group_column]
  
  cat("Potential covariate candidates found:", paste(numeric_vars, collapse = ", "), "\n")
  
  potential_covariates <- list()
  
  # Universal statistical-based covariate identification
  # Works with any dataset and variable names
  data_clean <- data[complete.cases(data[c(group_column, numeric_vars)]), ]
  
  if (nrow(data_clean) > 0) {
    group_numeric <- as.numeric(as.factor(data_clean[[group_column]]))
    
    # Calculate correlations and variability for all numeric variables
    covariate_candidates <- c()
    for (var in numeric_vars) {
      if (var %in% names(data_clean)) {
        # Universal statistical criteria for covariate selection:
        # 1. Sufficient variability (CV > 3%) - meaningful variation in the data
        # 2. Not too highly correlated with groups (< 0.8 to avoid collinearity)
        # 3. No minimum correlation requirement - let statistical tests decide significance
        
        correlation <- abs(cor(data_clean[[var]], group_numeric, use = "complete.obs"))
        cv <- sd(data_clean[[var]], na.rm = TRUE) / mean(data_clean[[var]], na.rm = TRUE) * 100
        
        # Debug output for troubleshooting
        cat("- Evaluating", var, ": r =", round(correlation, 3), ", CV =", round(cv, 1), "%")
        
        # Universal criteria - works for any variable type and dataset
        if (cv > 3 && correlation < 0.8) {
          covariate_candidates <- c(covariate_candidates, var)
          cat(" → INCLUDED")
          cat(" (CV = ", round(cv, 1), "% > 3%, r = ", round(correlation, 3), " < 0.8)")
          cat("\n")
        } else {
          cat(" → EXCLUDED")
          if (cv <= 3) cat(" (low variability: CV =", round(cv, 1), "%)")
          if (correlation >= 0.8) cat(" (high correlation with groups: r =", round(correlation, 3), ")")
          cat("\n")
        }
      }
    }
    
    potential_covariates$statistical <- covariate_candidates
  }
  
  # Flatten the list
  all_covariates <- unlist(potential_covariates, use.names = FALSE)
  
  if (length(all_covariates) == 0) {
    cat("\nWARNING: No covariates identified using current criteria.\n")
    cat("This might indicate:\n")
    cat("1. Variables have very low variability (CV < 3%)\n")
    cat("2. Variables are too highly correlated with groups (r > 0.8)\n")
    cat("3. No suitable continuous variables available besides dependent vars\n")
    cat("Consider reviewing data quality or analysis strategy.\n\n")
  } else {
    cat("\nFinal covariates identified:", paste(all_covariates, collapse = ", "), "\n\n")
  }
  
  return(unique(all_covariates))
}

# Multiple linear regression with covariates
perform_multiple_linear_regression <- function(data, dependent_vars, group_column, covariates) {
  
  mlr_results <- list()
  
  for (var in dependent_vars) {
    cat("Analyzing", var, "with multiple regression...\n")
    
    # Prepare data
    model_vars <- c(var, group_column, covariates)
    clean_data <- data[complete.cases(data[model_vars]), ]
    
    if (nrow(clean_data) < 10) {
      mlr_results[[var]] <- list(
        variable = var,
        error = "Insufficient data for multiple regression"
      )
      next
    }
    
    tryCatch({
      # Model 1: Group only (baseline)
      formula_baseline <- as.formula(paste(var, "~", group_column))
      model_baseline <- lm(formula_baseline, data = clean_data)
      
      # Model 2: Group + Covariates (full model)
      if (length(covariates) > 0) {
        formula_full <- as.formula(paste(var, "~", group_column, "+", paste(covariates, collapse = " + ")))
        model_full <- lm(formula_full, data = clean_data)
        
        # Model comparison
        anova_comparison <- anova(model_baseline, model_full)
        
        # Model diagnostics
        diagnostics <- check_model_assumptions(model_full)
        
        mlr_results[[var]] <- list(
          variable = var,
          baseline_model = list(
            model = model_baseline,
            summary = summary(model_baseline),
            r_squared = summary(model_baseline)$r.squared,
            adj_r_squared = summary(model_baseline)$adj.r.squared
          ),
          full_model = list(
            model = model_full,
            summary = summary(model_full),
            r_squared = summary(model_full)$r.squared,
            adj_r_squared = summary(model_full)$adj.r.squared,
            coefficients = if (requireNamespace("broom", quietly = TRUE)) {
              broom::tidy(model_full, conf.int = TRUE)
            } else {
              summary(model_full)$coefficients
            }
          ),
          model_comparison = anova_comparison,
          diagnostics = diagnostics,
          n_observations = nrow(clean_data),
          covariates_used = covariates
        )
      } else {
        # Only baseline model if no covariates available
        mlr_results[[var]] <- list(
          variable = var,
          baseline_model = list(
            model = model_baseline,
            summary = summary(model_baseline),
            r_squared = summary(model_baseline)$r.squared,
            adj_r_squared = summary(model_baseline)$adj.r.squared,
            coefficients = if (requireNamespace("broom", quietly = TRUE)) {
              broom::tidy(model_baseline, conf.int = TRUE)
            } else {
              summary(model_baseline)$coefficients
            }
          ),
          n_observations = nrow(clean_data),
          note = "No covariates available"
        )
      }
      
    }, error = function(e) {
      mlr_results[[var]] <- list(
        variable = var,
        error = paste("Error in multiple regression:", e$message)
      )
    })
  }
  
  return(mlr_results)
}

# ANCOVA analysis
perform_ancova_analysis <- function(data, dependent_vars, group_column, covariates) {
  
  ancova_results <- list()
  
  if (length(covariates) == 0) {
    cat("No covariates available for ANCOVA analysis.\n")
    return(ancova_results)
  }
  
  for (var in dependent_vars) {
    cat("Performing ANCOVA for", var, "...\n")
    
    # Prepare data
    model_vars <- c(var, group_column, covariates)
    clean_data <- data[complete.cases(data[model_vars]), ]
    
    if (nrow(clean_data) < 15) {
      ancova_results[[var]] <- list(
        variable = var,
        error = "Insufficient data for ANCOVA"
      )
      next
    }
    
    tryCatch({
      # Fit ANCOVA model
      formula_ancova <- as.formula(paste(var, "~", group_column, "+", paste(covariates, collapse = " + ")))
      ancova_model <- lm(formula_ancova, data = clean_data)
      
      # Type III ANOVA (using car package)
      ancova_anova <- if (requireNamespace("car", quietly = TRUE)) {
        car::Anova(ancova_model, type = "III")
      } else {
        anova(ancova_model)
      }
      
      # Estimated marginal means
      emm_results <- if (requireNamespace("emmeans", quietly = TRUE)) {
        emmeans::emmeans(ancova_model, group_column)
      } else {
        NULL
      }
      emm_contrasts <- if (!is.null(emm_results) && requireNamespace("emmeans", quietly = TRUE)) {
        emmeans::pairs(emm_results)
      } else {
        NULL
      }
      
      # Model diagnostics
      diagnostics <- check_model_assumptions(ancova_model)
      
      # Effect sizes
      effect_sizes <- calculate_ancova_effect_sizes(ancova_model, ancova_anova)
      
      ancova_results[[var]] <- list(
        variable = var,
        model = ancova_model,
        model_summary = summary(ancova_model),
        anova_table = ancova_anova,
        estimated_marginal_means = list(
          means = emm_results,
          contrasts = emm_contrasts
        ),
        effect_sizes = effect_sizes,
        diagnostics = diagnostics,
        n_observations = nrow(clean_data),
        covariates_used = covariates
      )
      
    }, error = function(e) {
      ancova_results[[var]] <- list(
        variable = var,
        error = paste("Error in ANCOVA:", e$message)
      )
    })
  }
  
  return(ancova_results)
}

# Interaction terms analysis
perform_interaction_analysis <- function(data, dependent_vars, group_column, covariates) {
  
  interaction_results <- list()
  
  if (length(covariates) == 0) {
    cat("No covariates available for interaction analysis.\n")
    return(interaction_results)
  }
  
  for (var in dependent_vars) {
    cat("Testing interactions for", var, "...\n")
    
    var_interactions <- list()
    
    for (covariate in covariates) {
      # Test group × covariate interaction
      model_vars <- c(var, group_column, covariate)
      clean_data <- data[complete.cases(data[model_vars]), ]
      
      if (nrow(clean_data) < 20) {
        var_interactions[[covariate]] <- list(
          covariate = covariate,
          error = "Insufficient data for interaction analysis"
        )
        next
      }
      
      tryCatch({
        # Model without interaction
        formula_main <- as.formula(paste(var, "~", group_column, "+", covariate))
        model_main <- lm(formula_main, data = clean_data)
        
        # Model with interaction
        formula_interaction <- as.formula(paste(var, "~", group_column, "*", covariate))
        model_interaction <- lm(formula_interaction, data = clean_data)
        
        # Test significance of interaction
        interaction_test <- anova(model_main, model_interaction)
        
        # Extract interaction term p-value
        interaction_summary <- summary(model_interaction)
        interaction_coeff <- grep(":", rownames(interaction_summary$coefficients), value = TRUE)
        
        # Extract coefficient estimates and confidence intervals
        interaction_summary <- summary(model_interaction)
        interaction_coeff_table <- interaction_summary$coefficients
        
        # Get confidence intervals
        conf_intervals <- tryCatch({
          confint(model_interaction)
        }, error = function(e) {
          NULL
        })
        
        var_interactions[[covariate]] <- list(
          covariate = covariate,
          main_effects_model = model_main,
          interaction_model = model_interaction,
          interaction_test = interaction_test,
          interaction_significant = interaction_test$`Pr(>F)`[2] < 0.05,
          interaction_p_value = interaction_test$`Pr(>F)`[2],
          model_improvement = list(
            r_squared_change = summary(model_interaction)$r.squared - summary(model_main)$r.squared,
            f_change = interaction_test$F[2],
            p_change = interaction_test$`Pr(>F)`[2]
          ),
          coefficients = list(
            estimates = interaction_coeff_table,
            confidence_intervals = conf_intervals
          )
        )
        
        if (interaction_test$`Pr(>F)`[2] < 0.05) {
          cat("  - Significant", group_column, "×", covariate, "interaction (p =", 
              format.pval(interaction_test$`Pr(>F)`[2], digits = 3), ")\n")
        }
        
      }, error = function(e) {
        var_interactions[[covariate]] <- list(
          covariate = covariate,
          error = paste("Error testing interaction:", e$message)
        )
      })
    }
    
    interaction_results[[var]] <- var_interactions
  }
  
  return(interaction_results)
}

# Model comparison and selection
perform_model_comparison <- function(data, dependent_vars, group_column, covariates) {
  
  comparison_results <- list()
  
  for (var in dependent_vars) {
    cat("Comparing models for", var, "...\n")
    
    model_vars <- c(var, group_column, covariates)
    clean_data <- data[complete.cases(data[model_vars]), ]
    
    if (nrow(clean_data) < 15) {
      comparison_results[[var]] <- list(
        variable = var,
        error = "Insufficient data for model comparison"
      )
      next
    }
    
    tryCatch({
      models <- list()
      
      # Model 1: Intercept only
      models$intercept <- lm(as.formula(paste(var, "~ 1")), data = clean_data)
      
      # Model 2: Group only
      models$group_only <- lm(as.formula(paste(var, "~", group_column)), data = clean_data)
      
      # Model 3: Covariates only (if available)
      if (length(covariates) > 0) {
        models$covariates_only <- lm(as.formula(paste(var, "~", paste(covariates, collapse = " + "))), data = clean_data)
        
        # Model 4: Group + Covariates + Interactions (if any significant)
        # Check if any interactions are significant
        significant_interactions <- c()
        for (covariate in covariates) {
          interaction_formula <- as.formula(paste(var, "~", group_column, "*", covariate))
          interaction_model <- lm(interaction_formula, data = clean_data)
          interaction_anova <- anova(models$covariates_only, interaction_model)
          if (interaction_anova$`Pr(>F)`[2] < 0.1) {  # Use liberal threshold for model inclusion
            significant_interactions <- c(significant_interactions, paste(group_column, covariate, sep = ":"))
          }
        }
        
        if (length(significant_interactions) > 0) {
          interaction_terms <- paste(gsub(":", "*", significant_interactions), collapse = " + ")
          interaction_formula <- as.formula(paste(var, "~", group_column, "+", paste(covariates, collapse = " + "), "+", interaction_terms))
          models$with_interactions <- lm(interaction_formula, data = clean_data)
        }
      }
      
      # Compare models using AIC, BIC, and adjusted R-squared
      model_comparison <- compare_models(models)
      
      # Select best model
      best_model <- select_best_model(model_comparison)
      
      comparison_results[[var]] <- list(
        variable = var,
        models = models,
        comparison_table = model_comparison,
        best_model = best_model,
        n_observations = nrow(clean_data)
      )
      
    }, error = function(e) {
      comparison_results[[var]] <- list(
        variable = var,
        error = paste("Error in model comparison:", e$message)
      )
    })
  }
  
  return(comparison_results)
}

# Model assumption checking
check_model_assumptions <- function(model) {
  
  tryCatch({
    # Residual analysis
    residuals <- residuals(model)
    fitted_values <- fitted(model)
    
    # Normality of residuals
    shapiro_test <- if (length(residuals) <= 5000) shapiro.test(residuals) else NULL
    
    # Homoscedasticity (Breusch-Pagan test)
    bp_test <- if (requireNamespace("car", quietly = TRUE)) {
      car::ncvTest(model)
    } else {
      list(p = NA)
    }
    
    # Independence (Durbin-Watson test)
    dw_test <- if (requireNamespace("car", quietly = TRUE)) {
      car::durbinWatsonTest(model)
    } else {
      list(dw = NA, p = NA)
    }
    
    # Influential observations
    cooks_distance <- cooks.distance(model)
    influential_obs <- which(cooks_distance > 4/length(cooks_distance))
    
    return(list(
      normality = list(
        test = if (!is.null(shapiro_test)) "Shapiro-Wilk" else "Sample too large",
        p_value = if (!is.null(shapiro_test)) shapiro_test$p.value else NA,
        assumption_met = if (!is.null(shapiro_test)) shapiro_test$p.value > 0.05 else NA
      ),
      homoscedasticity = list(
        test = "Breusch-Pagan",
        p_value = bp_test$p,
        assumption_met = bp_test$p > 0.05
      ),
      independence = list(
        test = "Durbin-Watson",
        statistic = dw_test$dw,
        p_value = dw_test$p,
        assumption_met = dw_test$p > 0.05
      ),
      influential_observations = list(
        count = length(influential_obs),
        indices = influential_obs
      )
    ))
    
  }, error = function(e) {
    return(list(
      error = paste("Error checking assumptions:", e$message)
    ))
  })
}

# ANCOVA effect size calculations
calculate_ancova_effect_sizes <- function(model, anova_table) {
  
  tryCatch({
    # Extract sum of squares
    ss_total <- sum(anova_table$`Sum Sq`)
    
    effect_sizes <- list()
    for (term in rownames(anova_table)) {
      if (term != "Residuals") {
        ss_term <- anova_table[term, "Sum Sq"]
        eta_squared <- ss_term / ss_total
        
        # Partial eta squared
        ss_error <- anova_table["Residuals", "Sum Sq"]
        partial_eta_squared <- ss_term / (ss_term + ss_error)
        
        effect_sizes[[term]] <- list(
          eta_squared = eta_squared,
          partial_eta_squared = partial_eta_squared,
          interpretation = interpret_eta_squared(partial_eta_squared)
        )
      }
    }
    
    return(effect_sizes)
    
  }, error = function(e) {
    return(list(error = paste("Error calculating effect sizes:", e$message)))
  })
}

# Model comparison
compare_models <- function(models) {
  
  comparison_table <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    R_squared = numeric(),
    Adj_R_squared = numeric(),
    RMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model_name in names(models)) {
    model <- models[[model_name]]
    model_summary <- summary(model)
    
    comparison_table <- rbind(comparison_table, data.frame(
      Model = model_name,
      AIC = AIC(model),
      BIC = BIC(model),
      R_squared = model_summary$r.squared,
      Adj_R_squared = model_summary$adj.r.squared,
      RMSE = sqrt(mean(residuals(model)^2)),
      stringsAsFactors = FALSE
    ))
  }
  
  # Rank models
  comparison_table$AIC_rank <- rank(comparison_table$AIC)
  comparison_table$BIC_rank <- rank(comparison_table$BIC)
  comparison_table$Adj_R_squared_rank <- rank(-comparison_table$Adj_R_squared)
  
  return(comparison_table)
}

# Best model selection
select_best_model <- function(comparison_table) {
  
  # IMPROVED MODEL SELECTION LOGIC:
  # 1. Prefer models with statistical significance
  # 2. Among significant models, use composite scoring
  # 3. Only fall back to intercept if no other model shows improvement
  
  # Add F-test significance check for each model
  comparison_table$significant <- FALSE
  comparison_table$p_value <- NA
  
  # Check if any model other than intercept has meaningful R²
  non_intercept_models <- comparison_table[comparison_table$Model != "intercept", ]
  
  if (nrow(non_intercept_models) > 0) {
    # Look for models with meaningful R² (> 0.01) or good relative performance
    meaningful_models <- non_intercept_models[
      non_intercept_models$Adj_R_squared > 0.01 | 
      non_intercept_models$Adj_R_squared > max(comparison_table$Adj_R_squared[comparison_table$Model == "intercept"], na.rm = TRUE),
    ]
    
    if (nrow(meaningful_models) > 0) {
      # Select best among meaningful models using composite score
      meaningful_models$composite_score <- (meaningful_models$AIC_rank + meaningful_models$BIC_rank + meaningful_models$Adj_R_squared_rank) / 3
      best_index_in_meaningful <- which.min(meaningful_models$composite_score)
      
      # Find this model in the full table
      best_model_name <- meaningful_models$Model[best_index_in_meaningful]
      best_index <- which(comparison_table$Model == best_model_name)
      
      return(list(
        best_model_name = best_model_name,
        selection_criteria = comparison_table[best_index, ],
        rationale = paste0("Selected ", best_model_name, " over intercept model due to meaningful explanatory power (Adj R² = ", 
                          round(comparison_table$Adj_R_squared[best_index], 3), 
                          "), with good information criteria balance")
      ))
    }
  }
  
  # Fallback to original logic if no meaningful models found
  comparison_table$composite_score <- (comparison_table$AIC_rank + comparison_table$BIC_rank + comparison_table$Adj_R_squared_rank) / 3
  best_index <- which.min(comparison_table$composite_score)
  
  return(list(
    best_model_name = comparison_table$Model[best_index],
    selection_criteria = comparison_table[best_index, ],
    rationale = paste0("Selected based on composite score of AIC rank (", 
                      comparison_table$AIC_rank[best_index], 
                      "), BIC rank (", comparison_table$BIC_rank[best_index], 
                      "), and Adj R² rank (", comparison_table$Adj_R_squared_rank[best_index], 
                      "). Note: No model showed substantial improvement over intercept.")))
}

# Enhanced effect size calculations
calculate_enhanced_effect_sizes <- function(mlr_results, ancova_results) {
  
  effect_sizes <- list()
  
  # From multiple regression results
  for (var in names(mlr_results)) {
    result <- mlr_results[[var]]
    
    # Handle both full model (with covariates) and baseline model (group only) cases
    if (!is.null(result$full_model)) {
      # Case: covariates available, use full model
      model <- result$full_model$model
      r_squared <- result$full_model$r_squared
      
      # Cohen's f² for model
      cohens_f2 <- r_squared / (1 - r_squared)
      
      effect_sizes[[var]]$regression <- list(
        r_squared = r_squared,
        cohens_f_squared = cohens_f2,
        interpretation = interpret_cohens_f2(cohens_f2),
        model_type = "full_model_with_covariates"
      )
    } else if (!is.null(result$baseline_model)) {
      # Case: no covariates, use baseline model
      r_squared <- result$baseline_model$r_squared
      
      # Cohen's f² for model
      cohens_f2 <- r_squared / (1 - r_squared)
      
      effect_sizes[[var]]$regression <- list(
        r_squared = r_squared,
        cohens_f_squared = cohens_f2,
        interpretation = interpret_cohens_f2(cohens_f2),
        model_type = "baseline_model_groups_only"
      )
    }
  }
  
  # From ANCOVA results (only if covariates were available)
  for (var in names(ancova_results)) {
    result <- ancova_results[[var]]
    if (!is.null(result$effect_sizes)) {
      effect_sizes[[var]]$ancova <- result$effect_sizes
    }
  }
  
  return(effect_sizes)
}

# Eta squared interpretation
interpret_eta_squared <- function(eta_sq) {
  if (is.na(eta_sq)) return("Cannot interpret")
  if (eta_sq < 0.01) return("Negligible")
  if (eta_sq < 0.06) return("Small")
  if (eta_sq < 0.14) return("Medium")
  return("Large")
}

# Cohen's f² interpretation
interpret_cohens_f2 <- function(f2) {
  if (is.na(f2)) return("Cannot interpret")
  if (f2 < 0.02) return("Negligible")
  if (f2 < 0.15) return("Small")
  if (f2 < 0.35) return("Medium")
  return("Large")
}

# Create inferential plots
create_inferential_plots <- function(data, results, numeric_vars, group_column, output_path) {
  
  # Ensure required packages are available
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggpubr)
    library(gridExtra)
    if (requireNamespace("scales", quietly = TRUE)) library(scales)
  })
  
  # Prevent unwanted Rplots.pdf creation by ensuring no graphics device is open
  if (length(dev.list()) > 0) {
    graphics.off()
  }
  
  # Source plotting utilities if not already loaded
  if (!exists("ensure_clean_graphics_environment")) {
    source("modules/utils/plotting_utils.R")
  }
  ensure_clean_graphics_environment()
  
  # Create plots directory
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  # Debug output
  cat("Creating inferential plots...\n")
  cat("- Variables to plot:", paste(numeric_vars, collapse = ", "), "\n")
  cat("- Output path:", output_path, "\n")
  
  # 1. Model comparison plots (ENHANCED - works with any number of models)
  if (!is.null(results$model_comparison)) {
    cat("- Creating model comparison plots for", length(results$model_comparison), "variables\n")
    
    for (var in names(results$model_comparison)) {
      result <- results$model_comparison[[var]]
      if (!is.null(result$comparison_table) && nrow(result$comparison_table) > 0) {
        cat("  Creating plots for", var, "with", nrow(result$comparison_table), "models\n")
        
        # Create model comparison plot
        comp_table <- result$comparison_table
        
        # Ensure Model is a factor for proper ordering
        comp_table$Model <- factor(comp_table$Model, levels = comp_table$Model)
        
        p1 <- ggplot(comp_table, aes(x = Model, y = AIC)) +
          geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
          geom_text(aes(label = round(AIC, 1)), vjust = -0.5, size = 3) +
          labs(title = paste("Model Comparison for", var), 
               subtitle = "AIC values (lower is better)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 12, face = "bold"))
        
        p2 <- ggplot(comp_table, aes(x = Model, y = Adj_R_squared)) +
          geom_col(fill = "darkgreen", alpha = 0.7, width = 0.6) +
          geom_text(aes(label = round(Adj_R_squared, 3)), vjust = -0.5, size = 3) +
          labs(title = "Adjusted R²", subtitle = "Higher is better") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 12, face = "bold"))
        
        # Combine plots without auto-display to prevent unwanted Rplots.pdf creation
        combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
        
        plot_filename <- file.path(output_path, paste0("model_comparison_", var, ".png"))
        ggsave(plot_filename, combined_plot, width = 12, height = 6, dpi = 300)
        
        plots[[paste0("model_comparison_", var)]] <- combined_plot
        plot_files[[paste0("model_comparison_", var)]] <- plot_filename
        cat("    Saved:", plot_filename, "\n")
      } else {
        cat("  Skipping", var, "- no comparison table available\n")
      }
    }
  } else {
    cat("- No model comparison results available\n")
  }
  
  # 2. Effect sizes visualization
  if (!is.null(results$effect_sizes)) {
    cat("- Creating effect sizes visualization\n")
    
    # Extract R-squared values for plotting
    effect_data <- data.frame(
      Variable = character(),
      R_squared = numeric(),
      Effect_Size = character(),
      stringsAsFactors = FALSE
    )
    
    for (var in names(results$effect_sizes)) {
      if (!is.null(results$effect_sizes[[var]]$regression)) {
        r_sq <- results$effect_sizes[[var]]$regression$r_squared
        interpretation <- results$effect_sizes[[var]]$regression$interpretation
        
        effect_data <- rbind(effect_data, data.frame(
          Variable = var,
          R_squared = r_sq,
          Effect_Size = interpretation,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (nrow(effect_data) > 0) {
      # Order by R-squared
      effect_data <- effect_data[order(effect_data$R_squared, decreasing = TRUE), ]
      effect_data$Variable <- factor(effect_data$Variable, levels = effect_data$Variable)
      
      p_effects <- ggplot(effect_data, aes(x = Variable, y = R_squared, fill = Effect_Size)) +
        geom_col(alpha = 0.8, width = 0.7) +
        geom_text(aes(label = paste0(round(R_squared * 100, 1), "%")), 
                  vjust = -0.5, size = 3) +
        labs(title = "Effect Sizes (R² values) by Variable",
             subtitle = "Proportion of variance explained by group membership",
             x = "Variables", y = "R² (Proportion of Variance Explained)",
             fill = "Effect Size") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 14, face = "bold"),
              legend.position = "bottom") +
                 scale_fill_brewer(type = "qual", palette = "Set2") +
         scale_y_continuous(labels = if(requireNamespace("scales", quietly = TRUE)) scales::percent_format() else function(x) paste0(round(x*100, 1), "%"))
      
      plot_filename <- file.path(output_path, "effect_sizes_summary.png")
      ggsave(plot_filename, p_effects, width = 10, height = 6, dpi = 300)
      
      plots[["effect_sizes"]] <- p_effects
      plot_files[["effect_sizes"]] <- plot_filename
      cat("    Saved:", plot_filename, "\n")
    }
  }
  
  # 3. Group differences visualization (boxplots with effect sizes)
  if (length(numeric_vars) > 0) {
    cat("- Creating group differences plots\n")
    
    # Create boxplots for top variables (by effect size if available)
    vars_to_plot <- numeric_vars[1:min(4, length(numeric_vars))]
    
    # Reorder by effect size if available
    if (!is.null(results$effect_sizes)) {
      effect_order <- names(results$effect_sizes)[order(sapply(names(results$effect_sizes), function(v) {
        if (!is.null(results$effect_sizes[[v]]$regression)) {
          results$effect_sizes[[v]]$regression$r_squared
        } else {
          0
        }
      }), decreasing = TRUE)]
      vars_to_plot <- intersect(effect_order, numeric_vars)[1:min(4, length(intersect(effect_order, numeric_vars)))]
    }
    
    for (var in vars_to_plot) {
      tryCatch({
        # Get effect size for subtitle
        effect_info <- ""
        if (!is.null(results$effect_sizes) && !is.null(results$effect_sizes[[var]]$regression)) {
          r_sq <- results$effect_sizes[[var]]$regression$r_squared
          interpretation <- results$effect_sizes[[var]]$regression$interpretation
          effect_info <- paste0(" (R² = ", round(r_sq, 3), ", ", interpretation, " effect)")
        }
        
        p_box <- ggplot(data, aes(x = .data[[group_column]], y = .data[[var]], fill = .data[[group_column]])) +
          geom_boxplot(alpha = 0.7, outlier.alpha = 0.6) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.5, size = 1) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          labs(title = paste("Group Differences:", var),
               subtitle = paste0("Boxplots with individual data points", effect_info),
               x = group_column, y = var) +
          theme_minimal() +
          theme(plot.title = element_text(size = 12, face = "bold"),
                legend.position = "none") +
          scale_fill_brewer(type = "qual", palette = "Set2")
        
        # Add statistical comparison if possible
        if (requireNamespace("ggpubr", quietly = TRUE)) {
          groups <- unique(data[[group_column]])
          groups <- groups[!is.na(groups)]
          if (length(groups) > 1) {
            p_box <- p_box + ggpubr::stat_compare_means(method = if(length(groups) > 2) "anova" else "t.test",
                                                        label.y = max(data[[var]], na.rm = TRUE) * 1.1)
          }
        }
        
        plot_filename <- file.path(output_path, paste0("group_differences_", var, ".png"))
        ggsave(plot_filename, p_box, width = 8, height = 6, dpi = 300)
        
        plots[[paste0("group_differences_", var)]] <- p_box
        plot_files[[paste0("group_differences_", var)]] <- plot_filename
        cat("    Saved:", plot_filename, "\n")
        
      }, error = function(e) {
        cat("    Error creating plot for", var, ":", e$message, "\n")
      })
    }
  }
  
  # 4. Model diagnostics plots (for best models)
  if (!is.null(results$multiple_regression)) {
    cat("- Creating model diagnostics plots\n")
    
    # Select top 2 variables for diagnostics
    vars_for_diagnostics <- names(results$multiple_regression)[1:min(2, length(results$multiple_regression))]
    
    for (var in vars_for_diagnostics) {
      reg_result <- results$multiple_regression[[var]]
      
      # Use the available model (full_model or baseline_model)
      model <- NULL
      if (!is.null(reg_result$full_model)) {
        model <- reg_result$full_model$model
      } else if (!is.null(reg_result$baseline_model)) {
        model <- reg_result$baseline_model$model
      }
      
      if (!is.null(model)) {
        tryCatch({
          # Create diagnostic plots
          residuals_val <- residuals(model)
          fitted_val <- fitted(model)
          
          # Residuals vs Fitted
          p1 <- ggplot(data.frame(fitted = fitted_val, residuals = residuals_val), 
                       aes(x = fitted, y = residuals)) +
            geom_point(alpha = 0.6) +
            geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
            geom_smooth(se = FALSE, color = "blue") +
            labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
            theme_minimal()
          
          # Q-Q plot
          p2 <- ggplot(data.frame(residuals = residuals_val), aes(sample = residuals)) +
            stat_qq(alpha = 0.6) +
            stat_qq_line(color = "red") +
            labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
            theme_minimal()
          
          # Combine diagnostic plots
          combined_diagnostics <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
          
          plot_filename <- file.path(output_path, paste0("diagnostics_", var, ".png"))
          ggsave(plot_filename, combined_diagnostics, width = 12, height = 5, dpi = 300)
          
          plots[[paste0("diagnostics_", var)]] <- combined_diagnostics
          plot_files[[paste0("diagnostics_", var)]] <- plot_filename
          cat("    Saved:", plot_filename, "\n")
          
        }, error = function(e) {
          cat("    Error creating diagnostics for", var, ":", e$message, "\n")
        })
      }
    }
  }
  
  total_plots <- length(plots)
  cat("Enhanced inferential plots completed:", total_plots, "plots generated\n")
  
  return(list(plots = plots, plot_files = plot_files))
}

# Convenience function for quick enhanced inferential analysis
quick_enhanced_inferential <- function(data, group_column, generate_report = TRUE, 
                                     include_interactions = "auto", check_assumptions = TRUE) {
  
  # Run enhanced inferential analysis with assumptions testing
  result <- perform_enhanced_inferential_analysis(data, group_column, include_plots = TRUE, 
                                                 include_interactions = include_interactions,
                                                 check_assumptions = check_assumptions)
  
  # Generate report if requested
  if (generate_report) {
    # Note: Would need to create specific report template for this analysis
    cat("Enhanced inferential analysis completed. Results stored in object.\n")
    cat("Use generate_enhanced_inferential_report(result) to create HTML report.\n")
    
    # Display quick summary of assumptions if checked
    if (check_assumptions && !is.null(result$assumptions_analysis)) {
      cat("\n=== QUICK ASSUMPTIONS SUMMARY ===\n")
      assumptions_summary <- result$assumptions_analysis$assumptions_summary
      
      if (!is.null(assumptions_summary)) {
        normal_count <- sum(assumptions_summary$Overall_Normal, na.rm = TRUE)
        total_vars <- nrow(assumptions_summary)
        borderline_count <- sum(assumptions_summary$Borderline, na.rm = TRUE)
        
        cat("Variables tested:", total_vars, "\n")
        cat("Normal distributions:", normal_count, "/", total_vars, "\n")
        if (borderline_count > 0) {
          cat("WARNING -   Borderline cases:", borderline_count, "\n")
        }
        cat("See result$assumptions_analysis for detailed results.\n")
      }
    }
  }
  
  return(result)
}

# NOWA FUNKCJA: Display assumption summary with recommendations
display_assumption_summary <- function(assumptions_results) {
  
  if (is.null(assumptions_results$assumptions_summary)) {
    return()
  }
  
  summary_table <- assumptions_results$assumptions_summary
  
  cat("\n--- ASSUMPTIONS SUMMARY ---\n")
  
  # Check for assumption violations
  violations <- list()
  borderline_cases <- list()
  
  for (i in 1:nrow(summary_table)) {
    var <- summary_table$Variable[i]
    
    # Check normality violations
    if (!summary_table$Overall_Normal[i]) {
      violations[[var]] <- c(violations[[var]], "Non-normal distribution")
    }
    
    # Check borderline normality (Task H improvement)
    if (summary_table$Borderline[i]) {
      borderline_cases[[var]] <- c(borderline_cases[[var]], "Borderline normality")
    }
    
    # Check homogeneity violations
    if (!grepl("homogeneous|N/A", summary_table$Homogeneity[i], ignore.case = TRUE)) {
      violations[[var]] <- c(violations[[var]], "Heterogeneous variances")
    }
  }
  
  # Display violations
  if (length(violations) > 0) {
    cat("WARNING -   ASSUMPTION VIOLATIONS DETECTED:\n")
    for (var in names(violations)) {
      cat("  ", var, ":", paste(violations[[var]], collapse = ", "), "\n")
    }
  }
  
  # Display borderline cases
  if (length(borderline_cases) > 0) {
    cat("WARNING -   BORDERLINE CASES (require attention):\n")
    for (var in names(borderline_cases)) {
      cat("  ", var, ":", paste(borderline_cases[[var]], collapse = ", "), "\n")
    }
  }
  
  # Display test recommendations if available
  if (!is.null(assumptions_results$test_recommendations)) {
    cat("\n--- RECOMMENDED STATISTICAL TESTS ---\n")
    
    for (var in names(assumptions_results$test_recommendations)) {
      rec <- assumptions_results$test_recommendations[[var]]
      cat("• ", var, ":", rec$primary_test, "\n")
      if (rec$assumption_flag %in% c("BORDERLINE_NORMAL", "BORDERLINE_NON_NORMAL")) {
        cat(" Action: ", rec$borderline_action, "\n")
      }
    }
  }
  
  cat("\n")
}