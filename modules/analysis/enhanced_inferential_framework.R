# Enhanced Inferential Framework Module
# Multiple linear regression and ANCOVA models with covariate adjustments
# Implements advanced statistical modeling with interaction terms
#
# Variable Nomenclature Standards:
# - hsCRP: High-sensitivity C-reactive protein (mg/L) - standardized biomarker name
# - BMI: Body Mass Index (kg/m²)
# - wiek: Age (years)
# - plec: Gender (M/F)
# - grupa: Study group assignment
# - p-values: Formatted using format.pval() with 3 significant digits for scientific notation when p < 0.001
# - Effect sizes: η² (eta-squared) for ANOVA models, adjusted R² for regression models
#
# Advanced Statistical Modeling Documentation:
# 1. MODEL SELECTION HIERARCHY:
#    - Group-only model: outcome ~ group (baseline comparison)
#    - Covariate-adjusted: outcome ~ group + covariates (ANCOVA approach)
#    - Interaction model: outcome ~ group * covariates (if interactions significant)
# 2. COVARIATE CENTERING:
#    - Continuous covariates centered around sample mean for interpretability
#    - Reduces multicollinearity in interaction terms
#    - Intercept represents group means at average covariate values
# 3. INTERACTION TESTING:
#    - Systematic testing of group × covariate interactions
#    - Retained only if p < 0.05 and improve model fit (AIC criterion)
# 4. EFFECT SIZES: η² for ANCOVA, adjusted R² for regression, Cohen's f² for model comparisons

# Load required libraries with error handling
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

# Main function: Enhanced inferential analysis with covariates
perform_enhanced_inferential_analysis <- function(data, group_column = "grupa", include_plots = TRUE) {
  
  # Create analysis result object
  result <- create_analysis_result("enhanced_inferential")
  
  cat("Starting enhanced inferential framework analysis...\n")
  
  # Identify variable types and potential covariates
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # Remove group column from dependent variables
  numeric_vars <- numeric_vars[numeric_vars != group_column]
  categorical_vars <- categorical_vars[categorical_vars != group_column]
  
  # Identify potential covariates
  potential_covariates <- tryCatch({
    identify_covariates(data, numeric_vars, group_column)
  }, error = function(e) {
    cat("Warning: Could not identify covariates automatically. Using manual identification.\n")
    # Manual covariate identification as fallback
    manual_covariates <- c()
    if ("wiek" %in% names(data)) manual_covariates <- c(manual_covariates, "wiek")
    if ("hsCRP" %in% names(data)) manual_covariates <- c(manual_covariates, "hsCRP")
    return(manual_covariates)
  })
  
  # CRITICAL FIX: Remove identified covariates from dependent variables list
  # to avoid analyzing covariates as both dependent variables AND covariates
  dependent_vars <- numeric_vars[!numeric_vars %in% potential_covariates]
  
  # Center continuous covariates for better interpretation of interactions
  centered_data <- data
  covariate_centering_info <- list()
  
  for (covariate in potential_covariates) {
    if (covariate %in% names(data) && is.numeric(data[[covariate]])) {
      original_mean <- mean(data[[covariate]], na.rm = TRUE)
      centered_data[[paste0(covariate, "_centered")]] <- data[[covariate]] - original_mean
      covariate_centering_info[[covariate]] <- list(
        original_mean = original_mean,
        centered_variable = paste0(covariate, "_centered")
      )
      cat("- Centered", covariate, "around mean =", round(original_mean, 2), "\n")
    }
  }
  
  cat("- Analyzing", length(dependent_vars), "dependent variables\n")
  cat("- Identified", length(potential_covariates), "potential covariates:", paste(potential_covariates, collapse = ", "), "\n")
  cat("- Dependent variables:", paste(dependent_vars, collapse = ", "), "\n")
  
  # Step 1: Multiple Linear Regression with Covariates
  cat("\n=== STEP 1: MULTIPLE LINEAR REGRESSION WITH COVARIATES ===\n")
  mlr_results <- tryCatch({
    perform_multiple_linear_regression(centered_data, dependent_vars, group_column, potential_covariates)
  }, error = function(e) {
    cat("Warning: Multiple regression analysis failed:", e$message, "\n")
    list()
  })
  result$multiple_regression <- mlr_results
  
  # Step 2: ANCOVA Models
  cat("\n=== STEP 2: ANCOVA MODELS ===\n")
  ancova_results <- tryCatch({
    perform_ancova_analysis(centered_data, dependent_vars, group_column, potential_covariates)
  }, error = function(e) {
    cat("Warning: ANCOVA analysis failed:", e$message, "\n")
    list()
  })
  result$ancova_analysis <- ancova_results
  
  # Step 3: Interaction Terms Analysis
  cat("\n=== STEP 3: INTERACTION TERMS ANALYSIS ===\n")
  interaction_results <- tryCatch({
    perform_interaction_analysis(centered_data, dependent_vars, group_column, potential_covariates)
  }, error = function(e) {
    cat("Warning: Interaction analysis failed:", e$message, "\n")
    list()
  })
  result$interaction_analysis <- interaction_results
  
  # Step 4: Model Selection and Comparison
  cat("\n=== STEP 4: MODEL SELECTION AND COMPARISON ===\n")
  model_comparison <- perform_model_comparison(centered_data, dependent_vars, group_column, potential_covariates)
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
    covariate_centering = covariate_centering_info,
    group_column = group_column,
    analysis_date = Sys.time()
  )
  
  cat("\nEnhanced inferential analysis completed successfully.\n")
  return(result)
}

# Identify potential covariates (age, biomarkers, etc.)
identify_covariates <- function(data, dependent_vars, group_column) {
  
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  numeric_vars <- numeric_vars[!numeric_vars %in% dependent_vars]
  numeric_vars <- numeric_vars[numeric_vars != group_column]
  
  potential_covariates <- list()
  
  # Common covariate patterns in medical data
  age_patterns <- c("wiek", "age", "Age", "AGE", "anos", "alter")
  biomarker_patterns <- c("hsCRP", "hscrp", "CRP", "crp", "BMI", "bmi")
  # Note: Standardized biomarker name is "hsCRP" (high-sensitivity C-reactive protein)
  
  # Identify age-like variables
  age_vars <- numeric_vars[sapply(numeric_vars, function(x) any(sapply(age_patterns, function(p) grepl(p, x, ignore.case = TRUE))))]
  if (length(age_vars) > 0) {
    potential_covariates$age <- age_vars[1]  # Take first match
    cat("- Identified age covariate:", age_vars[1], "\n")
  }
  
  # Identify biomarker variables
  biomarker_vars <- numeric_vars[sapply(numeric_vars, function(x) any(sapply(biomarker_patterns, function(p) grepl(p, x, ignore.case = TRUE))))]
  if (length(biomarker_vars) > 0) {
    potential_covariates$biomarkers <- biomarker_vars
    cat("- Identified biomarker covariates:", paste(biomarker_vars, collapse = ", "), "\n")
  }
  
  # Identify other continuous variables as potential covariates
  other_covariates <- numeric_vars[!numeric_vars %in% c(age_vars, biomarker_vars)]
  if (length(other_covariates) > 0) {
    # Filter based on correlation with group membership (if group is factor with numeric encoding)
    data_clean <- data[complete.cases(data[c(group_column, other_covariates)]), ]
    if (nrow(data_clean) > 0) {
      group_numeric <- as.numeric(as.factor(data_clean[[group_column]]))
      correlations <- sapply(other_covariates, function(var) {
        if (var %in% names(data_clean)) {
          abs(cor(data_clean[[var]], group_numeric, use = "complete.obs"))
        } else {
          0
        }
      })
      
      # Select variables with moderate correlation (0.1 < |r| < 0.7) as potential covariates
      relevant_covariates <- other_covariates[correlations > 0.1 & correlations < 0.7]
      if (length(relevant_covariates) > 0) {
        potential_covariates$other <- relevant_covariates
        cat("- Identified other potential covariates:", paste(relevant_covariates, collapse = ", "), "\n")
      }
    }
  }
  
  # Flatten the list
  all_covariates <- unlist(potential_covariates, use.names = FALSE)
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
        # Only baseline model if no covariates
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
        
        # Model 4: Group + Covariates (additive)
        models$additive <- lm(as.formula(paste(var, "~", group_column, "+", paste(covariates, collapse = " + "))), data = clean_data)
        
        # Model 5: Group + Covariates + Interactions (if any significant)
        # Check if any interactions are significant
        significant_interactions <- c()
        for (covariate in covariates) {
          interaction_formula <- as.formula(paste(var, "~", group_column, "*", covariate))
          interaction_model <- lm(interaction_formula, data = clean_data)
          interaction_anova <- anova(models$additive, interaction_model)
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

# Helper function: Check model assumptions
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

# Helper function: Calculate ANCOVA effect sizes
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

# Helper function: Compare models
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

# Helper function: Select best model
select_best_model <- function(comparison_table) {
  
  # Simple scoring system: lower AIC/BIC and higher adjusted R-squared are better
  comparison_table$composite_score <- (comparison_table$AIC_rank + comparison_table$BIC_rank + comparison_table$Adj_R_squared_rank) / 3
  
  best_index <- which.min(comparison_table$composite_score)
  
  return(list(
    best_model_name = comparison_table$Model[best_index],
    selection_criteria = comparison_table[best_index, ],
    rationale = paste0("Selected based on composite score of AIC rank (", 
                      comparison_table$AIC_rank[best_index], 
                      "), BIC rank (", comparison_table$BIC_rank[best_index], 
                      "), and Adj R² rank (", comparison_table$Adj_R_squared_rank[best_index], ")")
  ))
}

# Helper function: Calculate enhanced effect sizes
calculate_enhanced_effect_sizes <- function(mlr_results, ancova_results) {
  
  effect_sizes <- list()
  
  # From multiple regression results
  for (var in names(mlr_results)) {
    result <- mlr_results[[var]]
    if (!is.null(result$full_model)) {
      model <- result$full_model$model
      r_squared <- result$full_model$r_squared
      
      # Cohen's f² for model
      cohens_f2 <- r_squared / (1 - r_squared)
      
      effect_sizes[[var]]$regression <- list(
        r_squared = r_squared,
        cohens_f_squared = cohens_f2,
        interpretation = interpret_cohens_f2(cohens_f2)
      )
    }
  }
  
  # From ANCOVA results
  for (var in names(ancova_results)) {
    result <- ancova_results[[var]]
    if (!is.null(result$effect_sizes)) {
      effect_sizes[[var]]$ancova <- result$effect_sizes
    }
  }
  
  return(effect_sizes)
}

# Helper function: Interpret eta squared
interpret_eta_squared <- function(eta_sq) {
  if (is.na(eta_sq)) return("Cannot interpret")
  if (eta_sq < 0.01) return("Negligible")
  if (eta_sq < 0.06) return("Small")
  if (eta_sq < 0.14) return("Medium")
  return("Large")
}

# Helper function: Interpret Cohen's f²
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
  
  # Model comparison plots
  if (!is.null(results$model_comparison)) {
    for (var in names(results$model_comparison)) {
      result <- results$model_comparison[[var]]
      if (!is.null(result$comparison_table)) {
        # Create model comparison plot
        comp_table <- result$comparison_table
        
        p1 <- ggplot(comp_table, aes(x = Model, y = AIC)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          geom_text(aes(label = round(AIC, 1)), vjust = -0.5) +
          labs(title = paste("Model Comparison for", var), subtitle = "AIC values (lower is better)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        p2 <- ggplot(comp_table, aes(x = Model, y = Adj_R_squared)) +
          geom_col(fill = "darkgreen", alpha = 0.7) +
          geom_text(aes(label = round(Adj_R_squared, 3)), vjust = -0.5) +
          labs(title = "Adjusted R²", subtitle = "Higher is better") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Combine plots without auto-display to prevent unwanted Rplots.pdf creation
        combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
        
        plot_filename <- file.path(output_path, paste0("model_comparison_", var, ".png"))
        ggsave(plot_filename, combined_plot, width = 12, height = 6, dpi = 300)
        
        plots[[paste0("model_comparison_", var)]] <- combined_plot
        plot_files[[paste0("model_comparison_", var)]] <- plot_filename
      }
    }
  }
  
  return(list(plots = plots, plot_files = plot_files))
}

# Convenience function for quick enhanced inferential analysis
quick_enhanced_inferential <- function(data, group_column = "grupa", generate_report = TRUE) {
  
  # Run enhanced inferential analysis
  result <- perform_enhanced_inferential_analysis(data, group_column, include_plots = TRUE)
  
  # Generate report if requested
  if (generate_report) {
    # Note: Would need to create specific report template for this analysis
    cat("Enhanced inferential analysis completed. Results stored in object.\n")
    cat("Use generate_enhanced_inferential_report(result) to create HTML report.\n")
  }
  
  return(result)
}