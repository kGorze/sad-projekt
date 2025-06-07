# Comparative Analysis Module
# Functions for performing statistical comparisons between independent groups
# Supports multiple groups (>2) with appropriate statistical tests

# Load required libraries with error handling
# NOTE: Packages are now loaded centrally in config.R - no individual loading needed

# Source reporting utilities
source("modules/reporting/export_results.R")

# Source descriptive statistics utilities for helper functions
source("modules/analysis/descriptive_stats.R")

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
  
  # Step 1: Assess distributions and assumptions
  cat("\n=== STEP 1: DISTRIBUTION ANALYSIS ===\n")
  distribution_results <- assess_distributions(data, numeric_vars, group_column)
  result$distribution_analysis <- distribution_results
  
  # Step 2: Test homogeneity of variances
  cat("\n=== STEP 2: HOMOGENEITY ANALYSIS ===\n")
  homogeneity_results <- assess_homogeneity(data, numeric_vars, group_column)
  result$homogeneity_analysis <- homogeneity_results
  
  # Step 3: Determine appropriate tests and perform comparisons
  cat("\n=== STEP 3: STATISTICAL COMPARISONS ===\n")
  comparison_results <- perform_statistical_comparisons(data, numeric_vars, categorical_vars, 
                                                       group_column, distribution_results, 
                                                       homogeneity_results)
  result$test_results <- comparison_results$test_results
  result$test_recommendations <- comparison_results$recommendations
  
  # Step 3.5: Calculate Cohen's D effect sizes for all numeric variables
  cat("\n=== STEP 3.5: EFFECT SIZE ANALYSIS (COHEN'S D) ===\n")
  effect_size_results <- list()
  for (var in numeric_vars) {
    cat("Calculating Cohen's D for", var, "...\n")
    cohens_d_result <- calculate_cohens_d(data, var, group_column)
    effect_size_results[[var]] <- cohens_d_result
  }
  result$effect_sizes <- effect_size_results
  
  # Step 3.6: Linear regression analysis for numeric variables
  cat("\n=== STEP 3.6: LINEAR REGRESSION ANALYSIS ===\n")
  regression_results <- list()
  for (var in numeric_vars) {
    cat("Performing linear regression for", var, "...\n")
    lm_result <- perform_linear_regression(data, var, group_column)
    regression_results[[var]] <- lm_result
  }
  result$regression_analysis <- regression_results
  
  # Step 4: Generate plots if requested
  if (include_plots) {
    cat("\n=== STEP 4: GENERATING VISUALIZATIONS ===\n")
    
    # Use fixed output path for plots
    plots_output_path <- file.path("output", "plots", "comparative_analysis")
    
    plots_result <- create_comparative_plots(data, numeric_vars, categorical_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
    cat("- Generated", length(plots_result$plots), "comparison plots\n")
    cat("- Saved", length(plots_result$plot_files), "plot files\n")
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
    # Kolmogorov-Smirnov test for larger samples
    test_result <- ks.test(clean_var, "pnorm", mean(clean_var), sd(clean_var))
    test_name <- "Kolmogorov-Smirnov"
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
                                    round(levene_result$`Pr(>F)`[1], 4), ")"),
                              paste("Variances are heterogeneous (Levene's test p =", 
                                    round(levene_result$`Pr(>F)`[1], 4), ")"))
      
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
  formula_str <- paste(variable, "~", group_column)
  anova_result <- aov(as.formula(formula_str), data = clean_data)
  anova_summary <- summary(anova_result)
  
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  f_statistic <- anova_summary[[1]]$`F value`[1]
  
  # Calculate effect size (eta-squared)
  ss_between <- anova_summary[[1]]$`Sum Sq`[1]
  ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
  eta_squared <- ss_between / ss_total
  
  # Post-hoc tests if significant
  posthoc_result <- NULL
  if (!is.na(p_value) && p_value < 0.05) {
    posthoc_result <- perform_tukey_hsd(anova_result)
  }
  
  interpretation <- ifelse(p_value < 0.05,
                          paste("Significant difference between groups (F =", round(f_statistic, 3), 
                                ", p =", round(p_value, 4), ")"),
                          paste("No significant difference between groups (F =", round(f_statistic, 3), 
                                ", p =", round(p_value, 4), ")"))
  
  return(list(
    variable = variable,
    test_name = "One-way ANOVA",
    statistic = f_statistic,
    p_value = p_value,
    effect_size = eta_squared,
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
  formula_str <- paste(variable, "~", group_column)
  welch_result <- oneway.test(as.formula(formula_str), data = clean_data, var.equal = FALSE)
  
  interpretation <- ifelse(welch_result$p.value < 0.05,
                          paste("Significant difference between groups (Welch F =", 
                                round(welch_result$statistic, 3), ", p =", 
                                round(welch_result$p.value, 4), ")"),
                          paste("No significant difference between groups (Welch F =", 
                                round(welch_result$statistic, 3), ", p =", 
                                round(welch_result$p.value, 4), ")"))
  
  return(list(
    variable = variable,
    test_name = "Welch's ANOVA",
    statistic = welch_result$statistic,
    p_value = welch_result$p.value,
    interpretation = interpretation
  ))
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
  kw_result <- kruskal.test(clean_data[[variable]], clean_data[[group_column]])
  
  # Post-hoc tests if significant (Dunn's test)
  posthoc_result <- NULL
  if (!is.na(kw_result$p.value) && kw_result$p.value < 0.05) {
    posthoc_result <- perform_dunn_test(clean_data, variable, group_column)
  }
  
  interpretation <- ifelse(kw_result$p.value < 0.05,
                          paste("Significant difference between groups (χ² =", 
                                round(kw_result$statistic, 3), ", p =", 
                                round(kw_result$p.value, 4), ")"),
                          paste("No significant difference between groups (χ² =", 
                                round(kw_result$statistic, 3), ", p =", 
                                round(kw_result$p.value, 4), ")"))
  
  return(list(
    variable = variable,
    test_name = "Kruskal-Wallis",
    statistic = kw_result$statistic,
    p_value = kw_result$p.value,
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
                                  round(fisher_result$p.value, 4), ")"),
                            paste("No significant association (Fisher's exact test p =", 
                                  round(fisher_result$p.value, 4), ")"))
    
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
                                  ", p =", round(chi_result$p.value, 4), ")"),
                            paste("No significant association (χ² =", round(chi_result$statistic, 3), 
                                  ", p =", round(chi_result$p.value, 4), ")"))
    
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

      # Create enhanced boxplot with statistical tests
      p <- ggboxplot(data, x = group_column, y = var,
                     color = group_column, palette = "jco",
                     add = "jitter", add.params = list(alpha = 0.3)) +
        stat_compare_means(method = overall_method,
                           label.y = max(data[[var]], na.rm = TRUE) * 1.1) +
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
      # Calculate group means for vertical lines using base R
      group_means <- aggregate(data[[var]], by = list(group = data[[group_column]]), 
                              FUN = function(x) mean(x, na.rm = TRUE), na.action = na.pass)
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
            stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
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
  
  # 6. Correlation matrix heatmap (for numeric variables)
  if (length(numeric_vars) >= 3) {
    tryCatch({
      # Calculate correlation matrix
      cor_data <- data[, numeric_vars, drop = FALSE]
      cor_matrix <- cor(cor_data, use = "complete.obs")
      
      # Convert to long format for ggplot
      cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_long$value <- as.vector(cor_matrix)
      
      p <- ggplot(cor_long, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                            midpoint = 0, limit = c(-1,1), space = "Lab",
                            name = "Correlation") +
        labs(title = "Correlation Matrix Heatmap",
             subtitle = "Pearson correlations between numeric variables",
             x = "", y = "") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1))
      
      plot_filename <- file.path(output_path, paste0("correlation_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[["correlation_heatmap"]] <- p
      plot_files[["correlation_heatmap"]] <- plot_filename
      cat("Created correlation heatmap\n")
      
    }, error = function(e) {
      cat("Error creating correlation heatmap:", e$message, "\n")
    })
  }
  
  # 7. Enhanced bar plots for categorical variables
  for (var in categorical_vars) {
    if (length(unique(data[[var]])) <= 10) {
      tryCatch({
        # Calculate proportions using base R
        temp_data <- aggregate(rep(1, nrow(data)), 
                             by = list(group = data[[group_column]], var = data[[var]]), 
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
      
      # Get data for each group
      data1 <- data[data[[group_column]] == group1 & !is.na(data[[group_column]]), variable]
      data2 <- data[data[[group_column]] == group2 & !is.na(data[[group_column]]), variable]
      
      # Remove missing values
      data1 <- data1[!is.na(data1)]
      data2 <- data2[!is.na(data2)]
      
      if (length(data1) > 1 && length(data2) > 1) {
        # Calculate Cohen's D using effsize package
        tryCatch({
          cohens_d_result <- cohen.d(data1, data2)
          d_value <- cohens_d_result$estimate
          
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
            magnitude = interpretation,
            confidence_interval = cohens_d_result$conf.int,
            group1 = group1,
            group2 = group2,
            n1 = length(data1),
            n2 = length(data2)
          )
          
        }, error = function(e) {
          comparison_name <- paste(group1, "vs", group2)
          effect_sizes[[comparison_name]] <- list(
            cohens_d = NA,
            magnitude = "calculation error",
            error = e$message
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
    ", p = ", round(results$p_value, 4), ")"
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