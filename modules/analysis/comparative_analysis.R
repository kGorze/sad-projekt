# Comparative Analysis Module
# Functions for performing statistical comparisons between independent groups
# Supports multiple groups (>2) with appropriate statistical tests

# Load required libraries with error handling
if (!require(car, quietly = TRUE)) {
  install.packages("car", repos = "https://cran.r-project.org")
  library(car)
}

if (!require(dunn.test, quietly = TRUE)) {
  install.packages("dunn.test", repos = "https://cran.r-project.org")
  library(dunn.test)
}

if (!require(effsize, quietly = TRUE)) {
  install.packages("effsize", repos = "https://cran.r-project.org")
  library(effsize)
}

if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cran.r-project.org")
  library(dplyr)
}

if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org")
  library(ggplot2)
}

# Source reporting utilities
source("modules/reporting/export_results.R")

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

# Create comparison plots
create_comparative_plots <- function(data, numeric_vars, categorical_vars, group_column, output_path = "output/plots/") {
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  # Box plots for numeric variables (limit to first 6 to avoid too many plots)
  for (var in numeric_vars[1:min(6, length(numeric_vars))]) {
    tryCatch({
      p <- ggplot(data, aes(x = .data[[group_column]], y = .data[[var]], fill = .data[[group_column]])) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(title = paste("Comparison of", var, "across groups"),
             x = group_column, y = var) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none") +
        scale_fill_brewer(type = "qual", palette = "Set2")
      
      # Save plot to file
      plot_filename <- file.path(output_path, paste0("boxplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 8, height = 6, dpi = 300)
      
      plots[[paste0("boxplot_", var)]] <- p
      plot_files[[paste0("boxplot_", var)]] <- plot_filename
      cat("Created and saved boxplot for", var, "to", plot_filename, "\n")
    }, error = function(e) {
      cat("Error creating boxplot for", var, ":", e$message, "\n")
    })
  }
  
  # Bar plots for categorical variables
  for (var in categorical_vars) {
    if (length(unique(data[[var]])) <= 10) {  # Only if not too many categories
      tryCatch({
        p <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
          geom_bar(position = "dodge", alpha = 0.7) +
          labs(title = paste("Distribution of", var, "by group"),
               x = var, y = "Count", fill = group_column) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_brewer(type = "qual", palette = "Set2")
        
        # Save plot to file
        plot_filename <- file.path(output_path, paste0("barplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 8, height = 6, dpi = 300)
        
        plots[[paste0("barplot_", var)]] <- p
        plot_files[[paste0("barplot_", var)]] <- plot_filename
        cat("Created and saved barplot for", var, "to", plot_filename, "\n")
      }, error = function(e) {
        cat("Error creating barplot for", var, ":", e$message, "\n")
      })
    }
  }
  
  cat("Total plots created:", length(plots), "\n")
  cat("Total plot files saved:", length(plot_files), "\n")
  
  return(list(
    plots = plots,
    plot_files = plot_files
  ))
}

# Helper functions for skewness and kurtosis (from descriptive_stats.R)
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / ((n - 1) * sd_x^3)
  return(skew)
}

calculate_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA)
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  kurt <- sum((x - mean_x)^4) / ((n - 1) * sd_x^4) - 3
  return(kurt)
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