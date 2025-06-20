# Enhanced post hoc analysis
# Advanced post hoc testing with multiple correction methods and comprehensive reporting


# Source configuration
source("modules/utils/config.R")

# Enhanced post hoc analysis function
perform_enhanced_posthoc_analysis <- function(data, results_obj, group_column) {
  
  cat("=== ENHANCED POST HOC ANALYSIS ===\n")
  
  enhanced_results <- list()
  
  # Extract test results that may need post hoc analysis
  if (!is.null(results_obj$test_results)) {
    
    for (var_name in names(results_obj$test_results)) {
      var_result <- results_obj$test_results[[var_name]]
      
      # Check if this is a significant omnibus test that needs post hoc
      if (!is.null(var_result$p_value) && var_result$p_value < 0.05) {
        
        cat("Performing enhanced post hoc analysis for:", var_name, "\n")
        
        # Determine test type and perform appropriate post hoc
        if (grepl("ANOVA", var_result$test_name)) {
          enhanced_results[[var_name]] <- enhanced_anova_posthoc(data, var_name, group_column)
        } else if (grepl("Kruskal-Wallis", var_result$test_name)) {
          enhanced_results[[var_name]] <- enhanced_kruskal_posthoc(data, var_name, group_column)
        }
      }
    }
  }
  
  return(enhanced_results)
}

# Enhanced ANOVA post hoc with multiple methods
enhanced_anova_posthoc <- function(data, variable, group_column) {
  
  # Prepare clean data
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  formula_str <- paste(variable, "~", group_column)
  anova_result <- aov(as.formula(formula_str), data = clean_data)
  
  posthoc_results <- list()
  
  # 1. Tukey HSD (current implementation)
  posthoc_results$tukey <- perform_comprehensive_tukey(anova_result, clean_data, variable, group_column)
  
  # 2. Bonferroni correction
  posthoc_results$bonferroni <- perform_bonferroni_posthoc(clean_data, variable, group_column)
  
  # 3. Holm correction
  posthoc_results$holm <- perform_holm_posthoc(clean_data, variable, group_column)
  
  # 4. False Discovery Rate (FDR/Benjamini-Hochberg)
  posthoc_results$fdr <- perform_fdr_posthoc(clean_data, variable, group_column)
  
  # 5. Games-Howell (for unequal variances)
  posthoc_results$games_howell <- perform_games_howell(clean_data, variable, group_column)
  
  # 6. Compute effect sizes for all pairwise comparisons
  posthoc_results$effect_sizes <- compute_pairwise_effect_sizes(clean_data, variable, group_column)
  
  # 7. Create summary comparison table
  posthoc_results$summary <- create_posthoc_summary_table(posthoc_results)
  posthoc_results$adjustment_comparison <- compare_adjustment_methods(posthoc_results)
  
  return(posthoc_results)
}

# Enhanced Kruskal-Wallis post hoc with multiple methods
enhanced_kruskal_posthoc <- function(data, variable, group_column) {
  
  clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_column]]), ]
  
  posthoc_results <- list()
  
  # 1. Dunn's test with different corrections
  posthoc_results$dunn_bonferroni <- perform_dunn_test_enhanced(clean_data, variable, group_column, "bonferroni")
  posthoc_results$dunn_holm <- perform_dunn_test_enhanced(clean_data, variable, group_column, "holm")
  posthoc_results$dunn_fdr <- perform_dunn_test_enhanced(clean_data, variable, group_column, "fdr")
  
  # 2. Pairwise Wilcoxon tests with corrections
  posthoc_results$wilcoxon <- perform_pairwise_wilcoxon_with_adjustments(clean_data, variable, group_column)
  
  # 3. Effect sizes for non-parametric comparisons
  posthoc_results$effect_sizes <- compute_nonparametric_effect_sizes(clean_data, variable, group_column)
  
  # 4. Create summary comparison table
  posthoc_results$summary <- create_nonparametric_summary_table(posthoc_results)
  posthoc_results$adjustment_comparison <- compare_nonparametric_adjustment_methods(posthoc_results)
  
  return(posthoc_results)
}

# Comprehensive Tukey HSD with additional information
perform_comprehensive_tukey <- function(anova_result, data, variable, group_column) {
  
  tukey_result <- TukeyHSD(anova_result)
  comparisons <- tukey_result[[1]]
  
  # Enhanced output with confidence intervals and effect size interpretation
  enhanced_tukey <- data.frame(
    comparison = rownames(comparisons),
    mean_difference = round(comparisons[, "diff"], 3),
    lower_ci = round(comparisons[, "lwr"], 3),
    upper_ci = round(comparisons[, "upr"], 3),
          p_value = format.pval(comparisons[, "p adj"], digits = 3),
    significant = comparisons[, "p adj"] < 0.05,
    interpretation = ifelse(comparisons[, "p adj"] < 0.001, "***", 
                           ifelse(comparisons[, "p adj"] < 0.01, "**",
                                 ifelse(comparisons[, "p adj"] < 0.05, "*", "ns"))),
    stringsAsFactors = FALSE
  )
  
  return(list(
    test_name = "Tukey HSD",
    method = "Family-wise error rate control",
    results = enhanced_tukey,
    significant_pairs = enhanced_tukey$comparison[enhanced_tukey$significant]
  ))
}

# Bonferroni post hoc correction
perform_bonferroni_posthoc <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results <- data.frame(
    comparison = character(),
    t_statistic = numeric(),
    df = numeric(),
    p_value = numeric(),
    p_adjusted = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      t_test <- t.test(data1, data2)
      
      results[i, "comparison"] <- paste(group1, "vs", group2)
      results[i, "t_statistic"] <- round(t_test$statistic, 3)
      results[i, "df"] <- t_test$parameter
      results[i, "p_value"] <- t_test$p.value
      
      p_values[i] <- t_test$p.value
    }
  }
  
  # Apply Bonferroni correction
  # Rationale: Bonferroni method chosen for strict Type I error control
  # Formula: p_adjusted = min(p_raw × m, 1), where m = number of comparisons
  # Conservative but guarantees FWER ≤ α
  results$p_adjusted <- p.adjust(p_values, method = "bonferroni")
  results$significant <- results$p_adjusted < 0.05
  
  return(list(
    test_name = "Pairwise t-tests",
    method = "Bonferroni correction",
    results = results,
    significant_pairs = results$comparison[results$significant]
  ))
}

# Holm post hoc correction
perform_holm_posthoc <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results <- data.frame(
    comparison = character(),
    t_statistic = numeric(),
    df = numeric(),
    p_value = numeric(),
    p_adjusted = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      t_test <- t.test(data1, data2)
      
      results[i, "comparison"] <- paste(group1, "vs", group2)
      results[i, "t_statistic"] <- round(t_test$statistic, 3)
      results[i, "df"] <- t_test$parameter
      results[i, "p_value"] <- t_test$p.value
      
      p_values[i] <- t_test$p.value
    }
  }
  
  # Apply Holm correction
  results$p_adjusted <- p.adjust(p_values, method = "holm")
  results$significant <- results$p_adjusted < 0.05
  
  return(list(
    test_name = "Pairwise t-tests",
    method = "Holm correction (step-down Bonferroni)",
    results = results,
    significant_pairs = results$comparison[results$significant]
  ))
}

# False Discovery Rate post hoc correction
perform_fdr_posthoc <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results <- data.frame(
    comparison = character(),
    t_statistic = numeric(),
    df = numeric(),
    p_value = numeric(),
    p_adjusted = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      t_test <- t.test(data1, data2)
      
      results[i, "comparison"] <- paste(group1, "vs", group2)
      results[i, "t_statistic"] <- round(t_test$statistic, 3)
      results[i, "df"] <- t_test$parameter
      results[i, "p_value"] <- t_test$p.value
      
      p_values[i] <- t_test$p.value
    }
  }
  
  # Apply FDR/Benjamini-Hochberg correction
  results$p_adjusted <- p.adjust(p_values, method = "fdr")
  results$significant <- results$p_adjusted < 0.05
  
  return(list(
    test_name = "Pairwise t-tests",
    method = "False Discovery Rate (Benjamini-Hochberg)",
    results = results,
    significant_pairs = results$comparison[results$significant]
  ))
}

# Games-Howell post hoc (for unequal variances)
perform_games_howell <- function(data, variable, group_column) {
  
  # This is a simplified implementation
  # For full Games-Howell, you'd typically use the `userfriendlyscience` package
  # or implement the full algorithm
  
  tryCatch({
    # Try to load the package for proper Games-Howell
    if (requireNamespace("PMCMRplus", quietly = TRUE)) {
      library(PMCMRplus)
      gh_result <- gamesHowellTest(data[[variable]], data[[group_column]])
      
      return(list(
        test_name = "Games-Howell",
        method = "Unequal variances adjustment",
        results = gh_result,
        note = "Robust for unequal variances"
      ))
    } else {
      # Fallback to Welch t-tests with appropriate correction
      return(perform_welch_posthoc(data, variable, group_column))
    }
  }, error = function(e) {
    return(list(
      test_name = "Games-Howell",
      error = "Could not perform Games-Howell test",
      fallback = "Use Welch t-tests instead"
    ))
  })
}

# Welch t-tests as fallback for unequal variances
perform_welch_posthoc <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results <- data.frame(
    comparison = character(),
    t_statistic = numeric(),
    df = numeric(),
    p_value = numeric(),
    p_adjusted = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      # Welch t-test (unequal variances)
      t_test <- t.test(data1, data2, var.equal = FALSE)
      
      results[i, "comparison"] <- paste(group1, "vs", group2)
      results[i, "t_statistic"] <- round(t_test$statistic, 3)
      results[i, "df"] <- round(t_test$parameter, 2)
      results[i, "p_value"] <- t_test$p.value
      
      p_values[i] <- t_test$p.value
    }
  }
  
  # Apply Holm correction (conservative for unequal variances)
  results$p_adjusted <- p.adjust(p_values, method = "holm")
  results$significant <- results$p_adjusted < 0.05
  
  return(list(
    test_name = "Welch t-tests",
    method = "Unequal variances with Holm correction",
    results = results,
    significant_pairs = results$comparison[results$significant]
  ))
}

# Enhanced Dunn's test with multiple correction options
perform_dunn_test_enhanced <- function(data, variable, group_column, method = "bonferroni") {
  
  tryCatch({
    dunn_result <- dunn.test(data[[variable]], data[[group_column]], 
                            method = method, alpha = 0.05)
    
    # Create enhanced results table
    results_df <- data.frame(
      comparison = dunn_result$comparisons,
      z_statistic = round(dunn_result$Z, 3),
      p_value = format.pval(dunn_result$P, digits = 3),
      p_adjusted = format.pval(dunn_result$P.adjusted, digits = 3),
      significant = dunn_result$P.adjusted < 0.05,
      stringsAsFactors = FALSE
    )
    
    return(list(
      test_name = paste("Dunn's test with", method, "correction"),
      method = paste("Non-parametric multiple comparisons,", method, "adjustment"),
      results = results_df,
      significant_pairs = results_df$comparison[results_df$significant]
    ))
    
  }, error = function(e) {
    return(list(
      test_name = "Dunn's test",
      error = paste("Could not perform Dunn's test:", e$message)
    ))
  })
}

# Pairwise Wilcoxon tests
perform_pairwise_wilcoxon <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results <- data.frame(
    comparison = character(),
    w_statistic = numeric(),
    p_value = numeric(),
    p_adjusted = numeric(),
    significant = logical(),
    stringsAsFactors = FALSE
  )
  
  p_values <- numeric()
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      # Use exact=FALSE to avoid ties warning and continuity correction
      wilcox_test <- wilcox.test(data1, data2, exact = FALSE, correct = TRUE)
      
      results[i, "comparison"] <- paste(group1, "vs", group2)
      results[i, "w_statistic"] <- wilcox_test$statistic
      results[i, "p_value"] <- wilcox_test$p.value
      
      p_values[i] <- wilcox_test$p.value
    }
  }
  
  # Apply Holm correction
  results$p_adjusted <- p.adjust(p_values, method = "holm")
  results$significant <- results$p_adjusted < 0.05
  
  return(list(
    test_name = "Pairwise Wilcoxon tests",
    method = "Non-parametric with Holm correction",
    results = results,
    significant_pairs = results$comparison[results$significant]
  ))
}

# Compute pairwise effect sizes (Cohen's d)
compute_pairwise_effect_sizes <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  effect_sizes <- data.frame(
    comparison = character(),
    cohens_d = numeric(),
    effect_size_interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      # Calculate Cohen's d
      mean1 <- mean(data1)
      mean2 <- mean(data2)
      sd1 <- sd(data1)
      sd2 <- sd(data2)
      n1 <- length(data1)
      n2 <- length(data2)
      
      # Pooled standard deviation
      pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
      
      cohens_d <- (mean1 - mean2) / pooled_sd
      
      # Interpret effect size
      interpretation <- if (abs(cohens_d) < 0.2) "Negligible"
                       else if (abs(cohens_d) < 0.5) "Small"
                       else if (abs(cohens_d) < 0.8) "Medium"
                       else "Large"
      
      effect_sizes[i, "comparison"] <- paste(group1, "vs", group2)
      effect_sizes[i, "cohens_d"] <- round(cohens_d, 3)
      effect_sizes[i, "effect_size_interpretation"] <- interpretation
    }
  }
  
  return(effect_sizes)
}

# Compute non-parametric effect sizes (rank biserial correlation)
compute_nonparametric_effect_sizes <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  effect_sizes <- data.frame(
    comparison = character(),
    rank_biserial_r = numeric(),
    effect_size_interpretation = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    if (length(data1) > 1 && length(data2) > 1) {
      # Calculate rank biserial correlation
      wilcox_result <- wilcox.test(data1, data2)
      n1 <- length(data1)
      n2 <- length(data2)
      
      # Rank biserial correlation
      r <- 1 - (2 * wilcox_result$statistic) / (n1 * n2)
      
      # Interpret effect size
      interpretation <- if (abs(r) < 0.1) "Negligible"
                       else if (abs(r) < 0.3) "Small"
                       else if (abs(r) < 0.5) "Medium"
                       else "Large"
      
      effect_sizes[i, "comparison"] <- paste(group1, "vs", group2)
      effect_sizes[i, "rank_biserial_r"] <- round(r, 3)
      effect_sizes[i, "effect_size_interpretation"] <- interpretation
    }
  }
  
  return(effect_sizes)
}

# Create post hoc summary table
create_posthoc_summary_table <- function(posthoc_results) {
  
  # Extract significant pairs from each method
  methods <- names(posthoc_results)[names(posthoc_results) != "summary"]
  
  summary_df <- data.frame(
    method = character(),
    significant_pairs = character(),
    total_comparisons = numeric(),
    alpha_level = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (method in methods) {
    if (!is.null(posthoc_results[[method]]$significant_pairs)) {
      sig_pairs <- paste(posthoc_results[[method]]$significant_pairs, collapse = "; ")
      total_comp <- length(posthoc_results[[method]]$significant_pairs)
      
      summary_df <- rbind(summary_df, data.frame(
        method = posthoc_results[[method]]$test_name,
        significant_pairs = if(sig_pairs == "") "None" else sig_pairs,
        total_comparisons = total_comp,
        alpha_level = 0.05,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_df)
}

# Create non-parametric summary table
create_nonparametric_summary_table <- function(posthoc_results) {
  
  methods <- names(posthoc_results)[names(posthoc_results) != "summary"]
  
  summary_df <- data.frame(
    method = character(),
    significant_pairs = character(),
    total_comparisons = numeric(),
    correction_method = character(),
    stringsAsFactors = FALSE
  )
  
  for (method in methods) {
    if (!is.null(posthoc_results[[method]]$significant_pairs)) {
      sig_pairs <- paste(posthoc_results[[method]]$significant_pairs, collapse = "; ")
      
      summary_df <- rbind(summary_df, data.frame(
        method = posthoc_results[[method]]$test_name,
        significant_pairs = if(sig_pairs == "") "None" else sig_pairs,
        total_comparisons = length(posthoc_results[[method]]$significant_pairs),
        correction_method = posthoc_results[[method]]$method,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(summary_df)
}

# Main function to add enhanced post hoc to existing results
add_enhanced_posthoc <- function(comparative_results, data, group_column) {
  
  cat("\n=== ADDING ENHANCED POST HOC ANALYSIS ===\n")
  
  enhanced_posthoc <- perform_enhanced_posthoc_analysis(data, comparative_results, group_column)
  
  # Add to existing results
  comparative_results$enhanced_posthoc <- enhanced_posthoc
  
  # Create comprehensive summary
  comparative_results$posthoc_summary <- create_comprehensive_posthoc_summary(enhanced_posthoc)
  
  cat("Enhanced post hoc analysis completed.\n")
  
  return(comparative_results)
}

# Create comprehensive post hoc summary
create_comprehensive_posthoc_summary <- function(enhanced_results) {
  
  summary_list <- list()
  
  for (var_name in names(enhanced_results)) {
    var_summary <- list(
      variable = var_name,
      methods_compared = names(enhanced_results[[var_name]]),
      consensus_significant_pairs = find_consensus_pairs(enhanced_results[[var_name]]),
      method_agreement = calculate_method_agreement(enhanced_results[[var_name]])
    )
    
    summary_list[[var_name]] <- var_summary
  }
  
  return(summary_list)
}

# Find consensus significant pairs across methods
find_consensus_pairs <- function(var_results) {
  
  # Extract significant pairs from each method
  all_sig_pairs <- list()
  
  for (method in names(var_results)) {
    if (!is.null(var_results[[method]]$significant_pairs)) {
      all_sig_pairs[[method]] <- var_results[[method]]$significant_pairs
    }
  }
  
  if (length(all_sig_pairs) == 0) return(character(0))
  
  # Find pairs that are significant in most methods
  all_pairs <- unique(unlist(all_sig_pairs))
  consensus_pairs <- character(0)
  
  for (pair in all_pairs) {
    methods_finding_sig <- sum(sapply(all_sig_pairs, function(x) pair %in% x))
    if (methods_finding_sig >= length(all_sig_pairs) / 2) {
      consensus_pairs <- c(consensus_pairs, pair)
    }
  }
  
  return(consensus_pairs)
}

# Calculate agreement between methods
calculate_method_agreement <- function(var_results) {
  
  methods <- names(var_results)[names(var_results) != "summary"]
  if (length(methods) < 2) return(1.0)
  
  # Simple agreement score based on significant pairs
  sig_pairs_list <- list()
  
  for (method in methods) {
    if (!is.null(var_results[[method]]$significant_pairs)) {
      sig_pairs_list[[method]] <- var_results[[method]]$significant_pairs
    }
  }
  
  if (length(sig_pairs_list) < 2) return(1.0)
  
  # Calculate Jaccard similarity
  all_pairs <- unique(unlist(sig_pairs_list))
  if (length(all_pairs) == 0) return(1.0)
  
  intersection_size <- length(Reduce(intersect, sig_pairs_list))
  union_size <- length(all_pairs)
  
  agreement <- intersection_size / union_size
  
  return(round(agreement, 3))
}

compare_adjustment_methods <- function(posthoc_results) {
  
  # Extract significant comparisons from each method
  tukey_sig <- if (!is.null(posthoc_results$tukey)) posthoc_results$tukey$significant_pairs else c()
  bonferroni_sig <- if (!is.null(posthoc_results$bonferroni)) posthoc_results$bonferroni$significant_pairs else c()
  holm_sig <- if (!is.null(posthoc_results$holm)) posthoc_results$holm$significant_pairs else c()
  fdr_sig <- if (!is.null(posthoc_results$fdr)) posthoc_results$fdr$significant_pairs else c()
  
  # Create comparison summary
  all_comparisons <- unique(c(tukey_sig, bonferroni_sig, holm_sig, fdr_sig))
  
  if (length(all_comparisons) == 0) {
    return(list(
      message = "No significant pairwise comparisons found across any adjustment method",
      total_comparisons = 0
    ))
  }
  
  comparison_table <- data.frame(
    comparison = all_comparisons,
    tukey = all_comparisons %in% tukey_sig,
    bonferroni = all_comparisons %in% bonferroni_sig,
    holm = all_comparisons %in% holm_sig,
    fdr = all_comparisons %in% fdr_sig,
    stringsAsFactors = FALSE
  )
  
  # Count agreements
  comparison_table$total_significant <- rowSums(comparison_table[, c("tukey", "bonferroni", "holm", "fdr")])
  
  # Summary statistics
  adjustment_summary <- data.frame(
    method = c("Tukey HSD", "Bonferroni", "Holm", "FDR (BH)"),
    significant_pairs = c(length(tukey_sig), length(bonferroni_sig), length(holm_sig), length(fdr_sig)),
    type = c("FWER", "FWER", "FWER", "FDR"),
    description = c("Simultaneous confidence intervals", 
                   "Conservative FWER control", 
                   "Step-down FWER control", 
                   "False discovery rate control"),
    stringsAsFactors = FALSE
  )
  
  return(list(
    comparison_table = comparison_table,
    adjustment_summary = adjustment_summary,
    consensus_significant = all_comparisons[comparison_table$total_significant >= 3],
    interpretation = paste0("Found ", nrow(comparison_table), " significant comparisons across methods. ",
                           "Consensus (≥3 methods agree): ", 
                           length(all_comparisons[comparison_table$total_significant >= 3]), " comparisons.")
  ))
}

# TASK 6: Compare non-parametric adjustment methods
compare_nonparametric_adjustment_methods <- function(posthoc_results) {
  
  # Extract significant comparisons from each Dunn's test method
  dunn_bonf_sig <- if (!is.null(posthoc_results$dunn_bonferroni)) posthoc_results$dunn_bonferroni$significant_pairs else c()
  dunn_holm_sig <- if (!is.null(posthoc_results$dunn_holm)) posthoc_results$dunn_holm$significant_pairs else c()
  dunn_fdr_sig <- if (!is.null(posthoc_results$dunn_fdr)) posthoc_results$dunn_fdr$significant_pairs else c()
  wilcox_sig <- if (!is.null(posthoc_results$wilcoxon) && !is.null(posthoc_results$wilcoxon$significant_pairs)) {
    posthoc_results$wilcoxon$significant_pairs 
  } else { c() }
  
  # Create comparison summary
  all_comparisons <- unique(c(dunn_bonf_sig, dunn_holm_sig, dunn_fdr_sig, wilcox_sig))
  
  if (length(all_comparisons) == 0) {
    return(list(
      message = "No significant pairwise comparisons found across any non-parametric adjustment method",
      total_comparisons = 0
    ))
  }
  
  comparison_table <- data.frame(
    comparison = all_comparisons,
    dunn_bonferroni = all_comparisons %in% dunn_bonf_sig,
    dunn_holm = all_comparisons %in% dunn_holm_sig,
    dunn_fdr = all_comparisons %in% dunn_fdr_sig,
    wilcoxon = all_comparisons %in% wilcox_sig,
    stringsAsFactors = FALSE
  )
  
  # Count agreements
  comparison_table$total_significant <- rowSums(comparison_table[, c("dunn_bonferroni", "dunn_holm", "dunn_fdr", "wilcoxon")])
  
  # Summary statistics
  adjustment_summary <- data.frame(
    method = c("Dunn + Bonferroni", "Dunn + Holm", "Dunn + FDR", "Pairwise Wilcoxon"),
    significant_pairs = c(length(dunn_bonf_sig), length(dunn_holm_sig), length(dunn_fdr_sig), length(wilcox_sig)),
    type = c("FWER", "FWER", "FDR", "Multiple"),
    description = c("Conservative FWER control", 
                   "Step-down FWER control", 
                   "False discovery rate control",
                   "Individual Mann-Whitney tests"),
    stringsAsFactors = FALSE
  )
  
  return(list(
    comparison_table = comparison_table,
    adjustment_summary = adjustment_summary,
    consensus_significant = all_comparisons[comparison_table$total_significant >= 3],
    interpretation = paste0("Found ", nrow(comparison_table), " significant comparisons across non-parametric methods. ",
                           "Consensus (≥3 methods agree): ", 
                           length(all_comparisons[comparison_table$total_significant >= 3]), " comparisons.")
  ))
}

# TASK 6: Enhanced pairwise Wilcoxon tests with multiple adjustments
perform_pairwise_wilcoxon_with_adjustments <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  comparisons <- combn(groups, 2, simplify = FALSE)
  
  results_list <- list()
  raw_p_values <- numeric()
  comparison_names <- character()
  
  # Perform all pairwise Wilcoxon tests
  for (i in seq_along(comparisons)) {
    group1 <- comparisons[[i]][1]
    group2 <- comparisons[[i]][2]
    
    data1 <- data[data[[group_column]] == group1, variable]
    data2 <- data[data[[group_column]] == group2, variable]
    
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]
    
    comparison_name <- paste(group1, "vs", group2)
    comparison_names[i] <- comparison_name
    
    if (length(data1) > 1 && length(data2) > 1) {
      wilcox_test <- wilcox.test(data1, data2, exact = FALSE)
      
      raw_p_values[i] <- wilcox_test$p.value
      
      results_list[[comparison_name]] <- list(
        comparison = comparison_name,
        statistic = wilcox_test$statistic,
        p_value_raw = wilcox_test$p.value,
        n1 = length(data1),
        n2 = length(data2)
      )
    } else {
      raw_p_values[i] <- NA
      results_list[[comparison_name]] <- list(
        comparison = comparison_name,
        error = "Insufficient data for comparison",
        n1 = length(data1),
        n2 = length(data2)
      )
    }
  }
  
  # Apply multiple adjustment methods - TASK 6
  p_bonferroni <- p.adjust(raw_p_values, method = "bonferroni")
  p_holm <- p.adjust(raw_p_values, method = "holm")
  p_fdr <- p.adjust(raw_p_values, method = "BH")
  
  # Create comprehensive results table
  results_table <- data.frame(
    comparison = comparison_names,
    p_raw = raw_p_values,
    p_bonferroni = p_bonferroni,
    p_holm = p_holm,
    p_fdr = p_fdr,
    sig_raw = raw_p_values < 0.05,
    sig_bonferroni = p_bonferroni < 0.05,
    sig_holm = p_holm < 0.05,
    sig_fdr = p_fdr < 0.05,
    stringsAsFactors = FALSE
  )
  
  # Remove rows with missing p-values
  results_table <- results_table[!is.na(results_table$p_raw), ]
  
  return(list(
    test_name = "Pairwise Wilcoxon tests",
    method = "Multiple adjustment methods applied",
    results_table = results_table,
    significant_pairs = results_table$comparison[results_table$sig_fdr], # Use FDR as default
    bonferroni_significant = results_table$comparison[results_table$sig_bonferroni],
    holm_significant = results_table$comparison[results_table$sig_holm],
    fdr_significant = results_table$comparison[results_table$sig_fdr],
    summary = list(
      total_comparisons = nrow(results_table),
      significant_bonferroni = sum(results_table$sig_bonferroni, na.rm = TRUE),
      significant_holm = sum(results_table$sig_holm, na.rm = TRUE),
      significant_fdr = sum(results_table$sig_fdr, na.rm = TRUE)
    )
  ))
} 