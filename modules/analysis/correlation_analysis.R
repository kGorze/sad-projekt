# Correlation analysis - Simplified version
# Maintaining all functionality while removing redundancies

# Package dependencies
source("modules/reporting/export_results.R")
source("modules/analysis/assumptions_dashboard.R")

# Main correlation analysis function
perform_correlation_analysis <- function(data, group_column = NULL, variables = NULL, include_plots = TRUE, 
                                       shared_assumptions = NULL) {
  
  result <- create_analysis_result("correlation_analysis")
  cat("Starting correlation analysis...\n")
  
  # Identify numeric variables
  if (is.null(variables)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    if (!is.null(group_column) && group_column %in% numeric_vars) {
      numeric_vars <- numeric_vars[numeric_vars != group_column]
    }
  } else {
    numeric_vars <- variables[variables %in% names(data)]
    numeric_vars <- numeric_vars[sapply(data[numeric_vars], is.numeric)]
  }
  
  if (length(numeric_vars) < 2) {
    cat("Error: Need at least 2 numeric variables for correlation analysis\n")
    return(NULL)
  }
  
  cat("- Analyzing correlations for", length(numeric_vars), "variables\n")
  
  # Step 1: Use shared assumptions analysis (no duplication)
  if (!is.null(shared_assumptions)) {
    cat("\n=== STEP 1: USING SHARED ASSUMPTIONS ANALYSIS ===\n")
    cat("- Reusing assumptions testing from descriptive stats (eliminates duplication)\n")
    assumptions_results <- shared_assumptions
    result$assumptions_analysis <- assumptions_results
    display_method_recommendations(assumptions_results)
  } else {
    cat("\n=== STEP 1: ASSUMPTIONS TESTING SKIPPED ===\n")
    cat("- No shared assumptions provided - proceeding with correlation analysis\n")
    cat("- Recommendation: Run descriptive stats first for comprehensive assumptions testing\n")
  }
  
  # Step 2: Overall correlations
  cat("\n=== STEP 2: OVERALL CORRELATIONS ===\n")
  overall_correlations <- calculate_correlations(data, numeric_vars)
  result$overall_correlations <- overall_correlations
  cat("- Overall correlation matrix calculated for", length(numeric_vars), "variables\n")
  
  # Step 3: Group-wise correlations
  if (!is.null(group_column)) {
    cat("\n=== STEP 3: GROUP-WISE CORRELATIONS ===\n")
    group_correlations <- calculate_group_correlations(data, numeric_vars, group_column)
    result$group_correlations <- group_correlations
    
    # Step 4: Partial correlations controlling for group
    cat("\n=== STEP 4: PARTIAL CORRELATIONS ===\n")
    partial_correlations <- calculate_partial_correlations(data, numeric_vars, group_column)
    result$partial_correlations <- partial_correlations
    cat("- Partial correlations calculated controlling for", group_column, "\n")
  }
  
  # Step 5: Summary and interpretation
  cat("\n=== STEP 5: CORRELATION SUMMARY ===\n")
  correlation_summary <- create_correlation_summary(overall_correlations, 
                                                   if(!is.null(group_column)) group_correlations else NULL)
  result$correlation_summary <- correlation_summary
  cat("- Correlation summary and interpretation completed\n")
  
  # Step 6: Generate plots
  if (include_plots) {
    cat("\n=== STEP 6: GENERATING VISUALIZATIONS ===\n")
    plots_output_path <- file.path("output", "plots", "correlation_analysis")
    
    plots_result <- create_correlation_plots(data, numeric_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
    cat("- Generated", length(plots_result$plots), "correlation plots\n")
  }
  
  # Add metadata
  result$metadata <- list(
    total_observations = nrow(data),
    total_variables = length(numeric_vars),
    group_column = group_column,
    groups = if(!is.null(group_column)) unique(data[[group_column]]) else NULL,
    analysis_date = Sys.time()
  )
  
  cat("Correlation analysis completed successfully.\n")
  return(result)
}

# Unified function to calculate correlations with p-values and FDR correction
calculate_correlations <- function(data, variables) {
  
  if (length(variables) < 2) return(NULL)
  
  # Prepare clean data
  cor_data <- data[, variables, drop = FALSE]
  cor_data <- cor_data[complete.cases(cor_data), ]
  
  if (nrow(cor_data) < 3) {
    return(list(error = "Insufficient complete observations", n_complete = nrow(cor_data)))
  }
  
  # Calculate correlation matrices
  pearson_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
  spearman_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")
  
  # Calculate p-values and apply FDR correction
  p_results <- calculate_p_values_with_fdr(cor_data, variables)
  
  return(list(
    pearson_matrix = pearson_matrix,
    spearman_matrix = spearman_matrix,
    pearson_p_values = p_results$pearson_p_values,
    spearman_p_values = p_results$spearman_p_values,
    pearson_p_values_fdr = p_results$pearson_p_values_fdr,
    spearman_p_values_fdr = p_results$spearman_p_values_fdr,
    n_observations = nrow(cor_data),
    variables = variables
  ))
}

# Unified p-value calculation with FDR correction (removes redundancy)
calculate_p_values_with_fdr <- function(cor_data, variables) {
  
  n_vars <- length(variables)
  pearson_p_values <- matrix(NA, nrow = n_vars, ncol = n_vars)
  spearman_p_values <- matrix(NA, nrow = n_vars, ncol = n_vars)
  rownames(pearson_p_values) <- colnames(pearson_p_values) <- variables
  rownames(spearman_p_values) <- colnames(spearman_p_values) <- variables
  
  # Collect all p-values for FDR correction
  pearson_p_raw <- c()
  spearman_p_raw <- c()
  indices <- list()
  
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i != j) {
        # Pearson test
        pearson_test <- cor.test(cor_data[[variables[i]]], cor_data[[variables[j]]], method = "pearson")
        pearson_p_values[i, j] <- pearson_test$p.value
        pearson_p_raw <- c(pearson_p_raw, pearson_test$p.value)
        
        # Spearman test
        spearman_test <- cor.test(cor_data[[variables[i]]], cor_data[[variables[j]]], method = "spearman", exact = FALSE)
        spearman_p_values[i, j] <- spearman_test$p.value
        spearman_p_raw <- c(spearman_p_raw, spearman_test$p.value)
        
        indices[[length(indices) + 1]] <- c(i, j)
      }
    }
  }
  
  # Apply FDR correction
  pearson_p_values_fdr <- pearson_p_values
  spearman_p_values_fdr <- spearman_p_values
  
  if (length(pearson_p_raw) > 0) {
    pearson_p_adjusted <- p.adjust(pearson_p_raw, method = "BH")
    spearman_p_adjusted <- p.adjust(spearman_p_raw, method = "BH")
    
    # Fill adjusted matrices
    for (k in 1:length(indices)) {
      i <- indices[[k]][1]
      j <- indices[[k]][2]
      pearson_p_values_fdr[i, j] <- pearson_p_adjusted[k]
      spearman_p_values_fdr[i, j] <- spearman_p_adjusted[k]
    }
  }
  
  return(list(
    pearson_p_values = pearson_p_values,
    spearman_p_values = spearman_p_values,
    pearson_p_values_fdr = pearson_p_values_fdr,
    spearman_p_values_fdr = spearman_p_values_fdr
  ))
}

# Group-wise correlations (simplified, removing duplicate with detailed tables)
calculate_group_correlations <- function(data, variables, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  group_correlations <- list()
  
  for (group in groups) {
    cat("- Processing group:", group, "\n")
    
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variables, drop = FALSE]
    group_data <- group_data[complete.cases(group_data), ]
    
    if (nrow(group_data) >= 3 && ncol(group_data) >= 2) {
      group_correlations[[as.character(group)]] <- calculate_correlations(group_data, variables)
    } else {
      group_correlations[[as.character(group)]] <- list(
        error = "Insufficient data for correlation analysis",
        n_observations = nrow(group_data)
      )
    }
  }
  
  return(group_correlations)
}

# Partial correlations (simplified)
calculate_partial_correlations <- function(data, variables, group_column) {
  
  clean_data <- data[complete.cases(data[c(variables, group_column)]), ]
  
  if (nrow(clean_data) < 10) {
    return(list(error = "Insufficient data for partial correlation analysis", n = nrow(clean_data)))
  }
  
  # Convert group to numeric
  if (is.factor(clean_data[[group_column]]) || is.character(clean_data[[group_column]])) {
    clean_data$group_numeric <- as.numeric(as.factor(clean_data[[group_column]]))
  } else {
    clean_data$group_numeric <- clean_data[[group_column]]
  }
  
  # Calculate partial correlations using residuals
  n_vars <- length(variables)
  partial_cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  partial_p_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  rownames(partial_cor_matrix) <- colnames(partial_cor_matrix) <- variables
  rownames(partial_p_matrix) <- colnames(partial_p_matrix) <- variables
  
  all_partial_p <- c()
  indices <- list()
  
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i != j) {
        tryCatch({
          var1_resid <- residuals(lm(clean_data[[variables[i]]] ~ clean_data$group_numeric))
          var2_resid <- residuals(lm(clean_data[[variables[j]]] ~ clean_data$group_numeric))
          
          partial_cor_test <- cor.test(var1_resid, var2_resid, method = "pearson")
          partial_cor_matrix[i, j] <- partial_cor_test$estimate
          partial_p_matrix[i, j] <- partial_cor_test$p.value
          
          all_partial_p <- c(all_partial_p, partial_cor_test$p.value)
          indices[[length(indices) + 1]] <- c(i, j)
        }, error = function(e) {
          partial_cor_matrix[i, j] <- NA
          partial_p_matrix[i, j] <- NA
        })
      } else {
        partial_cor_matrix[i, j] <- 1.0
      }
    }
  }
  
  # Apply FDR correction
  partial_p_matrix_fdr <- partial_p_matrix
  if (length(all_partial_p) > 0) {
    partial_p_fdr <- p.adjust(all_partial_p, method = "BH")
    for (k in 1:length(indices)) {
      i <- indices[[k]][1]
      j <- indices[[k]][2]
      partial_p_matrix_fdr[i, j] <- partial_p_fdr[k]
    }
  }
  
  return(list(
    control_variable = group_column,
    n = nrow(clean_data),
    partial_correlation_matrix = partial_cor_matrix,
    p_value_matrix_fdr = partial_p_matrix_fdr,
    interpretation = paste0("Partial correlations controlling for ", group_column)
  ))
}

# Interpret correlation strength
interpret_correlation_strength <- function(correlation_value) {
  if (is.na(correlation_value)) return(list(strength = "unknown", direction = "unknown"))
  
  abs_cor <- abs(correlation_value)
  strength <- if (abs_cor < 0.1) "negligible" else if (abs_cor < 0.3) "weak" else if (abs_cor < 0.5) "moderate" else if (abs_cor < 0.7) "strong" else "very strong"
  direction <- ifelse(correlation_value > 0, "positive", "negative")
  
  return(list(strength = strength, direction = direction))
}

# Create correlation summary
create_correlation_summary <- function(overall_correlations, group_correlations = NULL) {
  
  summary_info <- list()
  
  # Overall summary
  if (!is.null(overall_correlations) && !is.null(overall_correlations$pearson_matrix)) {
    pearson_mat <- overall_correlations$pearson_matrix
    pearson_p_fdr <- overall_correlations$pearson_p_values_fdr
    
    upper_tri <- upper.tri(pearson_mat)
    correlations_df <- data.frame(
      variable1 = rep(rownames(pearson_mat), ncol(pearson_mat))[upper_tri],
      variable2 = rep(colnames(pearson_mat), each = nrow(pearson_mat))[upper_tri],
      pearson_r = pearson_mat[upper_tri],
      pearson_p_fdr = pearson_p_fdr[upper_tri],
      stringsAsFactors = FALSE
    )
    
    correlations_df$strength <- sapply(correlations_df$pearson_r, function(r) interpret_correlation_strength(r)$strength)
    correlations_df$direction <- sapply(correlations_df$pearson_r, function(r) interpret_correlation_strength(r)$direction)
    correlations_df$significant_fdr <- correlations_df$pearson_p_fdr < 0.05
    
    correlations_df <- correlations_df[order(abs(correlations_df$pearson_r), decreasing = TRUE), ]
    
    summary_info$overall_summary <- correlations_df
    summary_info$significant_correlations <- correlations_df[correlations_df$significant_fdr & !is.na(correlations_df$significant_fdr), ]
    summary_info$strength_distribution <- table(correlations_df$strength)
  }
  
  # Group summaries
  if (!is.null(group_correlations)) {
    group_summaries <- list()
    for (group_name in names(group_correlations)) {
      group_data <- group_correlations[[group_name]]
      if (!is.null(group_data$pearson_matrix)) {
        pearson_mat <- group_data$pearson_matrix
        pearson_p_fdr <- group_data$pearson_p_values_fdr
        
        upper_tri <- upper.tri(pearson_mat)
        group_cors_df <- data.frame(
          variable1 = rep(rownames(pearson_mat), ncol(pearson_mat))[upper_tri],
          variable2 = rep(colnames(pearson_mat), each = nrow(pearson_mat))[upper_tri],
          pearson_r = pearson_mat[upper_tri],
          pearson_p_fdr = pearson_p_fdr[upper_tri],
          stringsAsFactors = FALSE
        )
        
        group_cors_df$significant_fdr <- group_cors_df$pearson_p_fdr < 0.05
        group_summaries[[group_name]] <- list(
          correlations = group_cors_df,
          significant_count_fdr = sum(group_cors_df$significant_fdr, na.rm = TRUE),
          n_observations = group_data$n_observations
        )
      }
    }
    summary_info$group_summaries <- group_summaries
  }
  
  return(summary_info)
}

# Create essential correlation plots (keeping only required ones)
create_correlation_plots <- function(data, variables, group_column = NULL, output_path = "output/plots/") {
  
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  plots <- list()
  plot_files <- list()
  
  cat("- Creating correlation visualizations...\n")
  
  # Source plotting utilities
  if (!exists("ensure_clean_graphics_environment")) {
    source("modules/utils/plotting_utils.R")
  }
  ensure_clean_graphics_environment()
  
  cor_data <- data[, variables, drop = FALSE]
  cor_data <- cor_data[complete.cases(cor_data), ]
  
  # 1. Overall correlation matrix
  if (nrow(cor_data) >= 3 && ncol(cor_data) >= 2) {
    tryCatch({
      cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
      
      p <- ggcorrplot(cor_matrix, 
                      hc.order = TRUE,
                      type = "lower",
                      lab = TRUE,
                      lab_size = 3.5,
                      method = "circle",
                      colors = c("#6D9EC1", "white", "#E46726"),
                      title = "Overall Correlation Matrix (Pearson)",
                      ggtheme = theme_minimal()) +
        labs(subtitle = paste("Based on", nrow(cor_data), "complete observations")) +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"))
      
      plot_filename <- file.path(output_path, paste0("correlation_matrix_overall_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      safe_ggsave(plot_filename, plot = p, width = 12, height = 10, dpi = 300)
      
      plots[["correlation_matrix_overall"]] <- p
      plot_files[["correlation_matrix_overall"]] <- plot_filename
             cat("  → Created overall correlation matrix\n")
    }, error = function(e) cat("Error creating overall correlation matrix:", e$message, "\n"))
  }
  
  # 2. Group comparison matrix (REQUIRED)
  if (!is.null(group_column)) {
    tryCatch({
      groups <- unique(data[[group_column]])
      groups <- groups[!is.na(groups)]
      
      if (length(groups) >= 2) {
        group_plots <- list()
        
        for (group in groups) {
          group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variables, drop = FALSE]
          group_data <- group_data[complete.cases(group_data), ]
          
          if (nrow(group_data) >= 3 && ncol(group_data) >= 2) {
            cor_matrix_group <- cor(group_data, method = "pearson", use = "complete.obs")
            cor_long <- reshape2::melt(cor_matrix_group, na.rm = TRUE)
            cor_long$Group <- group
            cor_long$n_obs <- nrow(group_data)
            group_plots[[group]] <- cor_long
          }
        }
        
        if (length(group_plots) >= 2) {
          combined_data <- do.call(rbind, group_plots)
          combined_data$Group <- factor(combined_data$Group)
          
          p_compare <- ggplot(combined_data, aes(x = Var1, y = Var2, fill = value)) +
            geom_tile(color = "white") +
            geom_text(aes(label = round(value, 2)), size = 2.5) +
            scale_fill_gradient2(low = "#6D9EC1", high = "#E46726", mid = "white", 
                               midpoint = 0, limit = c(-1,1), space = "Lab", 
                               name = "Correlation") +
            facet_wrap(~ paste(Group, "\n(n =", n_obs, ")"), ncol = length(groups)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "bold")) +
            labs(title = "Correlation Matrix Comparison Across Groups",
                 subtitle = "Pearson correlations for each group separately",
                 x = "", y = "") +
            coord_fixed()
          
          plot_filename <- file.path(output_path, paste0("correlation_comparison_groups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
          ggsave(plot_filename, plot = p_compare, width = 4 * length(groups), height = 6, dpi = 300)
          
          plots[["correlation_comparison_groups"]] <- p_compare
          plot_files[["correlation_comparison_groups"]] <- plot_filename
                     cat("  → Created group comparison correlation matrix\n")
        }
      }
    }, error = function(e) cat("Error creating group comparison matrix:", e$message, "\n"))
  }
  
  # 3. Distribution of correlation strengths (REQUIRED)
  tryCatch({
    cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
    upper_tri <- upper.tri(cor_matrix)
    all_correlations <- cor_matrix[upper_tri]
    all_correlations <- all_correlations[!is.na(all_correlations)]
    
    if (length(all_correlations) > 0) {
      cors_df <- data.frame(correlation = all_correlations)
      
      p <- ggplot(cors_df, aes(x = correlation)) +
        geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
        geom_vline(xintercept = c(-0.3, 0.3), linetype = "dotted", color = "orange", alpha = 0.7) +
        geom_vline(xintercept = c(-0.7, 0.7), linetype = "dotted", color = "darkgreen", alpha = 0.7) +
        labs(title = "Distribution of Correlation Strengths",
             subtitle = paste("Histogram of all pairwise correlations (n =", length(all_correlations), "pairs)"),
             x = "Correlation Coefficient", 
             y = "Frequency") +
        scale_x_continuous(breaks = seq(-1, 1, 0.2), limits = c(-1, 1)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10))
      
      plot_filename <- file.path(output_path, paste0("correlation_distribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[["correlation_distribution"]] <- p
      plot_files[["correlation_distribution"]] <- plot_filename
             cat("  → Created correlation strength distribution plot\n")
    }
  }, error = function(e) cat("Error creating correlation distribution plot:", e$message, "\n"))
  
  # Clean up graphics
  cleanup_graphics_after_analysis()
  
  return(list(plots = plots, plot_files = plot_files))
}

# Display method recommendations (simplified)
display_method_recommendations <- function(assumptions_results) {
  if (is.null(assumptions_results$assumptions_summary)) return()
  
  summary_table <- assumptions_results$assumptions_summary
  cat("\n--- CORRELATION METHOD RECOMMENDATIONS ---\n")
  
  pearson_count <- sum(summary_table$Overall_Normal & !summary_table$Borderline, na.rm = TRUE)
  total_vars <- nrow(summary_table)
  normal_percentage <- (pearson_count / total_vars) * 100
  
  strategy <- if (normal_percentage >= 80) {
    "Primarily PEARSON correlations (most variables normal)"
  } else if (normal_percentage >= 50) {
    "MIXED approach (both Pearson and Spearman as appropriate)"
  } else {
    "Primarily SPEARMAN correlations (many non-normal variables)"
  }
  
  cat("STRATEGY:", strategy, "\n")
  cat("Variables suitable for Pearson:", pearson_count, "/", total_vars, "\n\n")
}

# Quick analysis wrapper (simplified)
quick_correlation_analysis <- function(data, group_column = NULL, variables = NULL, generate_report = TRUE, 
                                     shared_assumptions = NULL) {
  result <- perform_correlation_analysis(data, group_column, variables, include_plots = TRUE, 
                                       shared_assumptions = shared_assumptions)
  
  if (generate_report) {
    report_file <- quick_report(result)
    cat("Correlation analysis report generated:", report_file, "\n")
    return(list(analysis_result = result, report_file = report_file))
  }
  
  return(result)
} 