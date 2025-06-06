# Correlation Analysis Module
# Functions for analyzing correlations within and between groups
# Supports different correlation methods based on data characteristics

# Load required libraries with error handling
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(corrplot, quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

if (!require(GGally, quietly = TRUE)) {
  install.packages("GGally")
  library(GGally)
}

if (!require(ggcorrplot, quietly = TRUE)) {
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

# Source reporting utilities
source("modules/reporting/export_results.R")

# Main correlation analysis function
perform_correlation_analysis <- function(data, group_column = NULL, variables = NULL, include_plots = TRUE) {
  
  # Create analysis result object
  result <- create_analysis_result("correlation_analysis")
  
  cat("Starting comprehensive correlation analysis...\n")
  
  # Identify numeric variables for correlation analysis
  if (is.null(variables)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    # Remove group column if specified
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
  
  # Step 1: Overall correlation analysis
  cat("\n=== STEP 1: OVERALL CORRELATION ANALYSIS ===\n")
  overall_correlations <- calculate_overall_correlations(data, numeric_vars)
  result$overall_correlations <- overall_correlations
  
  # Step 2: Group-wise correlation analysis
  if (!is.null(group_column)) {
    cat("\n=== STEP 2: GROUP-WISE CORRELATION ANALYSIS ===\n")
    group_correlations <- calculate_group_correlations(data, numeric_vars, group_column)
    result$group_correlations <- group_correlations
    cat("- Group-wise correlations calculated for", length(unique(data[[group_column]])), "groups\n")
  }
  
  # Step 3: Correlation significance testing
  cat("\n=== STEP 3: CORRELATION SIGNIFICANCE TESTING ===\n")
  correlation_tests <- perform_correlation_significance_tests(data, numeric_vars, group_column)
  result$correlation_tests <- correlation_tests
  cat("- Significance tests completed for", length(correlation_tests$overall_tests), "variable pairs\n")
  
  # Step 4: Correlation interpretation and summary
  cat("\n=== STEP 4: CORRELATION INTERPRETATION ===\n")
  correlation_summary <- create_correlation_summary(overall_correlations, correlation_tests, 
                                                  if(!is.null(group_column)) group_correlations else NULL)
  result$correlation_summary <- correlation_summary
  
  # Step 5: Generate correlation plots
  if (include_plots) {
    cat("\n=== STEP 5: GENERATING CORRELATION VISUALIZATIONS ===\n")
    
    # Use fixed output path for plots
    plots_output_path <- file.path("output", "plots", "correlation_analysis")
    
    plots_result <- create_detailed_correlation_plots(data, numeric_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
    cat("- Generated", length(plots_result$plots), "correlation plots\n")
    cat("- Saved", length(plots_result$plot_files), "plot files\n")
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

# Calculate overall correlations (Pearson and Spearman)
calculate_overall_correlations <- function(data, variables) {
  
  if (length(variables) < 2) {
    return(NULL)
  }
  
  # Select only specified variables for correlation
  cor_data <- data[, variables, drop = FALSE]
  
  # Remove rows with any missing values for correlation analysis
  cor_data <- cor_data[complete.cases(cor_data), ]
  
  if (nrow(cor_data) < 3) {
    return(list(
      error = "Insufficient complete observations for correlation analysis",
      n_complete = nrow(cor_data)
    ))
  }
  
  # Calculate Pearson correlation matrix
  pearson_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
  
  # Calculate Spearman correlation matrix
  spearman_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")
  
  # Calculate p-values for correlations
  n <- nrow(cor_data)
  pearson_p_values <- matrix(NA, nrow = ncol(cor_data), ncol = ncol(cor_data))
  spearman_p_values <- matrix(NA, nrow = ncol(cor_data), ncol = ncol(cor_data))
  rownames(pearson_p_values) <- colnames(pearson_p_values) <- variables
  rownames(spearman_p_values) <- colnames(spearman_p_values) <- variables
  
  for (i in 1:length(variables)) {
    for (j in 1:length(variables)) {
      if (i != j) {
        # Pearson correlation test
        pearson_test <- cor.test(cor_data[[variables[i]]], cor_data[[variables[j]]], method = "pearson")
        pearson_p_values[i, j] <- pearson_test$p.value
        
        # Spearman correlation test
        spearman_test <- cor.test(cor_data[[variables[i]]], cor_data[[variables[j]]], method = "spearman")
        spearman_p_values[i, j] <- spearman_test$p.value
      } else {
        pearson_p_values[i, j] <- NA
        spearman_p_values[i, j] <- NA
      }
    }
  }
  
  return(list(
    pearson_matrix = pearson_matrix,
    spearman_matrix = spearman_matrix,
    pearson_p_values = pearson_p_values,
    spearman_p_values = spearman_p_values,
    n_observations = n,
    variables = variables
  ))
}

# Calculate group-wise correlations
calculate_group_correlations <- function(data, variables, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  group_correlations <- list()
  
  for (group in groups) {
    cat("Calculating correlations for group:", group, "\n")
    
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variables, drop = FALSE]
    group_data <- group_data[complete.cases(group_data), ]
    
    if (nrow(group_data) >= 3 && ncol(group_data) >= 2) {
      
      # Pearson correlations
      pearson_cor <- cor(group_data, method = "pearson", use = "complete.obs")
      
      # Spearman correlations  
      spearman_cor <- cor(group_data, method = "spearman", use = "complete.obs")
      
      # Calculate p-values for this group
      n <- nrow(group_data)
      pearson_p <- matrix(NA, nrow = ncol(group_data), ncol = ncol(group_data))
      spearman_p <- matrix(NA, nrow = ncol(group_data), ncol = ncol(group_data))
      rownames(pearson_p) <- colnames(pearson_p) <- colnames(group_data)
      rownames(spearman_p) <- colnames(spearman_p) <- colnames(group_data)
      
      for (i in 1:ncol(group_data)) {
        for (j in 1:ncol(group_data)) {
          if (i != j) {
            pearson_test <- cor.test(group_data[[i]], group_data[[j]], method = "pearson")
            pearson_p[i, j] <- pearson_test$p.value
            
            spearman_test <- cor.test(group_data[[i]], group_data[[j]], method = "spearman")
            spearman_p[i, j] <- spearman_test$p.value
          }
        }
      }
      
      group_correlations[[as.character(group)]] <- list(
        pearson_matrix = pearson_cor,
        spearman_matrix = spearman_cor,
        pearson_p_values = pearson_p,
        spearman_p_values = spearman_p,
        n_observations = n
      )
      
    } else {
      group_correlations[[as.character(group)]] <- list(
        error = "Insufficient data for correlation analysis",
        n_observations = nrow(group_data)
      )
    }
  }
  
  return(group_correlations)
}

# Perform correlation significance testing
perform_correlation_significance_tests <- function(data, variables, group_column = NULL) {
  
  # Overall correlation tests
  overall_tests <- list()
  
  for (i in 1:(length(variables)-1)) {
    for (j in (i+1):length(variables)) {
      var1 <- variables[i]
      var2 <- variables[j]
      
      # Get complete data for this pair
      pair_data <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), c(var1, var2)]
      
      if (nrow(pair_data) >= 3) {
        
        # Determine best correlation method based on normality
        method <- determine_correlation_method(pair_data[[var1]], pair_data[[var2]])
        
        # Perform correlation test
        if (method == "pearson") {
          cor_test <- cor.test(pair_data[[var1]], pair_data[[var2]], method = "pearson")
        } else {
          cor_test <- cor.test(pair_data[[var1]], pair_data[[var2]], method = "spearman")
        }
        
        # Interpret correlation
        interpretation <- interpret_correlation_strength(cor_test$estimate)
        
        overall_tests[[paste(var1, "vs", var2)]] <- list(
          variables = c(var1, var2),
          method = method,
          correlation = cor_test$estimate,
          p_value = cor_test$p.value,
          confidence_interval = cor_test$conf.int,
          n = nrow(pair_data),
          strength = interpretation$strength,
          direction = interpretation$direction,
          significance = ifelse(cor_test$p.value < 0.05, "significant", "not significant"),
          interpretation = paste0(
            "There is a ", ifelse(cor_test$p.value < 0.05, "significant", "non-significant"),
            " ", interpretation$strength, " ", interpretation$direction,
            " correlation (", method, " r = ", round(cor_test$estimate, 3),
            ", p = ", round(cor_test$p.value, 4), ")"
          )
        )
      }
    }
  }
  
  # Group-wise correlation tests
  group_tests <- NULL
  if (!is.null(group_column)) {
    group_tests <- list()
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    
    for (group in groups) {
      group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), ]
      group_tests[[as.character(group)]] <- list()
      
      for (i in 1:(length(variables)-1)) {
        for (j in (i+1):length(variables)) {
          var1 <- variables[i]
          var2 <- variables[j]
          
          pair_data <- group_data[!is.na(group_data[[var1]]) & !is.na(group_data[[var2]]), c(var1, var2)]
          
          if (nrow(pair_data) >= 3) {
            method <- determine_correlation_method(pair_data[[var1]], pair_data[[var2]])
            
            if (method == "pearson") {
              cor_test <- cor.test(pair_data[[var1]], pair_data[[var2]], method = "pearson")
            } else {
              cor_test <- cor.test(pair_data[[var1]], pair_data[[var2]], method = "spearman")
            }
            
            interpretation <- interpret_correlation_strength(cor_test$estimate)
            
            group_tests[[as.character(group)]][[paste(var1, "vs", var2)]] <- list(
              correlation = cor_test$estimate,
              p_value = cor_test$p.value,
              method = method,
              strength = interpretation$strength,
              direction = interpretation$direction,
              n = nrow(pair_data)
            )
          }
        }
      }
    }
  }
  
  return(list(
    overall_tests = overall_tests,
    group_tests = group_tests
  ))
}

# Determine appropriate correlation method based on data characteristics
determine_correlation_method <- function(x, y) {
  
  # Remove missing values
  complete_data <- complete.cases(x, y)
  x_clean <- x[complete_data]
  y_clean <- y[complete_data]
  
  if (length(x_clean) < 3) {
    return("spearman")  # Default to non-parametric if insufficient data
  }
  
  # Test normality for both variables
  if (length(x_clean) >= 3 && length(x_clean) <= 50) {
    # Use Shapiro-Wilk for small samples
    x_normal <- shapiro.test(x_clean)$p.value > 0.05
    y_normal <- shapiro.test(y_clean)$p.value > 0.05
  } else if (length(x_clean) > 50) {
    # Use Kolmogorov-Smirnov for larger samples
    x_normal <- ks.test(x_clean, "pnorm", mean(x_clean), sd(x_clean))$p.value > 0.05
    y_normal <- ks.test(y_clean, "pnorm", mean(y_clean), sd(y_clean))$p.value > 0.05
  } else {
    # Too few observations for reliable normality testing
    return("spearman")
  }
  
  # Use Pearson if both variables are normal, otherwise Spearman
  if (x_normal && y_normal) {
    return("pearson")
  } else {
    return("spearman")
  }
}

# Interpret correlation strength and direction
interpret_correlation_strength <- function(correlation_value) {
  
  if (is.na(correlation_value)) {
    return(list(strength = "unknown", direction = "unknown"))
  }
  
  abs_cor <- abs(correlation_value)
  
  # Classify correlation strength
  if (abs_cor < 0.1) {
    strength <- "negligible"
  } else if (abs_cor < 0.3) {
    strength <- "weak"
  } else if (abs_cor < 0.5) {
    strength <- "moderate"
  } else if (abs_cor < 0.7) {
    strength <- "strong"
  } else {
    strength <- "very strong"
  }
  
  # Determine direction
  direction <- ifelse(correlation_value > 0, "positive", "negative")
  
  return(list(strength = strength, direction = direction))
}

# Create correlation summary and interpretation
create_correlation_summary <- function(overall_correlations, correlation_tests, group_correlations = NULL) {
  
  summary_info <- list()
  
  # Overall correlation summary
  if (!is.null(overall_correlations) && !is.null(overall_correlations$pearson_matrix)) {
    
    # Find strongest correlations
    pearson_mat <- overall_correlations$pearson_matrix
    pearson_p <- overall_correlations$pearson_p_values
    
    # Extract upper triangle (avoid duplicates and diagonal)
    upper_tri <- upper.tri(pearson_mat)
    correlations_df <- data.frame(
      variable1 = rep(rownames(pearson_mat), ncol(pearson_mat))[upper_tri],
      variable2 = rep(colnames(pearson_mat), each = nrow(pearson_mat))[upper_tri],
      pearson_r = pearson_mat[upper_tri],
      pearson_p = pearson_p[upper_tri],
      stringsAsFactors = FALSE
    )
    
    # Add interpretation
    correlations_df$strength <- sapply(correlations_df$pearson_r, function(r) {
      interpret_correlation_strength(r)$strength
    })
    correlations_df$direction <- sapply(correlations_df$pearson_r, function(r) {
      interpret_correlation_strength(r)$direction
    })
    correlations_df$significant <- correlations_df$pearson_p < 0.05
    
    # Sort by absolute correlation strength
    correlations_df <- correlations_df[order(abs(correlations_df$pearson_r), decreasing = TRUE), ]
    
    summary_info$overall_summary <- correlations_df
    
    # Identify significant correlations
    significant_cors <- correlations_df[correlations_df$significant & !is.na(correlations_df$significant), ]
    summary_info$significant_correlations <- significant_cors
    
    # Count correlations by strength
    strength_counts <- table(correlations_df$strength)
    summary_info$strength_distribution <- strength_counts
  }
  
  # Group-wise correlation summaries
  if (!is.null(group_correlations)) {
    group_summaries <- list()
    
    for (group_name in names(group_correlations)) {
      group_data <- group_correlations[[group_name]]
      
      if (!is.null(group_data$pearson_matrix)) {
        # Similar analysis for each group
        pearson_mat <- group_data$pearson_matrix
        pearson_p <- group_data$pearson_p_values
        
        upper_tri <- upper.tri(pearson_mat)
        group_cors_df <- data.frame(
          variable1 = rep(rownames(pearson_mat), ncol(pearson_mat))[upper_tri],
          variable2 = rep(colnames(pearson_mat), each = nrow(pearson_mat))[upper_tri],
          pearson_r = pearson_mat[upper_tri],
          pearson_p = pearson_p[upper_tri],
          stringsAsFactors = FALSE
        )
        
        group_cors_df$strength <- sapply(group_cors_df$pearson_r, function(r) {
          interpret_correlation_strength(r)$strength
        })
        group_cors_df$significant <- group_cors_df$pearson_p < 0.05
        
        group_summaries[[group_name]] <- list(
          correlations = group_cors_df,
          significant_count = sum(group_cors_df$significant, na.rm = TRUE),
          n_observations = group_data$n_observations
        )
      }
    }
    
    summary_info$group_summaries <- group_summaries
  }
  
  return(summary_info)
}

# Create comprehensive correlation plots
create_detailed_correlation_plots <- function(data, variables, group_column = NULL, output_path = "output/plots/") {
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  cat("Creating correlation visualizations...\n")
  
  # Prepare data for correlation analysis
  cor_data <- data[, variables, drop = FALSE]
  cor_data <- cor_data[complete.cases(cor_data), ]
  
  # 1. Overall correlation matrix heatmap
  if (nrow(cor_data) >= 3 && ncol(cor_data) >= 2) {
    tryCatch({
      # Calculate correlation matrix
      cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
      
      # Create correlation heatmap using ggcorrplot
      p <- ggcorrplot(cor_matrix, 
                      hc.order = TRUE,
                      type = "lower",
                      lab = TRUE,
                      lab_size = 3,
                      method = "circle",
                      colors = c("blue", "white", "red"),
                      title = "Overall Correlation Matrix (Pearson)",
                      ggtheme = theme_minimal()) +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
      plot_filename <- file.path(output_path, paste0("correlation_matrix_overall_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[["correlation_matrix_overall"]] <- p
      plot_files[["correlation_matrix_overall"]] <- plot_filename
      cat("Created overall correlation matrix heatmap\n")
      
    }, error = function(e) {
      cat("Error creating overall correlation matrix:", e$message, "\n")
    })
  }
  
  # 2. Correlation matrix with significance levels
  if (nrow(cor_data) >= 3 && ncol(cor_data) >= 2) {
    tryCatch({
      # Calculate correlation and p-values
      cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
      
      # Calculate p-values
      n <- nrow(cor_data)
      p_values <- matrix(NA, nrow = ncol(cor_data), ncol = ncol(cor_data))
      rownames(p_values) <- colnames(p_values) <- colnames(cor_data)
      
      for (i in 1:ncol(cor_data)) {
        for (j in 1:ncol(cor_data)) {
          if (i != j) {
            cor_test <- cor.test(cor_data[[i]], cor_data[[j]], method = "pearson")
            p_values[i, j] <- cor_test$p.value
          }
        }
      }
      
      # Create plot with significance indicators
      p <- ggcorrplot(cor_matrix,
                      hc.order = TRUE,
                      type = "lower", 
                      p.mat = p_values,
                      sig.level = 0.05,
                      insig = "blank",
                      lab = TRUE,
                      lab_size = 3,
                      colors = c("blue", "white", "red"),
                      title = "Correlation Matrix with Significance (p < 0.05)",
                      ggtheme = theme_minimal()) +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
      plot_filename <- file.path(output_path, paste0("correlation_matrix_significance_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[["correlation_matrix_significance"]] <- p
      plot_files[["correlation_matrix_significance"]] <- plot_filename
      cat("Created correlation matrix with significance indicators\n")
      
    }, error = function(e) {
      cat("Error creating significance correlation matrix:", e$message, "\n")
    })
  }
  
  # 3. Group-wise correlation matrices
  if (!is.null(group_column)) {
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    
    for (group in groups) {
      tryCatch({
        group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variables, drop = FALSE]
        group_data <- group_data[complete.cases(group_data), ]
        
        if (nrow(group_data) >= 3 && ncol(group_data) >= 2) {
          cor_matrix_group <- cor(group_data, method = "pearson", use = "complete.obs")
          
          p <- ggcorrplot(cor_matrix_group,
                          hc.order = TRUE,
                          type = "lower",
                          lab = TRUE,
                          lab_size = 3,
                          colors = c("blue", "white", "red"),
                          title = paste("Correlation Matrix -", group, "Group"),
                          ggtheme = theme_minimal()) +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          
          plot_filename <- file.path(output_path, paste0("correlation_matrix_", group, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
          ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
          
          plots[[paste0("correlation_matrix_", group)]] <- p
          plot_files[[paste0("correlation_matrix_", group)]] <- plot_filename
          cat("Created correlation matrix for group:", group, "\n")
        }
        
      }, error = function(e) {
        cat("Error creating correlation matrix for group", group, ":", e$message, "\n")
      })
    }
  }
  
  # 4. Pairs plot with correlations
  if (length(variables) >= 2 && length(variables) <= 6) {
    tryCatch({
      if (!is.null(group_column)) {
        pairs_data <- data[, c(variables, group_column), drop = FALSE]
        pairs_data <- pairs_data[complete.cases(pairs_data), ]
        
        p <- ggpairs(pairs_data,
                     columns = 1:length(variables),
                     aes(color = .data[[group_column]], alpha = 0.7),
                     title = "Pairwise Correlations by Group") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      } else {
        pairs_data <- data[, variables, drop = FALSE]
        pairs_data <- pairs_data[complete.cases(pairs_data), ]
        
        p <- ggpairs(pairs_data,
                     title = "Pairwise Correlations") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      }
      
      plot_filename <- file.path(output_path, paste0("pairs_correlations_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 12, height = 12, dpi = 300)
      
      plots[["pairs_correlations"]] <- p
      plot_files[["pairs_correlations"]] <- plot_filename
      cat("Created pairs correlation plot\n")
      
    }, error = function(e) {
      cat("Error creating pairs correlation plot:", e$message, "\n")
    })
  }
  
  # 5. Correlation strength distribution plot
  tryCatch({
    # Calculate all pairwise correlations
    cor_matrix <- cor(cor_data, method = "pearson", use = "complete.obs")
    upper_tri <- upper.tri(cor_matrix)
    all_correlations <- cor_matrix[upper_tri]
    all_correlations <- all_correlations[!is.na(all_correlations)]
    
    if (length(all_correlations) > 0) {
      cors_df <- data.frame(correlation = all_correlations)
      cors_df$strength <- sapply(cors_df$correlation, function(r) {
        interpret_correlation_strength(r)$strength
      })
      
      p <- ggplot(cors_df, aes(x = correlation)) +
        geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
        labs(title = "Distribution of Correlation Strengths",
             subtitle = "Histogram of all pairwise correlations",
             x = "Correlation Coefficient", y = "Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 10))
      
      plot_filename <- file.path(output_path, paste0("correlation_distribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
      
      plots[["correlation_distribution"]] <- p
      plot_files[["correlation_distribution"]] <- plot_filename
      cat("Created correlation strength distribution plot\n")
    }
    
  }, error = function(e) {
    cat("Error creating correlation distribution plot:", e$message, "\n")
  })
  
  cat("Correlation visualization creation completed!\n")
  cat("Total plots created:", length(plots), "\n")
  cat("Total plot files saved:", length(plot_files), "\n")
  
  return(list(
    plots = plots,
    plot_files = plot_files
  ))
}

# Convenience function for quick correlation analysis with report
quick_correlation_analysis <- function(data, group_column = NULL, variables = NULL, generate_report = TRUE) {
  
  # Run correlation analysis
  result <- perform_correlation_analysis(data, group_column, variables, include_plots = TRUE)
  
  # Generate report if requested
  if (generate_report) {
    report_file <- quick_report(result)
    cat("Correlation analysis report generated:", report_file, "\n")
    return(list(analysis_result = result, report_file = report_file))
  }
  
  return(result)
} 