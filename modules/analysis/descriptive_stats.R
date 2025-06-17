# Descriptive statistics and group characteristics
# Supports independent groups analysis for medical data


# Load required modules
source("modules/utils/statistical_helpers.R")
source("modules/reporting/export_results.R")

# Centralized modules are loaded in main.R

# Main function: Generate comprehensive descriptive statistics
generate_descriptive_stats <- function(data, group_column = NULL, include_plots = TRUE) {
  
  # Create analysis result object
  result <- create_analysis_result("descriptive_stats")
  
  cat("Generating descriptive statistics...\n")
  
  # Identify variable types
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # Remove group column from analysis if specified
  if (!is.null(group_column)) {
    numeric_vars <- numeric_vars[numeric_vars != group_column]
    categorical_vars <- categorical_vars[categorical_vars != group_column]
  }
  
  # Step 1: Generate master descriptive summary
  cat("\n=== DESCRIPTIVE STATS: STEP 1 - MASTER SUMMARY ===\n")
  master_summary <- generate_master_descriptive_summary(data, group_column, c(numeric_vars, categorical_vars))
  result$master_summary <- master_summary
  result$summary_data <- master_summary$numeric_summary$master_table
  result$categorical_data <- master_summary$categorical_summary
  result$data_summary <- master_summary$overall_summary
  
  cat("- Master descriptive summary completed\n")
  
  # Step 2: Comprehensive assumptions testing
  if (length(numeric_vars) > 0) {
    cat("\n=== DESCRIPTIVE STATS: STEP 2 - ASSUMPTIONS TESTING ===\n")
    assumptions_results <- perform_assumptions_testing(data, numeric_vars, group_column)
    result$assumptions_analysis <- assumptions_results
    result$normality_analysis <- assumptions_results$normality_tests
    result$homogeneity_analysis <- assumptions_results$homogeneity_tests
    
    cat("- Comprehensive assumptions testing completed\n")
  }
  
  # Step 3: Generate plots if requested
  if (include_plots) {
    cat("\n=== DESCRIPTIVE STATS: STEP 3 - GENERATING VISUALIZATIONS ===\n")
    
    # Use fixed output path for plots
    plots_output_path <- file.path("output", "plots", "descriptive_stats")
    
    plots_result <- create_descriptive_plots(data, numeric_vars, categorical_vars, group_column, plots_output_path)
    result$plots <- plots_result$plots
    result$plot_files <- plots_result$plot_files
    cat("- Generated", length(plots_result$plots), "descriptive plots\n")
    cat("- Saved", length(plots_result$plot_files), "plot files\n")
  }
  
  # Add metadata
  result$metadata <- list(
    total_observations = nrow(data),
    total_variables = ncol(data),
    numeric_variables = length(numeric_vars),
    categorical_variables = length(categorical_vars),
    group_column = group_column,
    missing_data_present = any(sapply(data, function(x) any(is.na(x)))),
    analysis_date = Sys.time()
  )
  
  # Ensure compatibility with HTML report generation by restructuring data
  # Map new structure to old expected structure
  
  # Create data_summary structure expected by HTML report
  result$data_summary <- list(
    dataset_overview = data.frame(
      metric = c("Total observations", "Total variables", "Numeric variables", "Categorical variables", "Group column"),
      value = c(nrow(data), ncol(data), length(numeric_vars), length(categorical_vars), group_column),
      stringsAsFactors = FALSE
    ),
    variable_types = data.frame(
      type = c("Numeric", "Categorical"),
      count = c(length(numeric_vars), length(categorical_vars)),
      stringsAsFactors = FALSE
    ),
    group_summary = {
      group_table <- table(data[[group_column]])
      data.frame(
        group = names(group_table),
        n = as.numeric(group_table),
        percentage = round(as.numeric(prop.table(group_table)) * 100, 2),
        stringsAsFactors = FALSE
      )
    },
    missing_data = {
      missing_counts <- sapply(data, function(x) sum(is.na(x)))
      missing_vars <- missing_counts[missing_counts > 0]
      if (length(missing_vars) > 0) {
        data.frame(
          variable = names(missing_vars),
          missing_count = as.numeric(missing_vars),
          missing_percentage = round(as.numeric(missing_vars) / nrow(data) * 100, 2),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          variable = character(0),
          missing_count = numeric(0),
          missing_percentage = numeric(0),
          stringsAsFactors = FALSE
        )
      }
    }
  )
  
  # Map summary_data for continuous variables (expected by HTML report)
  if (!is.null(master_summary$numeric_summary) && !is.null(master_summary$numeric_summary$master_table)) {
    # Map column names to expected HTML report format
    master_table <- master_summary$numeric_summary$master_table
    
    # Create properly formatted table for HTML report
    result$summary_data <- data.frame(
      variable = master_table$Variable,
      n = master_table$N,
      missing = master_table$Missing,
      mean = round(master_table$Mean, 3),
      sd = round(master_table$SD, 3),
      median = round(master_table$Median, 3),
      q25 = round(master_table$Q25, 3),
      q75 = round(master_table$Q75, 3),
      min = round(master_table$Min, 3),
      max = round(master_table$Max, 3),
      range = round(master_table$Max - master_table$Min, 3),
      iqr = round(master_table$Q75 - master_table$Q25, 3),
      cv = round(master_table$CV_Pct, 1),
      skewness = round(master_table$Skewness, 3),
      kurtosis = round(master_table$Kurtosis, 3),
      stringsAsFactors = FALSE
    )
  }
  
  # Map categorical_data (expected by HTML report)
  if (!is.null(master_summary$categorical_summary)) {
    # Convert categorical summary format to expected format
    result$categorical_data <- list()
    for (var_name in names(master_summary$categorical_summary)) {
      var_data <- master_summary$categorical_summary[[var_name]]
      if (!is.null(var_data$summary_table)) {
        result$categorical_data[[var_name]] <- var_data$summary_table
      } else if (!is.null(var_data$overall_summary)) {
        result$categorical_data[[var_name]] <- var_data$overall_summary
      } else {
        # Fallback: create basic categorical summary
        if (var_name %in% names(data)) {
          freq_table <- table(data[[var_name]], useNA = "ifany")
          prop_table <- prop.table(freq_table) * 100
          result$categorical_data[[var_name]] <- data.frame(
            category = names(freq_table),
            frequency = as.numeric(freq_table),
            percentage = round(as.numeric(prop_table), 2),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  # Map normality_analysis to expected HTML report format
  if (!is.null(result$assumptions_analysis) && !is.null(result$assumptions_analysis$normality_tests)) {
    # Convert assumptions dashboard format to HTML expected format
    result$normality_analysis <- list()
    for (var_name in names(result$assumptions_analysis$normality_tests)) {
      norm_data <- result$assumptions_analysis$normality_tests[[var_name]]
      
      # Extract overall test results
      overall_test <- norm_data$overall_test
      descriptive_measures <- norm_data$descriptive_measures
      
      # Map to expected HTML format
      result$normality_analysis[[var_name]] <- list(
        test = if (!is.null(overall_test$test)) overall_test$test else "Not available",
        statistic = if (!is.null(overall_test$statistic)) overall_test$statistic else NULL,
        p_value = if (!is.null(overall_test$p_value)) overall_test$p_value else NULL,
        normal = if (!is.null(overall_test$normal)) overall_test$normal else FALSE,
        interpretation = if (!is.null(overall_test$interpretation)) overall_test$interpretation else "",
        skewness = if (!is.null(descriptive_measures$skewness)) descriptive_measures$skewness else NULL,
        kurtosis = if (!is.null(descriptive_measures$kurtosis)) descriptive_measures$kurtosis else NULL,
        group_normality = if (!is.null(norm_data$group_tests)) norm_data$group_tests else NULL
      )
    }
  }
  
  # Add variable_properties for HTML report compatibility
  result$variable_properties <- list(
    properties_table = if (!is.null(result$assumptions_analysis$assumptions_summary)) {
      result$assumptions_analysis$assumptions_summary
    } else {
      data.frame()
    },
    homogeneity_p_values = if (!is.null(result$assumptions_analysis$homogeneity_tests)) {
      lapply(result$assumptions_analysis$homogeneity_tests, function(x) x$p_value)
    } else {
      list()
    }
  )
  
  cat("Descriptive statistics analysis completed successfully.\n")
  return(result)
}

# Create advanced descriptive plots with comprehensive visualizations
create_descriptive_plots <- function(data, numeric_vars, categorical_vars, group_column = NULL, output_path = "output/plots/") {
  
  # Ensure required packages are available for plotting
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggpubr) 
    library(GGally)
    library(dplyr)
    library(scales)
  })
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  cat("Creating advanced descriptive visualizations...\n")
  
  # Helper function to calculate optimal plot parameters
  calculate_plot_parameters <- function(var_data) {
    n_obs <- length(var_data)
    data_range <- max(var_data) - min(var_data)
    
    # Handle edge cases
    if (data_range == 0) {
      # All values are the same - use fixed parameters
      return(list(
        binwidth = 1,
        bandwidth = 1,
        x_limits = c(min(var_data) - 2, max(var_data) + 2),
        n_obs = n_obs
      ))
    }
    
    # Sturges' rule for binwidth (adjusted for practical use)
    optimal_binwidth <- data_range / (1 + 3.322 * log10(n_obs))
    binwidth <- max(0.1, min(optimal_binwidth, data_range / 15))
    
    # Scott's rule for bandwidth (with practical limits)
    var_sd <- sd(var_data)
    var_iqr <- IQR(var_data)
    
    # Handle cases where sd or IQR might be 0
    if (var_sd == 0) var_sd <- data_range / 4  # Fallback
    if (var_iqr == 0) var_iqr <- data_range / 2  # Fallback
    
    bw_scott <- 3.5 * var_sd * n_obs^(-1/3)
    bw_nrd <- 0.9 * min(var_sd, var_iqr / 1.34) * n_obs^(-1/5)
    bw_optimal <- max(min(bw_scott, bw_nrd), binwidth / 3, data_range / 100)
    
    # Axis limits with buffer for kernel density
    x_min <- min(var_data)
    x_max <- max(var_data)
    x_range <- x_max - x_min
    x_buffer <- max(x_range * 0.05, 2 * bw_optimal)  # Buffer for kernel
    x_limits <- c(x_min - x_buffer, x_max + x_buffer)
    
    return(list(
      binwidth = binwidth,
      bandwidth = bw_optimal,
      x_limits = x_limits,
      n_obs = n_obs
    ))
  }
  
  # 1. Histograms with density overlays and normality curves
  # Improvements: consistent binwidth, controlled bandwidth, unified axis limits
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(6, length(numeric_vars))]) {
      tryCatch({
        # Calculate data and distribution parameters
        var_data <- data[[var]][!is.na(data[[var]])]
        mean_val <- mean(var_data)
        sd_val <- sd(var_data)
        
        # Get optimal plot parameters using helper function
        plot_params <- calculate_plot_parameters(var_data)
        
        if (is.null(group_column)) {
          p <- ggplot(data, aes(x = .data[[var]])) +
            geom_histogram(aes(y = after_stat(density)), binwidth = plot_params$binwidth, 
                          fill = "steelblue", alpha = 0.7, color = "white") +
            geom_density(color = "red", linewidth = 1.2, alpha = 0.8, bw = plot_params$bandwidth) +
            stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                         color = "blue", linetype = "dashed", linewidth = 1) +
            scale_x_continuous(limits = plot_params$x_limits, 
                              expand = expansion(mult = 0, add = 0)) +
            labs(title = paste("Distribution Analysis of", var),
                 subtitle = paste("Red: Actual density, Blue dashed: Normal distribution (n =", plot_params$n_obs, ")"),
                 x = var, y = "Density") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10))
        } else {
          p <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
            geom_histogram(aes(y = after_stat(density)), binwidth = plot_params$binwidth, 
                          alpha = 0.7, position = "identity", color = "black", linewidth = 0.3) +
            geom_density(aes(color = .data[[group_column]]), alpha = 0.8, 
                        linewidth = 1.2, bw = plot_params$bandwidth) +
            facet_wrap(as.formula(paste("~", group_column))) +
            scale_x_continuous(limits = plot_params$x_limits, 
                              expand = expansion(mult = 0, add = 0),
                              breaks = scales::pretty_breaks(n = 6)) +
            labs(title = paste("Distribution Analysis of", var, "by", group_column),
                 subtitle = paste("Unified axis limits for comparison (binwidth =", round(plot_params$binwidth, 2), 
                                 ", bw =", round(plot_params$bandwidth, 2), ")"),
                 x = var, y = "Density") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10),
                  strip.text = element_text(face = "bold")) +
            scale_fill_brewer(type = "qual", palette = "Set2") +
            scale_color_brewer(type = "qual", palette = "Set2")
        }
        
        plot_filename <- file.path(output_path, paste0("histogram_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 12, height = 8, dpi = 300)
        
        plots[[paste0("histogram_", var)]] <- p
        plot_files[[paste0("histogram_", var)]] <- plot_filename
        cat("Created histogram for", var, "with optimized binwidth (", round(plot_params$binwidth, 2), 
            ") and bandwidth (", round(plot_params$bandwidth, 2), ")\n")
        
              }, error = function(e) {
          cat("Error creating histogram for", var, ":", e$message, "\n")
        })
    }
  }
  
  # 6. Pairs plot for numeric variables (if not too many)
  if (length(numeric_vars) >= 3 && length(numeric_vars) <= 6) {
    tryCatch({
      pairs_data <- data[, c(numeric_vars, if(!is.null(group_column)) group_column), drop = FALSE]
      
      if (!is.null(group_column)) {
        p <- ggpairs(pairs_data, 
                     columns = 1:length(numeric_vars),
                     aes(color = .data[[group_column]], alpha = 0.7),
                     title = "Pairs Plot of Numeric Variables by Group") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      } else {
        p <- ggpairs(pairs_data, 
                     columns = 1:length(numeric_vars),
                     title = "Pairs Plot of Numeric Variables") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      }
      
      plot_filename <- file.path(output_path, paste0("pairs_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
      ggsave(plot_filename, plot = p, width = 12, height = 12, dpi = 300)
      
      plots[["pairs_plot"]] <- p
      plot_files[["pairs_plot"]] <- plot_filename
      cat("Created pairs plot\n")
      
    }, error = function(e) {
      cat("Error creating pairs plot:", e$message, "\n")
    })
  }
  
  # 7. Bar plots for categorical variables
  if (length(categorical_vars) > 0) {
    for (var in categorical_vars[1:min(4, length(categorical_vars))]) {
      
      if (length(unique(data[[var]])) > 20) next
      
      tryCatch({
        if (!is.null(group_column)) {
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
            labs(title = paste("Distribution of", var, "by", group_column, "(%)"),
                 subtitle = "Showing percentages within each group",
                 x = var, y = "Percentage (%)", fill = group_column) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_brewer(type = "qual", palette = "Set2")
        } else {
          # Simple frequency plot using base R
          freq_table <- table(data[[var]])
          freq_data <- data.frame(
            var_val = names(freq_table),
            n = as.numeric(freq_table),
            stringsAsFactors = FALSE
          )
          freq_data$prop <- freq_data$n / sum(freq_data$n) * 100
          names(freq_data)[1] <- var
          
          p <- ggplot(freq_data, aes(x = .data[[var]], y = n)) +
            geom_col(fill = "steelblue", alpha = 0.8) +
            geom_text(aes(label = paste0(n, " (", round(prop, 1), "%)")), 
                     vjust = -0.5, size = 3) +
            labs(title = paste("Frequency Distribution of", var),
                 subtitle = "Count and percentage for each category",
                 x = var, y = "Count") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10),
                  axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
        plot_filename <- file.path(output_path, paste0("barplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[[paste0("barplot_", var)]] <- p
        plot_files[[paste0("barplot_", var)]] <- plot_filename
        cat("Created barplot for", var, "\n")
        
              }, error = function(e) {
          cat("Error creating barplot for", var, ":", e$message, "\n")
        })
    }
  }
  
  # 8. Missing data visualization
  if (any(sapply(data, function(x) any(is.na(x))))) {
    tryCatch({
      # Missing data pattern plot
      missing_data <- data.frame(
        variable = names(data),
        missing_count = sapply(data, function(x) sum(is.na(x))),
        missing_percentage = sapply(data, function(x) sum(is.na(x)) / length(x) * 100),
        stringsAsFactors = FALSE
      )
      missing_data <- missing_data[missing_data$missing_count > 0, ]
      
      if (nrow(missing_data) > 0) {
        p <- ggplot(missing_data, aes(x = reorder(variable, missing_percentage), y = missing_percentage)) +
          geom_col(fill = "coral", alpha = 0.8) +
          geom_text(aes(label = paste0(missing_count, " (", round(missing_percentage, 1), "%)")), 
                   hjust = -0.1, size = 3) +
          coord_flip() +
          labs(title = "Missing Data Analysis",
               subtitle = "Count and percentage of missing values by variable",
               x = "Variable", y = "Missing Percentage (%)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10))
        
        plot_filename <- file.path(output_path, paste0("missing_data_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[["missing_data_analysis"]] <- p
        plot_files[["missing_data_analysis"]] <- plot_filename
        cat("Created missing data analysis plot\n")
      }
    }, error = function(e) {
      cat("Error creating missing data plot:", e$message, "\n")
    })
  }
  
  # 9. Outlier visualization for numeric variables
  if (length(numeric_vars) > 0) {
    tryCatch({
      # Create outlier summary plot
      outlier_summary <- data.frame(
        variable = character(),
        outlier_count = numeric(),
        outlier_percentage = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (var in numeric_vars) {
        var_data <- data[[var]][!is.na(data[[var]])]
        if (length(var_data) >= 4) {
          Q1 <- quantile(var_data, 0.25)
          Q3 <- quantile(var_data, 0.75)
          IQR_val <- Q3 - Q1
          outliers <- sum(var_data < (Q1 - 1.5 * IQR_val) | var_data > (Q3 + 1.5 * IQR_val))
          
          outlier_summary <- rbind(outlier_summary, data.frame(
            variable = var,
            outlier_count = outliers,
            outlier_percentage = outliers / length(var_data) * 100,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      if (nrow(outlier_summary) > 0) {
        p <- ggplot(outlier_summary, aes(x = reorder(variable, outlier_percentage), y = outlier_percentage)) +
          geom_col(fill = "orange", alpha = 0.8) +
          geom_text(aes(label = paste0(outlier_count, " (", round(outlier_percentage, 1), "%)")), 
                   hjust = -0.1, size = 3) +
          coord_flip() +
          labs(title = "Outlier Analysis Summary",
               subtitle = "IQR method: Count and percentage of outliers by variable",
               x = "Variable", y = "Outlier Percentage (%)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10))
        
        plot_filename <- file.path(output_path, paste0("outlier_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[["outlier_analysis"]] <- p
        plot_files[["outlier_analysis"]] <- plot_filename
        cat("Created outlier analysis plot\n")
      }
    }, error = function(e) {
      cat("Error creating outlier analysis plot:", e$message, "\n")
    })
  }
  
  cat("Advanced descriptive visualization creation completed!\n")
  cat("Total plots created:", length(plots), "\n")
  cat("Total plot files saved:", length(plot_files), "\n")
  
  return(list(
    plots = plots,
    plot_files = plot_files
  ))
}

