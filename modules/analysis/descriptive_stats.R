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
  
  # Step 1: Generate master descriptive summary (replaces duplicated descriptive stats)
  cat("\n=== STEP 1: MASTER DESCRIPTIVE SUMMARY ===\n")
  master_summary <- generate_master_descriptive_summary(data, group_column, c(numeric_vars, categorical_vars))
  result$master_summary <- master_summary
  result$summary_data <- master_summary$numeric_summary$master_table
  result$categorical_data <- master_summary$categorical_summary
  result$data_summary <- master_summary$overall_summary
  
  cat("- Master descriptive summary completed\n")
  
  # Step 2: Comprehensive assumptions testing (replaces duplicated normality tests)
  if (length(numeric_vars) > 0) {
    cat("\n=== STEP 2: ASSUMPTIONS TESTING DASHBOARD ===\n")
    assumptions_results <- perform_assumptions_testing(data, numeric_vars, group_column)
    result$assumptions_analysis <- assumptions_results
    result$normality_analysis <- assumptions_results$normality_tests
    result$homogeneity_analysis <- assumptions_results$homogeneity_tests
    
    cat("- Comprehensive assumptions testing completed\n")
  }
  
  # Step 3: Generate plots if requested
  if (include_plots) {
    cat("\n=== STEP 3: GENERATING VISUALIZATIONS ===\n")
    
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

# DEPRECATED: Use generate_master_descriptive_summary instead
# Calculate comprehensive statistics for continuous variables
calculate_continuous_stats <- function(data, variables, group_column = NULL) {
  
  if (length(variables) == 0) {
    return(NULL)
  }
  
  if (is.null(group_column)) {
    # Overall statistics
    stats_df <- data.frame(
      variable = variables,
      n = sapply(variables, function(v) sum(!is.na(data[[v]]))),
      missing = sapply(variables, function(v) sum(is.na(data[[v]]))),
      mean = sapply(variables, function(v) mean(data[[v]], na.rm = TRUE)),
      sd = sapply(variables, function(v) sd(data[[v]], na.rm = TRUE)),
      median = sapply(variables, function(v) median(data[[v]], na.rm = TRUE)),
      q25 = sapply(variables, function(v) quantile(data[[v]], 0.25, na.rm = TRUE)),
      q75 = sapply(variables, function(v) quantile(data[[v]], 0.75, na.rm = TRUE)),
      min = sapply(variables, function(v) min(data[[v]], na.rm = TRUE)),
      max = sapply(variables, function(v) max(data[[v]], na.rm = TRUE)),
      range = sapply(variables, function(v) max(data[[v]], na.rm = TRUE) - min(data[[v]], na.rm = TRUE)),
      iqr = sapply(variables, function(v) IQR(data[[v]], na.rm = TRUE)),
      cv = sapply(variables, function(v) {
        m <- mean(data[[v]], na.rm = TRUE)
        s <- sd(data[[v]], na.rm = TRUE)
        if (m != 0) s/m * 100 else NA
      }),
      skewness = sapply(variables, function(v) calculate_skewness(data[[v]])),
      kurtosis = sapply(variables, function(v) calculate_kurtosis(data[[v]])),
      stringsAsFactors = FALSE
    )
    
    # Round numeric columns
    numeric_cols <- sapply(stats_df, is.numeric)
    stats_df[numeric_cols] <- lapply(stats_df[numeric_cols], function(x) round(x, 3))
    
  } else {
    # Group-wise statistics
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    
    group_stats_list <- list()
    
    for (group in groups) {
      group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), ]
      
      group_stats <- data.frame(
        variable = variables,
        group = group,
        n = sapply(variables, function(v) sum(!is.na(group_data[[v]]))),
        missing = sapply(variables, function(v) sum(is.na(group_data[[v]]))),
        mean = sapply(variables, function(v) mean(group_data[[v]], na.rm = TRUE)),
        sd = sapply(variables, function(v) sd(group_data[[v]], na.rm = TRUE)),
        median = sapply(variables, function(v) median(group_data[[v]], na.rm = TRUE)),
        q25 = sapply(variables, function(v) quantile(group_data[[v]], 0.25, na.rm = TRUE)),
        q75 = sapply(variables, function(v) quantile(group_data[[v]], 0.75, na.rm = TRUE)),
        min = sapply(variables, function(v) min(group_data[[v]], na.rm = TRUE)),
        max = sapply(variables, function(v) max(group_data[[v]], na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
      
      group_stats_list[[as.character(group)]] <- group_stats
    }
    
    stats_df <- do.call(rbind, group_stats_list)
    
    # Round numeric columns
    numeric_cols <- sapply(stats_df, is.numeric)
    stats_df[numeric_cols] <- lapply(stats_df[numeric_cols], function(x) round(x, 3))
  }
  
  return(stats_df)
}

# Calculate statistics for categorical variables
calculate_categorical_stats <- function(data, variables, group_column = NULL) {
  
  if (length(variables) == 0) {
    return(NULL)
  }
  
  categorical_stats_list <- list()
  
  for (var in variables) {
    if (is.null(group_column)) {
      # Overall frequencies
      freq_table <- table(data[[var]], useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      var_stats <- data.frame(
        variable = var,
        category = names(freq_table),
        frequency = as.numeric(freq_table),
        percentage = round(as.numeric(prop_table), 2),
        stringsAsFactors = FALSE
      )
      
    } else {
      # Cross-tabulation with groups
      cross_table <- table(data[[var]], data[[group_column]], useNA = "ifany")
      
      # Convert to data frame for easier handling
      var_stats <- as.data.frame(cross_table)
      names(var_stats) <- c("category", "group", "frequency")
      var_stats$variable <- var
      
      # Calculate percentages within groups
      var_stats_grouped <- split(var_stats, var_stats$group)
      var_stats_with_pct <- do.call(rbind, lapply(var_stats_grouped, function(group_data) {
        group_data$percentage <- round(group_data$frequency / sum(group_data$frequency) * 100, 2)
        return(group_data)
      }))
      var_stats <- var_stats_with_pct[, c("variable", "category", "group", "frequency", "percentage")]
    }
    
    categorical_stats_list[[var]] <- var_stats
  }
  
  return(categorical_stats_list)
}

# Create comprehensive summary table
create_summary_table <- function(data, group_column = NULL) {
  
  summary_info <- list(
    dataset_overview = data.frame(
      metric = c("Total observations", "Total variables", "Missing observations", 
                 "Complete observations", "Missing data percentage"),
      value = c(
        nrow(data),
        ncol(data),
        sum(!complete.cases(data)),
        sum(complete.cases(data)),
        round(sum(!complete.cases(data)) / nrow(data) * 100, 2)
      ),
      stringsAsFactors = FALSE
    )
  )
  
  # Variable type summary
  var_types <- data.frame(
    type = c("Numeric", "Character", "Factor", "Logical", "Date"),
    count = c(
      sum(sapply(data, is.numeric)),
      sum(sapply(data, is.character)),
      sum(sapply(data, is.factor)),
      sum(sapply(data, is.logical)),
      sum(sapply(data, function(x) inherits(x, "Date")))
    ),
    stringsAsFactors = FALSE
  )
  var_types <- var_types[var_types$count > 0, ]
  
  summary_info$variable_types <- var_types
  
  # Group summary if group column specified
  if (!is.null(group_column) && group_column %in% names(data)) {
    group_summary <- data.frame(
      group = names(table(data[[group_column]])),
      n = as.numeric(table(data[[group_column]])),
      percentage = round(as.numeric(prop.table(table(data[[group_column]]))) * 100, 2),
      stringsAsFactors = FALSE
    )
    
    summary_info$group_summary <- group_summary
  }
  
  # Missing data summary by variable
  missing_summary <- data.frame(
    variable = names(data),
    missing_count = sapply(data, function(x) sum(is.na(x))),
    missing_percentage = round(sapply(data, function(x) sum(is.na(x)) / length(x) * 100), 2),
    stringsAsFactors = FALSE
  )
  missing_summary <- missing_summary[missing_summary$missing_count > 0, ]
  
  if (nrow(missing_summary) > 0) {
    summary_info$missing_data <- missing_summary
  }
  
  return(summary_info)
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

# Note: calculate_skewness and calculate_kurtosis are now sourced from statistical_helpers.R

# Convenience function for quick descriptive analysis with report
quick_descriptive_analysis <- function(data, group_column = NULL, generate_report = TRUE) {
  
  # Run descriptive statistics
  result <- generate_descriptive_stats(data, group_column, include_plots = TRUE)
  
  # Generate report if requested
  if (generate_report) {
    report_file <- quick_report(result)
    cat("Descriptive statistics report generated:", report_file, "\n")
    return(list(analysis_result = result, report_file = report_file))
  }
  
  return(result)
}

# Perform normality tests for numeric variables
perform_normality_tests <- function(data, variables, group_column = NULL) {
  
  normality_results <- list()
  
  for (var in variables) {
    var_data <- data[[var]][!is.na(data[[var]])]
    
    if (length(var_data) < 3) {
      normality_results[[var]] <- list(
        variable = var,
        test = "insufficient_data",
        interpretation = "Insufficient data for normality testing"
      )
      next
    }
    
    # Choose appropriate test based on sample size
    if (length(var_data) <= 50) {
      # Shapiro-Wilk test for small samples
      test_result <- shapiro.test(var_data)
      test_name <- "Shapiro-Wilk"
    } else {
      # Kolmogorov-Smirnov test for larger samples
      test_result <- ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
      test_name <- "Kolmogorov-Smirnov"
    }
    
    is_normal <- test_result$p.value > 0.05
    
    # Additional descriptive measures
    skewness_val <- calculate_skewness(var_data)
    kurtosis_val <- calculate_kurtosis(var_data)
    
    interpretation <- ifelse(is_normal, 
                                    paste("Data appears normally distributed (", test_name, " p =",
                                format.pval(test_result$p.value, digits = 3), ")"),
        paste("Data deviates from normal distribution (", test_name, " p =",
                                format.pval(test_result$p.value, digits = 3), ")"))
    
    normality_results[[var]] <- list(
      variable = var,
      test = test_name,
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      normal = is_normal,
      skewness = skewness_val,
      kurtosis = kurtosis_val,
      interpretation = interpretation
    )
    
    # Group-wise normality if group column specified
    if (!is.null(group_column)) {
      groups <- unique(data[[group_column]])
      groups <- groups[!is.na(groups)]
      
      group_normality <- list()
      for (group in groups) {
        group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), var]
        group_data <- group_data[!is.na(group_data)]
        
        if (length(group_data) >= 3) {
          if (length(group_data) <= 50) {
            group_test <- shapiro.test(group_data)
            group_test_name <- "Shapiro-Wilk"
          } else {
            group_test <- ks.test(group_data, "pnorm", mean(group_data), sd(group_data))
            group_test_name <- "Kolmogorov-Smirnov"
          }
          
          group_normality[[as.character(group)]] <- list(
            test = group_test_name,
            p_value = group_test$p.value,
            normal = group_test$p.value > 0.05
          )
        }
      }
      normality_results[[var]]$group_normality <- group_normality
    }
  }
  
  return(normality_results)
}

# Perform outlier analysis
perform_outlier_analysis <- function(data, variables, group_column = NULL) {
  
  outlier_results <- list()
  
  for (var in variables) {
    var_data <- data[[var]][!is.na(data[[var]])]
    
    if (length(var_data) < 4) {
      outlier_results[[var]] <- list(
        variable = var,
        method = "insufficient_data",
        outliers = NULL,
        interpretation = "Insufficient data for outlier detection"
      )
      next
    }
    
    # IQR method
    Q1 <- quantile(var_data, 0.25)
    Q3 <- quantile(var_data, 0.75)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    iqr_outliers <- which(data[[var]] < lower_bound | data[[var]] > upper_bound)
    
    # Z-score method (|z| > 3)
    z_scores <- abs(scale(var_data))
    z_outliers <- which(z_scores > 3)
    
    # Modified Z-score method using median
    median_val <- median(var_data)
    mad_val <- mad(var_data)
    modified_z_scores <- 0.6745 * (var_data - median_val) / mad_val
    modified_z_outliers <- which(abs(modified_z_scores) > 3.5)
    
    outlier_results[[var]] <- list(
      variable = var,
      iqr_outliers = iqr_outliers,
      iqr_bounds = c(lower_bound, upper_bound),
      z_outliers = z_outliers,
      modified_z_outliers = modified_z_outliers,
      outlier_percentage = round(length(iqr_outliers) / length(var_data) * 100, 2),
      interpretation = paste("IQR method detected", length(iqr_outliers), "outliers (", 
                            round(length(iqr_outliers) / length(var_data) * 100, 2), "%)")
    )
    
    # Group-wise outlier analysis
    if (!is.null(group_column)) {
      groups <- unique(data[[group_column]])
      groups <- groups[!is.na(groups)]
      
      group_outliers <- list()
      for (group in groups) {
        group_indices <- which(data[[group_column]] == group & !is.na(data[[group_column]]))
        group_data <- data[group_indices, var]
        group_data <- group_data[!is.na(group_data)]
        
        if (length(group_data) >= 4) {
          group_Q1 <- quantile(group_data, 0.25)
          group_Q3 <- quantile(group_data, 0.75)
          group_IQR <- group_Q3 - group_Q1
          group_lower <- group_Q1 - 1.5 * group_IQR
          group_upper <- group_Q3 + 1.5 * group_IQR
          
          group_outlier_indices <- group_indices[which(group_data < group_lower | group_data > group_upper)]
          
          group_outliers[[as.character(group)]] <- list(
            outlier_indices = group_outlier_indices,
            outlier_count = length(group_outlier_indices),
            outlier_percentage = round(length(group_outlier_indices) / length(group_data) * 100, 2)
          )
        }
      }
      outlier_results[[var]]$group_outliers <- group_outliers
    }
  }
  
  return(outlier_results)
}

# Create comprehensive variable properties table for each group
create_variable_properties_table <- function(data, numeric_vars, group_column, normality_results, outlier_results) {
  
  if (is.null(group_column) || length(numeric_vars) == 0) {
    return(NULL)
  }
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  # Initialize results data frame
  properties_df <- data.frame(
    Variable = character(),
    Group = character(),
    N = integer(),
    Missing = integer(),
    Mean = numeric(),
    SD = numeric(),
    Median = numeric(),
    IQR = numeric(),
    CV_Percent = numeric(),
    Skewness = numeric(),
    Kurtosis = numeric(),
    Normal_Distribution = character(),
    Normality_Test = character(),
    Normality_P_Value = numeric(),
    Outliers_Count = integer(),
    Outliers_Percent = numeric(),
    Homogeneity_Status = character(),
    Data_Quality = character(),
    Recommended_Test = character(),
    Post_Hoc_Needed = character(),
    Alternative_Tests = character(),
    Effect_Size_Measure = character(),
    Borderline_Cases = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate homogeneity for each variable (Levene's test)
  homogeneity_results <- list()
  for (var in numeric_vars) {
    clean_data <- data[!is.na(data[[var]]) & !is.na(data[[group_column]]), ]
    
    if (nrow(clean_data) >= 6 && length(unique(clean_data[[group_column]])) >= 2) {
      tryCatch({
        levene_result <- leveneTest(clean_data[[var]], clean_data[[group_column]])
        homogeneity_results[[var]] <- list(
          p_value = levene_result$`Pr(>F)`[1],
          homogeneous = levene_result$`Pr(>F)`[1] > 0.05
        )
      }, error = function(e) {
        homogeneity_results[[var]] <- list(
          p_value = NA,
          homogeneous = NA
        )
      })
    } else {
      homogeneity_results[[var]] <- list(
        p_value = NA,
        homogeneous = NA
      )
    }
  }
  
  # Fill the table for each variable and group
  for (var in numeric_vars) {
    for (group in groups) {
      # Get group-specific data
      group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), ]
      var_data <- group_data[[var]]
      var_data_clean <- var_data[!is.na(var_data)]
      
      # Basic statistics
      n_obs <- length(var_data_clean)
      n_missing <- sum(is.na(var_data))
      mean_val <- if(n_obs > 0) mean(var_data_clean) else NA
      sd_val <- if(n_obs > 1) sd(var_data_clean) else NA
      median_val <- if(n_obs > 0) median(var_data_clean) else NA
      iqr_val <- if(n_obs > 3) IQR(var_data_clean) else NA
      cv_val <- if(!is.na(mean_val) && mean_val != 0 && !is.na(sd_val)) (sd_val/mean_val) * 100 else NA
      skew_val <- if(n_obs >= 3) calculate_skewness(var_data_clean) else NA
      kurt_val <- if(n_obs >= 4) calculate_kurtosis(var_data_clean) else NA
      
      # Normality information
      normality_info <- normality_results[[var]]
      group_normality <- if(!is.null(normality_info$group_normality) && 
                           !is.null(normality_info$group_normality[[as.character(group)]])) {
        normality_info$group_normality[[as.character(group)]]
      } else {
        list(normal = NA, test = "Not tested", p_value = NA)
      }
      
      normal_status <- if(is.na(group_normality$normal)) {
        "Unknown"
      } else if(group_normality$normal) {
        "Normal"
      } else {
        "Non-normal"
      }
      
      # Outlier information
      outlier_info <- outlier_results[[var]]
      group_outliers <- if(!is.null(outlier_info$group_outliers) && 
                          !is.null(outlier_info$group_outliers[[as.character(group)]])) {
        outlier_info$group_outliers[[as.character(group)]]
      } else {
        list(outlier_count = 0, outlier_percentage = 0)
      }
      
      # Homogeneity status with proper error checking
      homog_info <- homogeneity_results[[var]]
      homog_status <- if(is.null(homog_info) || is.null(homog_info$homogeneous) || is.na(homog_info$homogeneous)) {
        "Unknown"
      } else if(homog_info$homogeneous) {
        "Homogeneous"
      } else {
        "Heterogeneous"
      }
      
      # Data quality assessment
      quality_issues <- c()
      if(n_missing > 0) quality_issues <- c(quality_issues, paste0(n_missing, " missing"))
      if(group_outliers$outlier_percentage > 10) quality_issues <- c(quality_issues, "High outliers")
      if(group_outliers$outlier_percentage > 5 && group_outliers$outlier_percentage <= 10) quality_issues <- c(quality_issues, "Moderate outliers")
      if(cv_val > 100) quality_issues <- c(quality_issues, "High variability")
      
      data_quality <- if(length(quality_issues) == 0) {
        "Good"
      } else if(length(quality_issues) == 1) {
        "Fair"
      } else {
        "Poor"
      }
      
      # Recommended statistical test - simplified logic for descriptive purposes
      recommended_test <- "See comparative analysis"
      homog_available <- !is.null(homog_info) && !is.null(homog_info$homogeneous) && !is.na(homog_info$homogeneous)
      
      if(!is.na(group_normality$normal) && homog_available) {
        if(group_outliers$outlier_percentage > 15) {
          recommended_test <- "Non-parametric (high outliers)"
        } else if(length(groups) == 2) {
          if(group_normality$normal && homog_info$homogeneous) {
            recommended_test <- "Student's t-test"
          } else if(group_normality$normal && !homog_info$homogeneous) {
            recommended_test <- "Welch's t-test"
          } else {
            recommended_test <- "Mann-Whitney U"
          }
        } else if(length(groups) > 2) {
          if(group_normality$normal && homog_info$homogeneous) {
            recommended_test <- "ANOVA + Tukey HSD"
          } else if(group_normality$normal && !homog_info$homogeneous) {
            recommended_test <- "Welch's ANOVA"
          } else {
            recommended_test <- "Kruskal-Wallis + Dunn"
          }
        }
      }
      
      # Determine post-hoc needs and alternatives
      post_hoc_needed <- "No"
      alternative_tests <- "None"
      effect_size_measure <- "None"
      borderline_cases <- "None"
      
      if (length(groups) > 2) {
        if (grepl("ANOVA", recommended_test)) {
          post_hoc_needed <- "Tukey HSD (if p < 0.05)"
          alternative_tests <- "Kruskal-Wallis + Dunn"
          effect_size_measure <- "Eta-squared (η²)"
          borderline_cases <- "Run both parametric & non-parametric if assumptions borderline"
        } else if (grepl("Kruskal-Wallis", recommended_test)) {
          post_hoc_needed <- "Dunn test (if p < 0.05)"
          alternative_tests <- "ANOVA + Tukey (if transform succeeds)"
          effect_size_measure <- "Rank-biserial correlation"
          borderline_cases <- "Try log/sqrt transformation"
        }
      } else if (length(groups) == 2) {
        if (grepl("t-test", recommended_test)) {
          effect_size_measure <- "Cohen's d"
          alternative_tests <- "Mann-Whitney U"
          borderline_cases <- "Check assumption p-values"
        } else if (grepl("Mann-Whitney", recommended_test)) {
          effect_size_measure <- "Rank-biserial correlation"
          alternative_tests <- "t-test (if transform succeeds)"
          borderline_cases <- "Try data transformation"
        }
      }
      
      # Check for borderline normality p-values
      if (!is.na(group_normality$p_value) && group_normality$p_value >= 0.04 && group_normality$p_value <= 0.06) {
        borderline_cases <- paste(borderline_cases, "| Borderline normality p =", format.pval(group_normality$p_value, digits = 3))
      }
      
      # Check for high skewness (transformation candidate)
      if (!is.na(skew_val) && abs(skew_val) > 1) {
        alternative_tests <- paste(alternative_tests, "| Log/sqrt transformation recommended")
      }
      
      # Add row to data frame
      new_row <- data.frame(
        Variable = var,
        Group = as.character(group),
        N = n_obs,
        Missing = n_missing,
        Mean = round(mean_val, 3),
        SD = round(sd_val, 3),
        Median = round(median_val, 3),
        IQR = round(iqr_val, 3),
        CV_Percent = round(cv_val, 1),
        Skewness = round(skew_val, 3),
        Kurtosis = round(kurt_val, 3),
        Normal_Distribution = normal_status,
        Normality_Test = group_normality$test,
        Normality_P_Value = if (!is.na(group_normality$p_value)) format.pval(group_normality$p_value, digits = 3) else NA,
        Outliers_Count = group_outliers$outlier_count,
        Outliers_Percent = round(group_outliers$outlier_percentage, 1),
        Homogeneity_Status = homog_status,
        Data_Quality = data_quality,
        Recommended_Test = recommended_test,
        Post_Hoc_Needed = post_hoc_needed,
        Alternative_Tests = alternative_tests,
        Effect_Size_Measure = effect_size_measure,
        Borderline_Cases = borderline_cases,
        stringsAsFactors = FALSE
      )
      
      properties_df <- rbind(properties_df, new_row)
    }
  }
  
  # Add overall homogeneity p-values as metadata
  homogeneity_p_values <- sapply(homogeneity_results, function(x) x$p_value)
  
  return(list(
    properties_table = properties_df,
    homogeneity_tests = homogeneity_results,
    homogeneity_p_values = homogeneity_p_values
  ))
} 