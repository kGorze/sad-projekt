# Descriptive Statistics Module
# Functions for generating descriptive statistics and group characteristics
# Supports independent groups analysis for medical data

# Load required libraries with error handling
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(ggplot2, quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Additional libraries for advanced analysis
if (!require(ggpubr, quietly = TRUE)) {
  install.packages("ggpubr", repos = "https://cran.r-project.org")
  library(ggpubr)
}

if (!require(gridExtra, quietly = TRUE)) {
  install.packages("gridExtra", repos = "https://cran.r-project.org")
  library(gridExtra)
}

if (!require(corrplot, quietly = TRUE)) {
  install.packages("corrplot", repos = "https://cran.r-project.org")
  library(corrplot)
}

if (!require(GGally, quietly = TRUE)) {
  install.packages("GGally", repos = "https://cran.r-project.org")
  library(GGally)
}

if (!require(VIM, quietly = TRUE)) {
  install.packages("VIM", repos = "https://cran.r-project.org")
  library(VIM)
}

if (!require(psych, quietly = TRUE)) {
  install.packages("psych", repos = "https://cran.r-project.org")
  library(psych)
}

# Source reporting utilities
source("modules/reporting/export_results.R")

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
  
  # Calculate statistics for continuous variables
  if (length(numeric_vars) > 0) {
    continuous_stats <- calculate_continuous_stats(data, numeric_vars, group_column)
    result$summary_data <- continuous_stats
    
    cat("- Continuous variables analyzed:", length(numeric_vars), "\n")
  }
  
  # Calculate statistics for categorical variables
  if (length(categorical_vars) > 0) {
    categorical_stats <- calculate_categorical_stats(data, categorical_vars, group_column)
    result$categorical_data <- categorical_stats
    
    cat("- Categorical variables analyzed:", length(categorical_vars), "\n")
  }
  
  # Create summary table
  summary_table <- create_summary_table(data, group_column)
  result$data_summary <- summary_table
  
  # Step 2: Correlation analysis for numeric variables
  if (length(numeric_vars) >= 2) {
    cat("\n=== STEP 2: CORRELATION ANALYSIS ===\n")
    correlation_results <- perform_correlation_analysis(data, numeric_vars, group_column)
    result$correlation_analysis <- correlation_results
    cat("- Correlation matrix calculated for", length(numeric_vars), "variables\n")
  }
  
  # Step 3: Normality testing for numeric variables
  if (length(numeric_vars) > 0) {
    cat("\n=== STEP 3: NORMALITY TESTING ===\n")
    normality_results <- perform_normality_tests(data, numeric_vars, group_column)
    result$normality_analysis <- normality_results
    cat("- Normality tests performed for", length(numeric_vars), "variables\n")
  }
  
  # Step 3.5: Outlier detection and analysis
  if (length(numeric_vars) > 0) {
    cat("\n=== STEP 3.5: OUTLIER ANALYSIS ===\n")
    outlier_results <- perform_outlier_analysis(data, numeric_vars, group_column)
    result$outlier_analysis <- outlier_results
    cat("- Outlier analysis completed for", length(numeric_vars), "variables\n")
  }
  
  # Step 3.6: Create comprehensive variable properties table
  if (length(numeric_vars) > 0 && !is.null(group_column)) {
    cat("\n=== STEP 3.6: VARIABLE PROPERTIES ANALYSIS ===\n")
    variable_properties <- create_variable_properties_table(data, numeric_vars, group_column, 
                                                           normality_results, outlier_results)
    result$variable_properties <- variable_properties
    cat("- Variable properties table created for", length(numeric_vars), "variables across groups\n")
  }
  
  # Step 4: Generate plots if requested
  if (include_plots) {
    cat("\n=== STEP 4: GENERATING VISUALIZATIONS ===\n")
    
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
  
  cat("Descriptive statistics analysis completed successfully.\n")
  return(result)
}

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
      var_stats <- var_stats %>%
        group_by(group) %>%
        mutate(percentage = round(frequency / sum(frequency) * 100, 2)) %>%
        ungroup() %>%
        select(variable, category, group, frequency, percentage)
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
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  cat("Creating advanced descriptive visualizations...\n")
  
  # 1. Enhanced histograms with density overlays and normality curves
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(6, length(numeric_vars))]) {
      tryCatch({
        # Calculate normal distribution parameters
        var_data <- data[[var]][!is.na(data[[var]])]
        mean_val <- mean(var_data)
        sd_val <- sd(var_data)
        
        p <- ggplot(data, aes(x = .data[[var]])) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
          geom_density(color = "red", size = 1.2, alpha = 0.8) +
          stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), 
                       color = "blue", linetype = "dashed", size = 1) +
          labs(title = paste("Distribution Analysis of", var),
               subtitle = paste("Red: Actual density, Blue dashed: Normal distribution"),
               x = var, y = "Density") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10))
        
        if (!is.null(group_column)) {
          p <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
            geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7, position = "identity") +
            geom_density(aes(color = .data[[group_column]]), alpha = 0.8, size = 1.2) +
            facet_wrap(as.formula(paste("~", group_column))) +
            labs(title = paste("Distribution Analysis of", var, "by", group_column),
                 subtitle = "Histograms with density overlays by group",
                 x = var, y = "Density") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10)) +
            scale_fill_brewer(type = "qual", palette = "Set2") +
            scale_color_brewer(type = "qual", palette = "Set2")
        }
        
        plot_filename <- file.path(output_path, paste0("enhanced_histogram_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 12, height = 8, dpi = 300)
        
        plots[[paste0("enhanced_histogram_", var)]] <- p
        plot_files[[paste0("enhanced_histogram_", var)]] <- plot_filename
        cat("Created enhanced histogram for", var, "\n")
        
      }, error = function(e) {
        cat("Error creating enhanced histogram for", var, ":", e$message, "\n")
      })
    }
  }
  
  # 2. Q-Q plots for normality assessment
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(4, length(numeric_vars))]) {
      tryCatch({
        if (!is.null(group_column)) {
          p <- ggplot(data, aes(sample = .data[[var]])) +
            stat_qq(aes(color = .data[[group_column]]), alpha = 0.7) +
            stat_qq_line(aes(color = .data[[group_column]])) +
            facet_wrap(as.formula(paste("~", group_column))) +
            labs(title = paste("Q-Q Plot for", var, "by", group_column),
                 subtitle = "Assessment of normality assumption by group",
                 x = "Theoretical Quantiles", y = "Sample Quantiles") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10)) +
            scale_color_brewer(type = "qual", palette = "Set2")
        } else {
          p <- ggplot(data, aes(sample = .data[[var]])) +
            stat_qq(alpha = 0.7, color = "steelblue") +
            stat_qq_line(color = "red", size = 1) +
            labs(title = paste("Q-Q Plot for", var),
                 subtitle = "Assessment of normality assumption",
                 x = "Theoretical Quantiles", y = "Sample Quantiles") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 10))
        }
        
        plot_filename <- file.path(output_path, paste0("qq_plot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[[paste0("qq_plot_", var)]] <- p
        plot_files[[paste0("qq_plot_", var)]] <- plot_filename
        cat("Created Q-Q plot for", var, "\n")
        
      }, error = function(e) {
        cat("Error creating Q-Q plot for", var, ":", e$message, "\n")
      })
    }
  }
  
  # 3. Enhanced box plots with statistical annotations
  if (!is.null(group_column) && length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(4, length(numeric_vars))]) {
      tryCatch({
        p <- ggboxplot(data, x = group_column, y = var, 
                       color = group_column, palette = "jco",
                       add = "jitter", add.params = list(alpha = 0.3)) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          labs(title = paste("Enhanced Box Plot of", var, "by", group_column),
               subtitle = "With jitter points and means (red diamonds)",
               x = group_column, y = var) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size = 10),
                legend.position = "none")
        
        plot_filename <- file.path(output_path, paste0("enhanced_boxplot_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 8, dpi = 300)
        
        plots[[paste0("enhanced_boxplot_", var)]] <- p
        plot_files[[paste0("enhanced_boxplot_", var)]] <- plot_filename
        cat("Created enhanced boxplot for", var, "\n")
        
      }, error = function(e) {
        cat("Error creating enhanced boxplot for", var, ":", e$message, "\n")
      })
    }
  }
  
  # 4. Violin plots for distribution shape analysis
  if (!is.null(group_column) && length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(3, length(numeric_vars))]) {
      tryCatch({
        p <- ggplot(data, aes(x = .data[[group_column]], y = .data[[var]], fill = .data[[group_column]])) +
          geom_violin(alpha = 0.7, trim = FALSE) +
          geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "blue") +
          labs(title = paste("Violin Plot of", var, "by", group_column),
               subtitle = "Shows distribution shape, quartiles, means (red) and medians (blue)",
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
  }
  
  # 5. Correlation matrix heatmap
  if (length(numeric_vars) >= 3) {
    tryCatch({
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
  
  # 7. Enhanced bar plots for categorical variables
  if (length(categorical_vars) > 0) {
    for (var in categorical_vars[1:min(4, length(categorical_vars))]) {
      
      if (length(unique(data[[var]])) > 20) next
      
      tryCatch({
        if (!is.null(group_column)) {
          # Calculate proportions
          prop_data <- data %>%
            group_by(.data[[group_column]], .data[[var]]) %>%
            summarise(count = n(), .groups = 'drop') %>%
            group_by(.data[[group_column]]) %>%
            mutate(prop = count / sum(count) * 100)
          
          p <- ggplot(prop_data, aes(x = .data[[var]], y = prop, fill = .data[[group_column]])) +
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
          # Simple frequency plot
          freq_data <- data %>%
            count(.data[[var]]) %>%
            mutate(prop = n / sum(n) * 100)
          
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

# Helper function to calculate skewness
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3) / ((n - 1) * sd_x^3)
  return(skew)
}

# Helper function to calculate kurtosis
calculate_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA)
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  kurt <- sum((x - mean_x)^4) / ((n - 1) * sd_x^4) - 3
  return(kurt)
}

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

# Perform correlation analysis for numeric variables
perform_correlation_analysis <- function(data, variables, group_column = NULL) {
  
  if (length(variables) < 2) {
    return(NULL)
  }
  
  # Select only numeric variables for correlation
  cor_data <- data[, variables, drop = FALSE]
  
  # Overall correlation matrix
  cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
  cor_spearman <- cor(cor_data, use = "complete.obs", method = "spearman")
  
  # Correlation significance tests
  cor_test_results <- list()
  for (i in 1:(length(variables)-1)) {
    for (j in (i+1):length(variables)) {
      var1 <- variables[i]
      var2 <- variables[j]
      
      # Pearson correlation test
      pearson_test <- cor.test(data[[var1]], data[[var2]], method = "pearson")
      
      # Spearman correlation test
      spearman_test <- cor.test(data[[var1]], data[[var2]], method = "spearman")
      
      cor_test_results[[paste(var1, "vs", var2)]] <- list(
        variables = c(var1, var2),
        pearson_r = pearson_test$estimate,
        pearson_p = pearson_test$p.value,
        pearson_ci = pearson_test$conf.int,
        spearman_rho = spearman_test$estimate,
        spearman_p = spearman_test$p.value,
        interpretation = interpret_correlation(pearson_test$estimate)
      )
    }
  }
  
  # Group-wise correlations if group column specified
  group_correlations <- NULL
  if (!is.null(group_column)) {
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    
    group_correlations <- list()
    for (group in groups) {
      group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variables, drop = FALSE]
      if (nrow(group_data) > 3) {  # Need at least 4 observations for correlation
        group_correlations[[as.character(group)]] <- cor(group_data, use = "complete.obs", method = "pearson")
      }
    }
  }
  
  return(list(
    correlation_matrix = cor_matrix,
    spearman_matrix = cor_spearman,
    correlation_tests = cor_test_results,
    group_correlations = group_correlations
  ))
}

# Interpret correlation strength
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("negligible")
  else if (abs_r < 0.3) return("weak")
  else if (abs_r < 0.5) return("moderate")
  else if (abs_r < 0.7) return("strong")
  else return("very strong")
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
                                  round(test_result$p.value, 4), ")"),
                            paste("Data deviates from normal distribution (", test_name, " p =", 
                                  round(test_result$p.value, 4), ")"))
    
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
        if (!require(car, quietly = TRUE)) {
          install.packages("car")
          library(car)
        }
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
      
      # Homogeneity status
      homog_info <- homogeneity_results[[var]]
      homog_status <- if(is.na(homog_info$homogeneous)) {
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
      if(!is.na(group_normality$normal) && !is.na(homog_info$homogeneous)) {
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
        borderline_cases <- paste(borderline_cases, "| Borderline normality p =", round(group_normality$p_value, 4))
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
        Normality_P_Value = round(group_normality$p_value, 4),
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