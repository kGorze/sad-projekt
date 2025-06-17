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
  cat("- Master descriptive summary completed\n")
  
  # Step 2: Comprehensive assumptions testing
  if (length(numeric_vars) > 0) {
    cat("\n=== DESCRIPTIVE STATS: STEP 2 - ASSUMPTIONS TESTING ===\n")
    assumptions_results <- perform_assumptions_testing(data, numeric_vars, group_column)
    result$assumptions_analysis <- assumptions_results
    cat("- Comprehensive assumptions testing completed\n")
  }
  
  # Step 3: Generate plots if requested
  if (include_plots) {
    cat("\n=== DESCRIPTIVE STATS: STEP 3 - GENERATING VISUALIZATIONS ===\n")
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
  
  # Map to HTML report format (extracted to separate function for clarity)
  result <- map_to_html_format(result, data, master_summary, assumptions_results, group_column)
  
  cat("Descriptive statistics analysis completed successfully.\n")
  return(result)
}

# Extract HTML mapping to separate function for clarity
map_to_html_format <- function(result, data, master_summary, assumptions_results, group_column) {
  
  # Basic data summary for HTML
  result$data_summary <- create_basic_data_summary(data, group_column)
  
  # Map numeric summary data
  if (!is.null(master_summary$numeric_summary$master_table)) {
    result$summary_data <- format_numeric_summary_for_html(master_summary$numeric_summary$master_table)
  }
  
  # Map categorical summary data
  if (!is.null(master_summary$categorical_summary)) {
    result$categorical_data <- format_categorical_summary_for_html(master_summary$categorical_summary, data)
  }
  
  # Map normality analysis
  if (!is.null(assumptions_results$normality_tests)) {
    result$normality_analysis <- format_normality_for_html(assumptions_results$normality_tests)
  }
  
  # Map variable properties
  result$variable_properties <- list(
    properties_table = assumptions_results$assumptions_summary %||% data.frame(),
    homogeneity_p_values = extract_homogeneity_p_values(assumptions_results$homogeneity_tests)
  )
  
  return(result)
}

# Helper function: Create basic data summary
create_basic_data_summary <- function(data, group_column) {
  list(
    dataset_overview = data.frame(
      metric = c("Total observations", "Total variables", "Numeric variables", "Categorical variables", "Group column"),
      value = c(nrow(data), ncol(data), 
                sum(sapply(data, is.numeric)), 
                sum(sapply(data, function(x) is.factor(x) || is.character(x))), 
                group_column %||% "None"),
      stringsAsFactors = FALSE
    ),
    variable_types = data.frame(
      type = c("Numeric", "Categorical"),
      count = c(sum(sapply(data, is.numeric)), 
                sum(sapply(data, function(x) is.factor(x) || is.character(x)))),
      stringsAsFactors = FALSE
    ),
    group_summary = if (!is.null(group_column)) {
      group_table <- table(data[[group_column]])
      data.frame(
        group = names(group_table),
        n = as.numeric(group_table),
        percentage = round(as.numeric(prop.table(group_table)) * 100, 2),
        stringsAsFactors = FALSE
      )
    } else data.frame(),
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
        data.frame(variable = character(0), missing_count = numeric(0), missing_percentage = numeric(0), stringsAsFactors = FALSE)
      }
    }
  )
}

# Helper function: Format numeric summary for HTML
format_numeric_summary_for_html <- function(master_table) {
  data.frame(
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

# Helper function: Format categorical summary for HTML
format_categorical_summary_for_html <- function(categorical_summary, data) {
  result <- list()
  for (var_name in names(categorical_summary)) {
    var_data <- categorical_summary[[var_name]]
    
    # Try different summary table formats
    summary_table <- var_data$summary_table %||% var_data$overall_summary
    
    if (!is.null(summary_table)) {
      result[[var_name]] <- summary_table
    } else {
      # Fallback: create basic categorical summary
      if (var_name %in% names(data)) {
        freq_table <- table(data[[var_name]], useNA = "ifany")
        result[[var_name]] <- data.frame(
          category = names(freq_table),
          frequency = as.numeric(freq_table),
          percentage = round(as.numeric(prop.table(freq_table)) * 100, 2),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  return(result)
}

# Helper function: Format normality analysis for HTML
format_normality_for_html <- function(normality_tests) {
  result <- list()
  for (var_name in names(normality_tests)) {
    norm_data <- normality_tests[[var_name]]
    overall_test <- norm_data$overall_test
    descriptive_measures <- norm_data$descriptive_measures
    
    result[[var_name]] <- list(
      test = overall_test$test %||% "Not available",
      statistic = overall_test$statistic,
      p_value = overall_test$p_value,
      normal = overall_test$normal %||% FALSE,
      interpretation = overall_test$interpretation %||% "",
      skewness = descriptive_measures$skewness,
      kurtosis = descriptive_measures$kurtosis,
      group_normality = norm_data$group_tests
    )
  }
  return(result)
}

# Helper function: Extract homogeneity p-values
extract_homogeneity_p_values <- function(homogeneity_tests) {
  if (is.null(homogeneity_tests)) return(list())
  lapply(homogeneity_tests, function(x) x$p_value)
}

# Null coalescing operator helper
`%||%` <- function(x, y) if (is.null(x)) y else x

# Simplified plotting function
create_descriptive_plots <- function(data, numeric_vars, categorical_vars, group_column = NULL, output_path = "output/plots/") {
  
  # Load required packages
  suppressPackageStartupMessages({
    library(ggplot2)
    library(ggpubr) 
    library(GGally)
    library(dplyr)
    library(scales)
  })
  
  # Create output directory
  if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
  
  plots <- list()
  plot_files <- list()
  
  cat("Creating simplified descriptive visualizations...\n")
  
  # 1. Histograms for numeric variables (simplified)
  for (var in numeric_vars[1:min(6, length(numeric_vars))]) {
    p <- create_histogram_plot(data, var, group_column)
    if (!is.null(p)) {
      filename <- save_plot(p, output_path, paste0("histogram_", var))
      plots[[paste0("histogram_", var)]] <- p
      plot_files[[paste0("histogram_", var)]] <- filename
      cat("Created histogram for", var, "\n")
    }
  }
  
  # 2. Bar plots for categorical variables (simplified)
  for (var in categorical_vars[1:min(4, length(categorical_vars))]) {
    if (length(unique(data[[var]])) <= 20) {
      p <- create_barplot(data, var, group_column)
      if (!is.null(p)) {
        filename <- save_plot(p, output_path, paste0("barplot_", var))
        plots[[paste0("barplot_", var)]] <- p
        plot_files[[paste0("barplot_", var)]] <- filename
        cat("Created barplot for", var, "\n")
      }
    }
  }
  
  # 3. Pairs plot (if applicable)
  if (length(numeric_vars) >= 3 && length(numeric_vars) <= 6) {
    p <- create_pairs_plot(data, numeric_vars, group_column)
    if (!is.null(p)) {
      filename <- save_plot(p, output_path, "pairs_plot", width = 12, height = 12)
      plots[["pairs_plot"]] <- p
      plot_files[["pairs_plot"]] <- filename
      cat("Created pairs plot\n")
    }
  }
  
  # 4. Missing data visualization
  p_missing <- create_missing_data_plot(data)
  if (!is.null(p_missing)) {
    filename <- save_plot(p_missing, output_path, "missing_data_analysis")
    plots[["missing_data_analysis"]] <- p_missing
    plot_files[["missing_data_analysis"]] <- filename
    cat("Created missing data analysis plot\n")
  }
  
  # 5. Outlier visualization
  p_outlier <- create_outlier_plot(data, numeric_vars)
  if (!is.null(p_outlier)) {
    filename <- save_plot(p_outlier, output_path, "outlier_analysis")
    plots[["outlier_analysis"]] <- p_outlier
    plot_files[["outlier_analysis"]] <- filename
    cat("Created outlier analysis plot\n")
  }
  
  cat("Simplified descriptive visualization creation completed!\n")
  cat("Total plots created:", length(plots), "\n")
  
  return(list(plots = plots, plot_files = plot_files))
}

# Simplified histogram creation
create_histogram_plot <- function(data, var, group_column = NULL) {
  tryCatch({
    if (is.null(group_column)) {
      ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "steelblue", alpha = 0.7, color = "white") +
        geom_density(color = "red", linewidth = 1) +
        labs(title = paste("Distribution of", var), x = var, y = "Density") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    } else {
      ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
        geom_histogram(aes(y = after_stat(density)), bins = 20, alpha = 0.7, position = "identity") +
        geom_density(aes(color = .data[[group_column]]), alpha = 0.8, linewidth = 1) +
        facet_wrap(as.formula(paste("~", group_column))) +
        labs(title = paste("Distribution of", var, "by", group_column), x = var, y = "Density") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        scale_color_brewer(type = "qual", palette = "Set2")
    }
  }, error = function(e) {
    cat("Error creating histogram for", var, ":", e$message, "\n")
    return(NULL)
  })
}

# Simplified barplot creation
create_barplot <- function(data, var, group_column = NULL) {
  tryCatch({
    if (is.null(group_column)) {
      freq_table <- table(data[[var]])
      freq_data <- data.frame(
        category = names(freq_table),
        count = as.numeric(freq_table),
        stringsAsFactors = FALSE
      )
      names(freq_data)[1] <- var
      
      ggplot(freq_data, aes(x = .data[[var]], y = count)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        labs(title = paste("Distribution of", var), x = var, y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
        geom_bar(position = "dodge", alpha = 0.8) +
        labs(title = paste("Distribution of", var, "by", group_column), 
             x = var, y = "Count", fill = group_column) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(type = "qual", palette = "Set2")
    }
  }, error = function(e) {
    cat("Error creating barplot for", var, ":", e$message, "\n")
    return(NULL)
  })
}

# Simplified pairs plot
create_pairs_plot <- function(data, numeric_vars, group_column = NULL) {
  tryCatch({
    pairs_data <- data[, c(numeric_vars, if(!is.null(group_column)) group_column), drop = FALSE]
    
    if (!is.null(group_column)) {
      ggpairs(pairs_data, columns = 1:length(numeric_vars),
              aes(color = .data[[group_column]], alpha = 0.7),
              title = "Pairs Plot of Numeric Variables by Group") +
        theme_minimal()
    } else {
      ggpairs(pairs_data, columns = 1:length(numeric_vars),
              title = "Pairs Plot of Numeric Variables") +
        theme_minimal()
    }
  }, error = function(e) {
    cat("Error creating pairs plot:", e$message, "\n")
    return(NULL)
  })
}

# Missing data plot
create_missing_data_plot <- function(data) {
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_vars <- missing_counts[missing_counts > 0]
  
  if (length(missing_vars) == 0) return(NULL)
  
  missing_data <- data.frame(
    variable = names(missing_vars),
    missing_count = as.numeric(missing_vars),
    missing_percentage = round(as.numeric(missing_vars) / nrow(data) * 100, 2),
    stringsAsFactors = FALSE
  )
  
  ggplot(missing_data, aes(x = reorder(variable, missing_percentage), y = missing_percentage)) +
    geom_col(fill = "coral", alpha = 0.8) +
    coord_flip() +
    labs(title = "Missing Data Analysis", x = "Variable", y = "Missing Percentage (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# Outlier analysis plot
create_outlier_plot <- function(data, numeric_vars) {
  if (length(numeric_vars) == 0) return(NULL)
  
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
  
  if (nrow(outlier_summary) == 0) return(NULL)
  
  ggplot(outlier_summary, aes(x = reorder(variable, outlier_percentage), y = outlier_percentage)) +
    geom_col(fill = "orange", alpha = 0.8) +
    coord_flip() +
    labs(title = "Outlier Analysis Summary", x = "Variable", y = "Outlier Percentage (%)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# Helper function to save plots
save_plot <- function(plot, output_path, name, width = 10, height = 8) {
  filename <- file.path(output_path, paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
  ggsave(filename, plot = plot, width = width, height = height, dpi = 300)
  return(filename)
}

