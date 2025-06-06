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

# Create descriptive plots
create_descriptive_plots <- function(data, numeric_vars, categorical_vars, group_column = NULL, output_path = "output/plots/") {
  
  # Create plots directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  plots <- list()
  plot_files <- list()
  
  # Histograms for numeric variables
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(6, length(numeric_vars))]) {  # Limit to first 6 variables
      tryCatch({
        p <- ggplot(data, aes(x = .data[[var]])) +
          geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
          labs(title = paste("Distribution of", var),
               x = var, y = "Frequency") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
        
        if (!is.null(group_column)) {
          p <- p + facet_wrap(as.formula(paste("~", group_column)))
        }
        
        # Save plot to file
        plot_filename <- file.path(output_path, paste0("histogram_", var, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 10, height = 6, dpi = 300)
        
        plots[[paste0("histogram_", var)]] <- p
        plot_files[[paste0("histogram_", var)]] <- plot_filename
        cat("Created and saved histogram for", var, "to", plot_filename, "\n")
      }, error = function(e) {
        cat("Error creating histogram for", var, ":", e$message, "\n")
      })
    }
  }
  
  # Box plots for numeric variables by group
  if (!is.null(group_column) && length(numeric_vars) > 0) {
    for (var in numeric_vars[1:min(4, length(numeric_vars))]) {  # Limit to first 4 variables
      tryCatch({
        p <- ggplot(data, aes(x = .data[[group_column]], y = .data[[var]], fill = .data[[group_column]])) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Distribution of", var, "by", group_column),
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
  }
  
  # Bar plots for categorical variables
  if (length(categorical_vars) > 0) {
    for (var in categorical_vars[1:min(4, length(categorical_vars))]) {  # Limit to first 4 variables
      
      # Skip if too many categories
      if (length(unique(data[[var]])) > 20) next
      
      tryCatch({
        p <- ggplot(data, aes(x = .data[[var]])) +
          geom_bar(fill = "steelblue", alpha = 0.7) +
          labs(title = paste("Frequency of", var),
               x = var, y = "Count") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1))
        
        if (!is.null(group_column)) {
          p <- ggplot(data, aes(x = .data[[var]], fill = .data[[group_column]])) +
            geom_bar(position = "dodge", alpha = 0.7) +
            labs(title = paste("Frequency of", var, "by", group_column),
                 x = var, y = "Count", fill = group_column) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_brewer(type = "qual", palette = "Set2")
        }
        
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
  
  # Overall summary plot - missing data pattern
  if (any(sapply(data, function(x) any(is.na(x))))) {
    tryCatch({
      missing_data <- data.frame(
        variable = names(data),
        missing_percentage = sapply(data, function(x) sum(is.na(x)) / length(x) * 100),
        stringsAsFactors = FALSE
      )
      missing_data <- missing_data[missing_data$missing_percentage > 0, ]
      
      if (nrow(missing_data) > 0) {
        p <- ggplot(missing_data, aes(x = reorder(variable, missing_percentage), y = missing_percentage)) +
          geom_col(fill = "coral", alpha = 0.7) +
          coord_flip() +
          labs(title = "Missing Data by Variable",
               x = "Variable", y = "Missing Percentage (%)") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5))
        
        # Save plot to file
        plot_filename <- file.path(output_path, paste0("missing_data_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))
        ggsave(plot_filename, plot = p, width = 8, height = 6, dpi = 300)
        
        plots[["missing_data_summary"]] <- p
        plot_files[["missing_data_summary"]] <- plot_filename
        cat("Created and saved missing data summary plot to", plot_filename, "\n")
      }
    }, error = function(e) {
      cat("Error creating missing data plot:", e$message, "\n")
    })
  }
  
  cat("Total descriptive plots created:", length(plots), "\n")
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