# Master descriptive summary
# Unified generation of comprehensive group-wise descriptive statistics
# Eliminates duplication across descriptive stats, comparative analysis, and correlation modules

# Package dependencies

# Source statistical helper functions
source("modules/utils/statistical_helpers.R")

# Main function: Generate master descriptive summary
generate_master_descriptive_summary <- function(data, group_column = NULL, variables = NULL) {
  
  cat("=== MASTER DESCRIPTIVE SUMMARY ===\n")
  cat("Generating comprehensive descriptive statistics...\n")
  
  # Auto-detect variables if not specified
  if (is.null(variables)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    # Remove group column if it exists
    if (!is.null(group_column)) {
      numeric_vars <- numeric_vars[numeric_vars != group_column]
      categorical_vars <- categorical_vars[categorical_vars != group_column]
    }
  } else {
    numeric_vars <- variables[sapply(data[variables], is.numeric)]
    categorical_vars <- variables[sapply(data[variables], function(x) is.factor(x) || is.character(x))]
  }
  
  cat("- Analyzing", length(numeric_vars), "numeric variables\n")
  cat("- Analyzing", length(categorical_vars), "categorical variables\n")
  
  if (!is.null(group_column)) {
    groups <- unique(data[[group_column]])
    groups <- groups[!is.na(groups)]
    cat("- Across", length(groups), "groups:", paste(groups, collapse = ", "), "\n")
  }
  
  # Initialize master summary structure
  master_summary <- list(
    numeric_summary = NULL,
    categorical_summary = NULL,
    group_comparisons = NULL,
    overall_summary = NULL,
    data_quality = NULL,
    metadata = list(
      total_observations = nrow(data),
      numeric_variables = length(numeric_vars),
      categorical_variables = length(categorical_vars),
      group_column = group_column,
      groups = if (!is.null(group_column)) groups else NULL,
      analysis_date = Sys.time()
    )
  )
  
  # Sub-step 1.1: Comprehensive numeric variable summary
  if (length(numeric_vars) > 0) {
    cat("\n--- Step 1.1: Numeric Variables Summary ---\n")
    master_summary$numeric_summary <- generate_comprehensive_numeric_summary(data, numeric_vars, group_column)
  }
  
  # Sub-step 1.2: Comprehensive categorical variable summary
  if (length(categorical_vars) > 0) {
    cat("\n--- Step 1.2: Categorical Variables Summary ---\n")
    master_summary$categorical_summary <- generate_comprehensive_categorical_summary(data, categorical_vars, group_column)
  }
  
  # Sub-step 1.3: Generate group comparison summary (if groups exist)
  if (!is.null(group_column) && length(numeric_vars) > 0) {
    cat("\n--- Step 1.3: Group Comparisons Summary ---\n")
    master_summary$group_comparisons <- generate_group_comparison_summary(data, numeric_vars, group_column)
  }
  
  # Sub-step 1.4: Overall dataset summary
  cat("\n--- Step 1.4: Overall Dataset Summary ---\n")
  master_summary$overall_summary <- generate_overall_dataset_summary(data, group_column)
  
  # Sub-step 1.5: Data quality assessment
  cat("\n--- Step 1.5: Data Quality Assessment ---\n")
  master_summary$data_quality <- assess_simple_data_quality(data, numeric_vars, categorical_vars, group_column)
  
  cat("Master descriptive summary completed.\n")
  return(master_summary)
}

# Comprehensive numeric variables summary with enhanced statistics
generate_comprehensive_numeric_summary <- function(data, numeric_vars, group_column = NULL) {
  
  if (length(numeric_vars) == 0) {
    return(NULL)
  }
  
  summary_list <- list()
  
  for (var in numeric_vars) {
    cat("Processing numeric variable:", var, "\n")
    
    # Overall statistics
    var_data <- data[[var]][!is.na(data[[var]])]
    n_total <- length(var_data)
    n_missing <- sum(is.na(data[[var]]))
    
    if (n_total == 0) {
      summary_list[[var]] <- list(
        variable = var,
        error = "No valid data points"
      )
      next
    }
    
    # Comprehensive statistics (basic + distribution)
    comprehensive_stats <- calculate_comprehensive_statistics(var_data)
    
    # Outlier detection
    outlier_info <- detect_outliers_simple(var_data)
    
    # Group-wise statistics (if group column specified)
    group_stats <- NULL
    if (!is.null(group_column)) {
      group_stats <- calculate_group_wise_statistics(data, var, group_column)
    }
    
    # Combine all information
    summary_list[[var]] <- list(
      variable = var,
      n_total = n_total,
      n_missing = n_missing,
      missing_percentage = round((n_missing / nrow(data)) * 100, 2),
      comprehensive_statistics = comprehensive_stats,
      outlier_information = outlier_info,
      group_statistics = group_stats
    )
  }
  
  # Create master numeric summary table
  master_table <- create_master_numeric_table(summary_list)
  
  return(list(
    detailed_summaries = summary_list,
    master_table = master_table
  ))
}

# Simplified comprehensive statistics (combines basic + distribution characteristics)
calculate_comprehensive_statistics <- function(data_vector) {
  
  if (length(data_vector) == 0) {
    return(NULL)
  }
  
  # Basic descriptive statistics
  stats <- list(
    n = length(data_vector),
    mean = mean(data_vector, na.rm = TRUE),
    median = median(data_vector, na.rm = TRUE),
    sd = sd(data_vector, na.rm = TRUE),
    min = min(data_vector, na.rm = TRUE),
    max = max(data_vector, na.rm = TRUE),
    q25 = quantile(data_vector, 0.25, na.rm = TRUE),
    q75 = quantile(data_vector, 0.75, na.rm = TRUE),
    iqr = IQR(data_vector, na.rm = TRUE),
    cv = ifelse(mean(data_vector, na.rm = TRUE) != 0, 
                sd(data_vector, na.rm = TRUE) / mean(data_vector, na.rm = TRUE) * 100, 
                NA)
  )
  
  # Distribution characteristics (only if sufficient data)
  if (length(data_vector) >= 3) {
    stats$skewness <- calculate_skewness(data_vector)
    stats$kurtosis <- calculate_kurtosis(data_vector)
  } else {
    stats$skewness <- NA
    stats$kurtosis <- NA
  }
  
  # Round numeric values
  stats <- lapply(stats, function(x) if (is.numeric(x)) round(x, 4) else x)
  
  return(stats)
}

# Simplified outlier detection (IQR method only)
detect_outliers_simple <- function(data_vector) {
  
  if (length(data_vector) < 4) {
    return(list(outlier_count = 0, outlier_percentage = 0))
  }
  
  # Use IQR method (most commonly used and reliable)
  Q1 <- quantile(data_vector, 0.25)
  Q3 <- quantile(data_vector, 0.75)
  IQR_val <- Q3 - Q1
  outliers <- sum(data_vector < (Q1 - 1.5 * IQR_val) | data_vector > (Q3 + 1.5 * IQR_val))
  
  return(list(
    outlier_count = outliers,
    outlier_percentage = round((outliers / length(data_vector)) * 100, 2)
  ))
}

# Calculate group-wise statistics
calculate_group_wise_statistics <- function(data, variable, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  group_summaries <- list()
  
  for (group in groups) {
    group_data <- data[data[[group_column]] == group & !is.na(data[[group_column]]), variable]
    group_data <- group_data[!is.na(group_data)]
    
    if (length(group_data) > 0) {
      comprehensive_stats <- calculate_comprehensive_statistics(group_data)
      outlier_info <- detect_outliers_simple(group_data)
      
      group_summaries[[as.character(group)]] <- list(
        group = group,
        n = length(group_data),
        comprehensive_statistics = comprehensive_stats,
        outlier_information = outlier_info
      )
    }
  }
  
  return(group_summaries)
}

# Create master numeric summary table
create_master_numeric_table <- function(summary_list) {
  
  if (length(summary_list) == 0) {
    return(NULL)
  }
  
  # Extract key statistics for tabular format
  table_rows <- list()
  
  for (var_name in names(summary_list)) {
    var_summary <- summary_list[[var_name]]
    
    if (is.null(var_summary$comprehensive_statistics)) {
      next
    }
    
    stats <- var_summary$comprehensive_statistics
    outliers <- var_summary$outlier_information
    
    table_rows[[var_name]] <- data.frame(
      Variable = var_name,
      N = stats$n,
      Missing = var_summary$n_missing,
      Missing_Pct = var_summary$missing_percentage,
      Mean = stats$mean,
      SD = stats$sd,
      Median = stats$median,
      Q25 = stats$q25,
      Q75 = stats$q75,
      Min = stats$min,
      Max = stats$max,
      CV_Pct = stats$cv,
      Skewness = stats$skewness,
      Kurtosis = stats$kurtosis,
      Outliers_IQR = outliers$outlier_count,
      Outliers_Pct = outliers$outlier_percentage,
      stringsAsFactors = FALSE
    )
  }
  
  master_table <- do.call(rbind, table_rows)
  return(master_table)
}

# Generate explanations for continuous variables statistics table
generate_continuous_stats_explanations <- function() {
  
  explanations <- c(
    "• N: Number of valid (non-missing) observations for each variable",
    "• Missing: Count of missing values (NA) that were excluded from calculations", 
    "• Mean: Arithmetic average calculated as sum of all values divided by N",
    "• SD: Standard deviation measuring variability around the mean (square root of variance)",
    "• Median: Middle value when data is arranged in ascending order (50th percentile)",
    "• Q25: First quartile, 25th percentile (25% of values fall below this point)",
    "• Q75: Third quartile, 75th percentile (75% of values fall below this point)", 
    "• Min: Smallest observed value in the dataset for each variable",
    "• Max: Largest observed value in the dataset for each variable",
    "• Range: Difference between maximum and minimum values (Max - Min)",
    "• IQR: Interquartile range, difference between Q75 and Q25 (middle 50% spread)",
    "• CV%: Coefficient of variation as percentage (SD/Mean × 100), relative variability measure",
    "• Skewness: Measure of asymmetry; positive = right tail, negative = left tail, 0 = symmetric",
    "• Kurtosis: Measure of tail heaviness; positive = heavy tails, negative = light tails, 0 = normal"
  )
  
  return(explanations)
}

# Comprehensive categorical variables summary
generate_comprehensive_categorical_summary <- function(data, categorical_vars, group_column = NULL) {
  
  if (length(categorical_vars) == 0) {
    return(NULL)
  }
  
  categorical_summaries <- list()
  
  for (var in categorical_vars) {
    cat("Processing categorical variable:", var, "\n")
    
    # Overall frequency analysis
    var_data <- data[[var]]
    n_total <- length(var_data[!is.na(var_data)])
    n_missing <- sum(is.na(var_data))
    
    # Frequency table
    freq_table <- table(var_data, useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    # Diversity measures
    n_categories <- length(unique(var_data[!is.na(var_data)]))
    mode_category <- names(freq_table)[which.max(freq_table)]
    mode_frequency <- max(freq_table)
    mode_percentage <- max(prop_table)
    
    # Group-wise analysis (if group column specified)
    group_analysis <- NULL
    if (!is.null(group_column)) {
      group_analysis <- analyze_categorical_by_group(data, var, group_column)
    }
    
    categorical_summaries[[var]] <- list(
      variable = var,
      n_total = n_total,
      n_missing = n_missing,
      missing_percentage = round((n_missing / nrow(data)) * 100, 2),
      n_categories = n_categories,
      mode_category = mode_category,
      mode_frequency = mode_frequency,
      mode_percentage = round(mode_percentage, 2),
      frequency_table = data.frame(
        Category = names(freq_table),
        Frequency = as.numeric(freq_table),
        Percentage = round(as.numeric(prop_table), 2),
        stringsAsFactors = FALSE
      ),
      group_analysis = group_analysis
    )
  }
  
  return(categorical_summaries)
}

# Analyze categorical variables by group
analyze_categorical_by_group <- function(data, variable, group_column) {
  
  # Create contingency table
  contingency_table <- table(data[[variable]], data[[group_column]], useNA = "ifany")
  
  # Chi-square test if appropriate
  chi_square_result <- NULL
  if (all(contingency_table >= 5)) {
    tryCatch({
      chi_test <- chisq.test(contingency_table)
      chi_square_result <- list(
        statistic = chi_test$statistic,
        p_value = chi_test$p.value,
        significant = chi_test$p.value < 0.05,
        interpretation = ifelse(chi_test$p.value < 0.05,
                               "Significant association between variables",
                               "No significant association between variables")
      )
    }, error = function(e) {
      chi_square_result <- list(error = "Chi-square test failed")
    })
  }
  
  # Group-wise proportions
  group_proportions <- prop.table(contingency_table, 2) * 100
  
  return(list(
    contingency_table = contingency_table,
    group_proportions = group_proportions,
    chi_square_test = chi_square_result
  ))
}

# Generate group comparison summary for numeric variables
generate_group_comparison_summary <- function(data, numeric_vars, group_column) {
  
  groups <- unique(data[[group_column]])
  groups <- groups[!is.na(groups)]
  
  # Group comparison table
  comparison_table <- data.frame(
    Group = groups,
    N = sapply(groups, function(g) sum(data[[group_column]] == g, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  
  # Add summary statistics for each numeric variable
  for (var in numeric_vars) {
    var_means <- sapply(groups, function(g) {
      group_data <- data[data[[group_column]] == g & !is.na(data[[group_column]]), var]
      mean(group_data, na.rm = TRUE)
    })
    
    var_sds <- sapply(groups, function(g) {
      group_data <- data[data[[group_column]] == g & !is.na(data[[group_column]]), var]
      sd(group_data, na.rm = TRUE)
    })
    
    comparison_table[[paste0(var, "_Mean")]] <- round(var_means, 3)
    comparison_table[[paste0(var, "_SD")]] <- round(var_sds, 3)
  }
  
  return(comparison_table)
}

# Generate overall dataset summary
generate_overall_dataset_summary <- function(data, group_column = NULL) {
  
  overall_summary <- list(
    total_observations = nrow(data),
    total_variables = ncol(data),
    numeric_variables = sum(sapply(data, is.numeric)),
    categorical_variables = sum(sapply(data, function(x) is.factor(x) || is.character(x))),
    total_missing_values = sum(is.na(data)),
    missing_percentage = round((sum(is.na(data)) / (nrow(data) * ncol(data))) * 100, 2),
    complete_cases = sum(complete.cases(data)),
    complete_cases_percentage = round((sum(complete.cases(data)) / nrow(data)) * 100, 2)
  )
  
  if (!is.null(group_column)) {
    group_info <- table(data[[group_column]], useNA = "ifany")
    overall_summary$group_information <- list(
      group_column = group_column,
      n_groups = length(unique(data[[group_column]][!is.na(data[[group_column]])])),
      group_sizes = as.list(group_info),
      balanced_design = max(group_info) / min(group_info) <= 2  # Rule of thumb for balance
    )
  }
  
  return(overall_summary)
}

# Simplified data quality assessment
assess_simple_data_quality <- function(data, numeric_vars, categorical_vars, group_column) {
  
  quality_issues <- c()
  
  # Check missing data (>5% threshold)
  missing_by_var <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
  high_missing <- names(missing_by_var)[missing_by_var > 5]
  if (length(high_missing) > 0) {
    quality_issues <- c(quality_issues, paste("High missing data in:", paste(high_missing, collapse = ", ")))
  }
  
  # Check sample size
  if (nrow(data) < 30) {
    quality_issues <- c(quality_issues, "Small sample size (n < 30)")
  }
  
  # Check group balance (if applicable)
  if (!is.null(group_column)) {
    group_sizes <- table(data[[group_column]])
    if (min(group_sizes) < 10) {
      quality_issues <- c(quality_issues, "Some groups have < 10 observations")
    }
    if (max(group_sizes) / min(group_sizes) > 3) {
      quality_issues <- c(quality_issues, "Groups are unbalanced (ratio > 3:1)")
    }
  }
  
  # Simple quality rating
  quality_rating <- if (length(quality_issues) == 0) "Good" else if (length(quality_issues) <= 2) "Fair" else "Poor"
  
  return(list(
    quality_rating = quality_rating,
    quality_issues = if (length(quality_issues) > 0) quality_issues else "No major issues identified"
  ))
}

# Helper functions loaded from modules/utils/statistical_helpers.R