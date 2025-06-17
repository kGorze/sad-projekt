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
  master_summary$data_quality <- assess_comprehensive_data_quality(data, numeric_vars, categorical_vars, group_column)
  
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
    
    # Basic descriptive statistics
    basic_stats <- calculate_basic_statistics(var_data)
    
    # Distribution characteristics
    distribution_stats <- calculate_distribution_characteristics(var_data)
    
    # Outlier detection
    outlier_info <- detect_outliers_comprehensive(var_data)
    
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
      basic_statistics = basic_stats,
      distribution_characteristics = distribution_stats,
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

# Calculate basic descriptive statistics
calculate_basic_statistics <- function(data_vector) {
  
  if (length(data_vector) == 0) {
    return(NULL)
  }
  
  stats <- list(
    n = length(data_vector),
    mean = mean(data_vector, na.rm = TRUE),
    median = median(data_vector, na.rm = TRUE),
    sd = sd(data_vector, na.rm = TRUE),
    var = var(data_vector, na.rm = TRUE),
    min = min(data_vector, na.rm = TRUE),
    max = max(data_vector, na.rm = TRUE),
    q25 = quantile(data_vector, 0.25, na.rm = TRUE),
    q75 = quantile(data_vector, 0.75, na.rm = TRUE),
    iqr = IQR(data_vector, na.rm = TRUE),
    range = max(data_vector, na.rm = TRUE) - min(data_vector, na.rm = TRUE),
    cv = ifelse(mean(data_vector, na.rm = TRUE) != 0, 
                sd(data_vector, na.rm = TRUE) / mean(data_vector, na.rm = TRUE) * 100, 
                NA)
  )
  
  # Round numeric values
  stats <- lapply(stats, function(x) if (is.numeric(x)) round(x, 4) else x)
  
  return(stats)
}

# Calculate distribution characteristics
calculate_distribution_characteristics <- function(data_vector) {
  
  if (length(data_vector) < 3) {
    return(NULL)
  }
  
  # Skewness and kurtosis
  skewness_val <- calculate_skewness(data_vector)
  kurtosis_val <- calculate_kurtosis(data_vector)
  
  # Distribution shape assessment
  skewness_interpretation <- ifelse(abs(skewness_val) < 0.5, "Approximately symmetric",
                                   ifelse(abs(skewness_val) < 1.0, "Moderately skewed",
                                          "Highly skewed"))
  
  kurtosis_interpretation <- ifelse(abs(kurtosis_val) < 0.5, "Approximately normal",
                                   ifelse(abs(kurtosis_val) < 1.0, "Moderately peaked/flat",
                                          "Highly peaked/flat"))
  
  # Mode estimation (for continuous data, use density peak)
  mode_estimate <- estimate_mode(data_vector)
  
  return(list(
    skewness = round(skewness_val, 4),
    kurtosis = round(kurtosis_val, 4),
    skewness_interpretation = skewness_interpretation,
    kurtosis_interpretation = kurtosis_interpretation,
    estimated_mode = round(mode_estimate, 4)
  ))
}

# Comprehensive outlier detection
detect_outliers_comprehensive <- function(data_vector) {
  
  if (length(data_vector) < 4) {
    return(list(
      method = "insufficient_data",
      outlier_count = 0,
      outlier_percentage = 0
    ))
  }
  
  # IQR method
  Q1 <- quantile(data_vector, 0.25)
  Q3 <- quantile(data_vector, 0.75)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  iqr_outliers <- sum(data_vector < lower_bound | data_vector > upper_bound)
  
  # Z-score method (|z| > 2.5 for moderate outliers, |z| > 3 for extreme)
  z_scores <- abs(scale(data_vector))
  moderate_z_outliers <- sum(z_scores > 2.5, na.rm = TRUE)
  extreme_z_outliers <- sum(z_scores > 3, na.rm = TRUE)
  
  # Modified Z-score method
  median_val <- median(data_vector)
  mad_val <- mad(data_vector)
  if (mad_val > 0) {
    modified_z_scores <- 0.6745 * (data_vector - median_val) / mad_val
    modified_z_outliers <- sum(abs(modified_z_scores) > 3.5)
  } else {
    modified_z_outliers <- 0
  }
  
  return(list(
    iqr_outliers = iqr_outliers,
    iqr_percentage = round((iqr_outliers / length(data_vector)) * 100, 2),
    z_moderate_outliers = moderate_z_outliers,
    z_extreme_outliers = extreme_z_outliers,
    modified_z_outliers = modified_z_outliers,
    iqr_bounds = list(lower = round(lower_bound, 4), upper = round(upper_bound, 4)),
    recommendation = ifelse(iqr_outliers / length(data_vector) > 0.1, 
                           "High outlier percentage - investigate data quality",
                           "Acceptable outlier levels")
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
      basic_stats <- calculate_basic_statistics(group_data)
      distribution_stats <- calculate_distribution_characteristics(group_data)
      outlier_info <- detect_outliers_comprehensive(group_data)
      
      group_summaries[[as.character(group)]] <- list(
        group = group,
        n = length(group_data),
        basic_statistics = basic_stats,
        distribution_characteristics = distribution_stats,
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
    
    if (is.null(var_summary$basic_statistics)) {
      next
    }
    
    basic <- var_summary$basic_statistics
    dist <- var_summary$distribution_characteristics
    outliers <- var_summary$outlier_information
    
    table_rows[[var_name]] <- data.frame(
      Variable = var_name,
      N = basic$n,
      Missing = var_summary$n_missing,
      Missing_Pct = var_summary$missing_percentage,
      Mean = basic$mean,
      SD = basic$sd,
      Median = basic$median,
      Q25 = basic$q25,
      Q75 = basic$q75,
      Min = basic$min,
      Max = basic$max,
      CV_Pct = basic$cv,
      Skewness = if (!is.null(dist)) dist$skewness else NA,
      Kurtosis = if (!is.null(dist)) dist$kurtosis else NA,
      Outliers_IQR = outliers$iqr_outliers,
      Outliers_Pct = outliers$iqr_percentage,
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

# Assess comprehensive data quality for analysis
assess_comprehensive_data_quality <- function(data, numeric_vars, categorical_vars, group_column) {
  
  quality_issues <- list()
  quality_score <- 100  # Start with perfect score and deduct points
  
  # Missing data assessment
  missing_by_variable <- sapply(data, function(x) sum(is.na(x)))
  high_missing_vars <- names(missing_by_variable)[missing_by_variable / nrow(data) > 0.05]
  
  if (length(high_missing_vars) > 0) {
    quality_issues$high_missing <- paste("Variables with >5% missing data:", paste(high_missing_vars, collapse = ", "))
    quality_score <- quality_score - (length(high_missing_vars) * 5)
  }
  
  # Outlier assessment for numeric variables
  high_outlier_vars <- c()
  for (var in numeric_vars) {
    var_data <- data[[var]][!is.na(data[[var]])]
    if (length(var_data) > 4) {
      outlier_info <- detect_outliers_comprehensive(var_data)
      if (outlier_info$iqr_percentage > 10) {
        high_outlier_vars <- c(high_outlier_vars, var)
      }
    }
  }
  
  if (length(high_outlier_vars) > 0) {
    quality_issues$high_outliers <- paste("Variables with >10% outliers:", paste(high_outlier_vars, collapse = ", "))
    quality_score <- quality_score - (length(high_outlier_vars) * 3)
  }
  
  # Sample size assessment
  if (nrow(data) < 30) {
    quality_issues$small_sample <- "Small sample size (n < 30) may limit statistical power"
    quality_score <- quality_score - 10
  }
  
  # Group balance assessment (if applicable)
  if (!is.null(group_column)) {
    group_sizes <- table(data[[group_column]])
    min_group_size <- min(group_sizes)
    max_group_size <- max(group_sizes)
    
    if (min_group_size < 10) {
      quality_issues$small_groups <- paste("Some groups have <10 observations:", paste(names(group_sizes)[group_sizes < 10], collapse = ", "))
      quality_score <- quality_score - 5
    }
    
    if (max_group_size / min_group_size > 3) {
      quality_issues$unbalanced_groups <- "Groups are highly unbalanced (ratio > 3:1)"
      quality_score <- quality_score - 5
    }
  }
  
  # Overall quality rating
  quality_rating <- ifelse(quality_score >= 90, "Excellent",
                          ifelse(quality_score >= 80, "Good",
                                ifelse(quality_score >= 70, "Fair", "Poor")))
  
  return(list(
    quality_score = max(quality_score, 0),
    quality_rating = quality_rating,
    quality_issues = quality_issues,
    recommendations = generate_quality_recommendations(quality_issues)
  ))
}

# Generate quality improvement recommendations
generate_quality_recommendations <- function(quality_issues) {
  
  recommendations <- c()
  
  if ("high_missing" %in% names(quality_issues)) {
    recommendations <- c(recommendations, "Consider imputation methods for variables with high missing data")
  }
  
  if ("high_outliers" %in% names(quality_issues)) {
    recommendations <- c(recommendations, "Consider outlier treatment (removal, transformation, or robust methods)")
  }
  
  if ("small_sample" %in% names(quality_issues)) {
    recommendations <- c(recommendations, "Consider collecting more data or using appropriate small-sample methods")
  }
  
  if ("small_groups" %in% names(quality_issues)) {
    recommendations <- c(recommendations, "Consider combining small groups or using appropriate small-sample tests")
  }
  
  if ("unbalanced_groups" %in% names(quality_issues)) {
    recommendations <- c(recommendations, "Consider stratified analysis or appropriate unbalanced design methods")
  }
  
  if (length(recommendations) == 0) {
    recommendations <- "Data quality is good - no major issues identified"
  }
  
  return(recommendations)
}

# Helper function to estimate mode for continuous data
estimate_mode <- function(x) {
  if (length(x) < 3) return(NA)
  
  # Use density estimation to find peak
  tryCatch({
    density_result <- density(x, na.rm = TRUE)
    mode_estimate <- density_result$x[which.max(density_result$y)]
    return(mode_estimate)
  }, error = function(e) {
    # Fallback to median if density estimation fails
    return(median(x, na.rm = TRUE))
  })
}

# Helper functions now loaded from modules/utils/statistical_helpers.R