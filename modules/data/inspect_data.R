# Data structure and quality examination
# Provides detailed information about datasets without modification

# Main data inspection function
inspect_data_structure <- function(data) {
  cat("\n=== DATA STRUCTURE INSPECTION ===\n")
  
  if (is.null(data) || nrow(data) == 0) {
    cat("Dataset is empty or null\n")
    cat("=== END INSPECTION ===\n\n")
    return(NULL)
  }
  
  # Basic structure information
  structure_info <- get_structure_info(data)
  
  # Missing data analysis
  missing_info <- analyze_missing_data(data)
  
  # Data quality overview
  quality_info <- assess_data_quality(data)
  
  # Group information (if applicable)
  group_info <- analyze_group_structure(data)
  
  cat("=== END INSPECTION ===\n\n")
  
  # Return comprehensive inspection results
  return(list(
    structure = structure_info,
    missing_data = missing_info,
    quality = quality_info,
    groups = group_info,
    timestamp = Sys.time()
  ))
}

# Get basic structure information
get_structure_info <- function(data) {
  
  cat("Column names:\n")
  print(names(data))
  
  cat("\nDataset dimensions:\n")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", ncol(data), "\n")
  
  cat("\nColumn types:\n")
  for (col in names(data)) {
    cat(sprintf("%-10s: %s\n", col, class(data[[col]])[1]))
  }
  
  return(list(
    dimensions = c(nrow(data), ncol(data)),
    column_names = names(data),
    column_types = sapply(data, class),
    numeric_count = sum(sapply(data, is.numeric)),
    factor_count = sum(sapply(data, is.factor)),
    character_count = sum(sapply(data, is.character)),
    logical_count = sum(sapply(data, is.logical))
  ))
}

# Analyze missing data patterns
analyze_missing_data <- function(data) {
  
  cat("\nMissing values per column:\n")
  
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_percentages <- round(100 * missing_counts / nrow(data), 1)
  
  has_missing <- FALSE
  for (col in names(missing_counts)) {
    if (missing_counts[col] > 0) {
      cat(sprintf("%-10s: %d missing (%.1f%%)\n", 
                  col, missing_counts[col], missing_percentages[col]))
      has_missing <- TRUE
    }
  }
  
  if (!has_missing) {
    cat("No missing values detected.\n")
  }
  
  return(list(
    missing_counts = missing_counts,
    missing_percentages = missing_percentages,
    total_missing = sum(missing_counts),
    complete_cases = sum(complete.cases(data)),
    missing_rate = sum(missing_counts) / (nrow(data) * ncol(data))
  ))
}

# Assess basic data quality
assess_data_quality <- function(data) {
  
  cat("\nPotential issues detected:\n")
  
  quality_issues <- list()
  
  # Check for outliers in numeric columns
  numeric_cols <- sapply(data, is.numeric)
  outlier_summary <- list()
  
  for (col in names(data)[numeric_cols]) {
    if (sum(!is.na(data[[col]])) > 0) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- sum(data[[col]] < (q1 - 1.5 * iqr) | 
                      data[[col]] > (q3 + 1.5 * iqr), na.rm = TRUE)
      
      outlier_summary[[col]] <- outliers
      
      if (outliers > 0) {
        cat(sprintf("- %s: %d potential outliers\n", col, outliers))
      }
    }
  }
  
  # Check for constant variables
  constant_vars <- names(data)[sapply(data, function(x) {
    if (is.numeric(x)) {
      length(unique(x[!is.na(x)])) <= 1
    } else {
      length(unique(x[!is.na(x)])) <= 1
    }
  })]
  
  if (length(constant_vars) > 0) {
    cat(sprintf("- Constant variables detected: %s\n", paste(constant_vars, collapse = ", ")))
    quality_issues$constant_variables <- constant_vars
  }
  
  # Check for duplicate rows
  duplicate_rows <- sum(duplicated(data))
  if (duplicate_rows > 0) {
    cat(sprintf("- %d duplicate rows detected\n", duplicate_rows))
    quality_issues$duplicate_rows <- duplicate_rows
  }
  
  # Check for extreme values (beyond 3 IQRs)
  extreme_values <- list()
  for (col in names(data)[numeric_cols]) {
    if (sum(!is.na(data[[col]])) > 0) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      extreme <- sum(data[[col]] < (q1 - 3 * iqr) | 
                     data[[col]] > (q3 + 3 * iqr), na.rm = TRUE)
      if (extreme > 0) {
        extreme_values[[col]] <- extreme
      }
    }
  }
  
  if (length(extreme_values) > 0) {
    quality_issues$extreme_values <- extreme_values
  }
  
  return(list(
    outliers = outlier_summary,
    quality_issues = quality_issues,
    total_outliers = sum(unlist(outlier_summary)),
    data_completeness = 1 - (sum(sapply(data, function(x) sum(is.na(x)))) / (nrow(data) * ncol(data)))
  ))
}

# Analyze group structure (if group variable exists)
analyze_group_structure <- function(data) {
  
  group_info <- list()
  
  # Check for grupa column
  if ("grupa" %in% names(data)) {
    cat("\nGroup distribution:\n")
    group_table <- table(data$grupa, useNA = "ifany")
    print(group_table)

    group_info$group_counts <- group_table
    group_info$group_proportions <- prop.table(group_table)

    # Use only groups with at least one observation
    group_sizes <- as.numeric(group_table)
    nonzero_sizes <- group_sizes[group_sizes > 0]

    group_info$n_groups <- length(nonzero_sizes)

    if (length(nonzero_sizes) > 1) {
      group_info$balance_ratio <- max(nonzero_sizes) / min(nonzero_sizes)
      group_info$is_balanced <- group_info$balance_ratio <= 1.5

      if (group_info$balance_ratio > 2) {
        cat(sprintf("WARNING: Unbalanced groups detected (ratio: %.2f)\n", group_info$balance_ratio))
      }
    } else {
      group_info$balance_ratio <- NA
      group_info$is_balanced <- TRUE
    }
  }
  
  # Check for plec (gender) if exists
  if ("plec" %in% names(data)) {
    cat("\nGender distribution:\n")
    gender_table <- table(data$plec, useNA = "ifany")
    print(gender_table)
    
    group_info$gender_counts <- gender_table
    
    # Cross-tabulation if both group and gender exist
    if ("grupa" %in% names(data)) {
      cat("\nGender by group distribution:\n")
      cross_table <- table(data$grupa, data$plec, useNA = "ifany")
      print(cross_table)
      group_info$gender_by_group <- cross_table
    }
  }
  
  return(group_info)
}

# Generate summary statistics for numeric variables
generate_numeric_summary <- function(data, by_group = FALSE) {
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  if (length(numeric_cols) == 0) {
    cat("No numeric variables found for summary statistics.\n")
    return(NULL)
  }
  
  summary_stats <- list()
  
  if (by_group && "grupa" %in% names(data)) {
    cat("\nNumeric variable summaries by group:\n")
    
    for (col in numeric_cols) {
      cat(sprintf("\n--- %s ---\n", col))
      group_summary <- aggregate(data[[col]], 
                                 by = list(Group = data$grupa), 
                                 FUN = function(x) {
                                   c(
                                     n = sum(!is.na(x)),
                                     mean = round(mean(x, na.rm = TRUE), 3),
                                     sd = round(sd(x, na.rm = TRUE), 3),
                                     median = round(median(x, na.rm = TRUE), 3),
                                     min = min(x, na.rm = TRUE),
                                     max = max(x, na.rm = TRUE)
                                   )
                                 })
      print(group_summary)
      summary_stats[[col]] <- group_summary
    }
  } else {
    cat("\nOverall numeric variable summaries:\n")
    
    for (col in numeric_cols) {
      cat(sprintf("\n%s:\n", col))
      col_summary <- summary(data[[col]])
      print(col_summary)
      summary_stats[[col]] <- col_summary
    }
  }
  
  return(summary_stats)
}

# Quick data overview function
quick_data_overview <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    cat("Dataset is empty or null\n")
    return(NULL)
  }
  
  cat(sprintf("Dataset: %d rows × %d columns\n", nrow(data), ncol(data)))
  cat(sprintf("Missing values: %d (%.1f%%)\n", 
              sum(sapply(data, function(x) sum(is.na(x)))),
              100 * sum(sapply(data, function(x) sum(is.na(x)))) / (nrow(data) * ncol(data))))
  
  if ("grupa" %in% names(data)) {
    cat(sprintf("Groups: %d (%s)\n", 
                nlevels(data$grupa), 
                paste(levels(data$grupa), collapse = ", ")))
  }
  
  cat(sprintf("Numeric variables: %d\n", sum(sapply(data, is.numeric))))
  cat(sprintf("Factor variables: %d\n", sum(sapply(data, is.factor))))
  
  return(invisible(data))
} 