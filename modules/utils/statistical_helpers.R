# Statistical calculation utilities
# Centralized location for commonly used statistical calculation functions
# Eliminates duplication across multiple analysis modules

# Calculate skewness of a numeric vector
calculate_skewness <- function(x) {
  if (length(x) < 3 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(0)
  
  skew <- sum(((x - mean_x) / sd_x)^3) / n
  return(skew)
}

# Calculate excess kurtosis of a numeric vector (normal distribution has kurtosis = 0)
calculate_kurtosis <- function(x) {
  if (length(x) < 4 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(0)
  
  # Calculate excess kurtosis (normal distribution has kurtosis = 0)
  kurt <- sum(((x - mean_x) / sd_x)^4) / n - 3
  return(kurt)
}

# Calculate coefficient of variation (CV) in percentage
calculate_cv <- function(x) {
  if (length(x) < 2 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (mean_x == 0) return(NA)
  
  cv <- (sd_x / mean_x) * 100
  return(cv)
}

# Calculate robust measures: median absolute deviation (MAD)
calculate_mad <- function(x) {
  if (length(x) < 2 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  mad_value <- mad(x, na.rm = TRUE)
  return(mad_value)
}

# Calculate interquartile range (IQR)
calculate_iqr <- function(x) {
  if (length(x) < 4 || all(is.na(x))) return(NA)
  
  x <- x[!is.na(x)]
  iqr_value <- IQR(x, na.rm = TRUE)
  return(iqr_value)
}

# Test if a variable is approximately normal based on skewness and kurtosis
is_approximately_normal <- function(x, skew_threshold = 1.0, kurt_threshold = 1.0) {
  if (length(x) < 3) return(FALSE)
  
  skew <- calculate_skewness(x)
  kurt <- calculate_kurtosis(x)
  
  if (is.na(skew) || is.na(kurt)) return(FALSE)
  
  skew_ok <- abs(skew) < skew_threshold
  kurt_ok <- abs(kurt) < kurt_threshold
  
  return(skew_ok && kurt_ok)
}

# Calculate standardized z-scores
calculate_z_scores <- function(x) {
  if (length(x) < 2 || all(is.na(x))) return(rep(NA, length(x)))
  
  z_scores <- scale(x)[, 1]
  return(z_scores)
}

# Calculate modified z-scores using median and MAD
calculate_modified_z_scores <- function(x) {
  if (length(x) < 2 || all(is.na(x))) return(rep(NA, length(x)))
  
  x_clean <- x[!is.na(x)]
  median_x <- median(x_clean)
  mad_x <- mad(x_clean)
  
  if (mad_x == 0) return(rep(0, length(x)))
  
  modified_z <- 0.6745 * (x - median_x) / mad_x
  return(modified_z)
}

# Determine outliers using IQR method
detect_iqr_outliers <- function(x, multiplier = 1.5) {
  if (length(x) < 4) return(rep(FALSE, length(x)))
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - multiplier * IQR_val
  upper_bound <- Q3 + multiplier * IQR_val
  
  outliers <- x < lower_bound | x > upper_bound
  outliers[is.na(x)] <- FALSE  # Missing values are not outliers
  
  return(outliers)
}

# Determine outliers using z-score method
detect_z_outliers <- function(x, threshold = 3) {
  if (length(x) < 2) return(rep(FALSE, length(x)))
  
  z_scores <- abs(calculate_z_scores(x))
  outliers <- z_scores > threshold
  outliers[is.na(z_scores)] <- FALSE
  
  return(outliers)
}

# Outlier detection summary
detect_outliers_summary <- function(x) {
  if (length(x) < 4) {
    return(list(
      iqr_outliers = rep(FALSE, length(x)),
      z_outliers = rep(FALSE, length(x)),
      modified_z_outliers = rep(FALSE, length(x)),
      outlier_count = 0,
      outlier_percentage = 0
    ))
  }
  
  iqr_outliers <- detect_iqr_outliers(x)
  z_outliers <- detect_z_outliers(x, threshold = 3)
  modified_z_scores <- abs(calculate_modified_z_scores(x))
  modified_z_outliers <- modified_z_scores > 3.5
  modified_z_outliers[is.na(modified_z_scores)] <- FALSE
  
  # Use IQR method as primary outlier detection
  primary_outliers <- iqr_outliers
  outlier_count <- sum(primary_outliers, na.rm = TRUE)
  total_valid <- sum(!is.na(x))
  outlier_percentage <- if (total_valid > 0) (outlier_count / total_valid) * 100 else 0
  
  return(list(
    iqr_outliers = iqr_outliers,
    z_outliers = z_outliers,
    modified_z_outliers = modified_z_outliers,
    outlier_count = outlier_count,
    outlier_percentage = round(outlier_percentage, 2)
  ))
}

# Calculate basic summary statistics
calculate_basic_summary <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(list(
      n = 0,
      n_missing = length(x),
      mean = NA,
      median = NA,
      sd = NA,
      min = NA,
      max = NA,
      q25 = NA,
      q75 = NA
    ))
  }
  
  x_clean <- x[!is.na(x)]
  n_valid <- length(x_clean)
  n_missing <- sum(is.na(x))
  
  summary_stats <- list(
    n = n_valid,
    n_missing = n_missing,
    mean = mean(x_clean),
    median = median(x_clean),
    sd = sd(x_clean),
    min = min(x_clean),
    max = max(x_clean),
    q25 = quantile(x_clean, 0.25),
    q75 = quantile(x_clean, 0.75)
  )
  
  # Round numeric values
  summary_stats <- lapply(summary_stats, function(val) {
    if (is.numeric(val) && !is.na(val)) round(val, 4) else val
  })
  
  return(summary_stats)
}

# Calculate distribution shape interpretation
interpret_distribution_shape <- function(x) {
  if (length(x) < 3) {
    return(list(
      shape = "insufficient_data",
      skewness_interpretation = "Cannot determine",
      kurtosis_interpretation = "Cannot determine"
    ))
  }
  
  skew <- calculate_skewness(x)
  kurt <- calculate_kurtosis(x)
  
  # Interpret skewness
  if (is.na(skew)) {
    skew_interp <- "Cannot calculate"
  } else if (abs(skew) < 0.5) {
    skew_interp <- "Approximately symmetric"
  } else if (abs(skew) < 1.0) {
    skew_interp <- "Moderately skewed"
  } else if (abs(skew) < 2.0) {
    skew_interp <- "Highly skewed"
  } else {
    skew_interp <- "Extremely skewed"
  }
  
  # Interpret kurtosis
  if (is.na(kurt)) {
    kurt_interp <- "Cannot calculate"
  } else if (abs(kurt) < 0.5) {
    kurt_interp <- "Normal-like peakedness"
  } else if (abs(kurt) < 1.0) {
    kurt_interp <- "Moderately peaked/flat"
  } else if (abs(kurt) < 2.0) {
    kurt_interp <- "Highly peaked/flat"
  } else {
    kurt_interp <- "Extremely peaked/flat"
  }
  
  # Overall shape assessment
  if (is.na(skew) || is.na(kurt)) {
    overall_shape <- "Cannot determine"
  } else if (abs(skew) < 1.0 && abs(kurt) < 1.0) {
    overall_shape <- "Approximately normal"
  } else if (abs(skew) >= 2.0 || abs(kurt) >= 2.0) {
    overall_shape <- "Highly non-normal"
  } else {
    overall_shape <- "Moderately non-normal"
  }
  
  return(list(
    shape = overall_shape,
    skewness = round(skew, 4),
    kurtosis = round(kurt, 4),
    skewness_interpretation = skew_interp,
    kurtosis_interpretation = kurt_interp
  ))
} 