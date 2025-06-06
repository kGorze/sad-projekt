# Data Fetching Module
# Functions for loading and initial validation of CSV datasets
# Handles file reading and basic data structure validation

# Main function to load dataset from CSV file
fetch_dataset <- function(file_path, encoding = "UTF-8") {
  # Validate file path first
  if (!validate_file_path(file_path)) {
    stop("File validation failed for: ", file_path)
  }
  
  # Detect encoding if not specified
  if (is.null(encoding) || encoding == "auto") {
    encoding <- detect_encoding(file_path)
  }
  
  # Read CSV with proper encoding and Polish format handling
  tryCatch({
    # Read with semicolon separator (Polish CSV format)
    data <- read.csv(file_path, 
                     sep = ";", 
                     dec = ",",  # Polish decimal separator
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     encoding = encoding,
                     na.strings = c("", "NA", "N/A", " "))
    
    # Convert numeric columns (handle Polish decimal format)
    numeric_cols <- c("wiek", "hsCRP", "ERY", "PLT", "HGB", "HCT", "MCHC", "MON", "LEU")
    
    for (col in numeric_cols) {
      if (col %in% names(data)) {
        # Replace comma with dot and convert to numeric
        data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
      }
    }
    
    # Convert categorical columns to factors
    if ("grupa" %in% names(data)) {
      data$grupa <- as.factor(data$grupa)
    }
    if ("plec" %in% names(data)) {
      data$plec <- as.factor(data$plec)
    }
    
    cat("Successfully loaded dataset with", nrow(data), "rows and", ncol(data), "columns\n")
    
    # Perform initial data structure inspection
    inspect_data_structure(data)
    
    return(data)
    
  }, error = function(e) {
    stop("Error reading CSV file: ", e$message)
  })
}

# Validate file path and accessibility
validate_file_path <- function(file_path) {
  # Check if file path is provided
  if (is.null(file_path) || file_path == "") {
    cat("Error: No file path provided\n")
    return(FALSE)
  }
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("Error: File does not exist:", file_path, "\n")
    return(FALSE)
  }
  
  # Check if file is readable
  if (file.access(file_path, mode = 4) != 0) {
    cat("Error: File is not readable:", file_path, "\n")
    return(FALSE)
  }
  
  # Validate file extension
  file_ext <- tolower(tools::file_ext(file_path))
  if (!file_ext %in% c("csv", "txt")) {
    cat("Warning: File extension is not CSV or TXT:", file_ext, "\n")
  }
  
  # Check file size (warn if too large)
  file_size <- file.info(file_path)$size
  if (file_size > 50 * 1024 * 1024) {  # 50 MB
    cat("Warning: Large file size detected:", round(file_size / 1024 / 1024, 2), "MB\n")
  }
  
  cat("File validation successful for:", file_path, "\n")
  return(TRUE)
}

# Detect and handle data encoding issues
detect_encoding <- function(file_path) {
  # Try to detect encoding by reading first few lines
  tryCatch({
    # First try UTF-8
    test_utf8 <- readLines(file_path, n = 5, encoding = "UTF-8", warn = FALSE)
    
    # Check for Polish characters and encoding issues
    has_polish_chars <- any(grepl("[ąćęłńóśźż]", test_utf8, ignore.case = TRUE))
    
    # If UTF-8 works and we have Polish characters, use UTF-8
    if (has_polish_chars) {
      cat("Detected Polish characters, using UTF-8 encoding\n")
      return("UTF-8")
    }
    
    # Try Windows-1250 (common for Polish files)
    test_cp1250 <- readLines(file_path, n = 5, encoding = "CP1250", warn = FALSE)
    has_polish_cp1250 <- any(grepl("[ąćęłńóśźż]", test_cp1250, ignore.case = TRUE))
    
    if (has_polish_cp1250) {
      cat("Detected Polish characters with Windows-1250 encoding\n")
      return("CP1250")
    }
    
    # Default to UTF-8
    cat("No specific encoding detected, defaulting to UTF-8\n")
    return("UTF-8")
    
  }, error = function(e) {
    cat("Encoding detection failed, defaulting to UTF-8\n")
    return("UTF-8")
  })
}

# Initial data structure inspection
inspect_data_structure <- function(data) {
  cat("\n=== DATA STRUCTURE INSPECTION ===\n")
  
  # Check column names
  cat("Column names:\n")
  print(names(data))
  
  # Count rows and columns
  cat("\nDataset dimensions:\n")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", ncol(data), "\n")
  
  # Identify data types
  cat("\nColumn types:\n")
  for (col in names(data)) {
    cat(sprintf("%-10s: %s\n", col, class(data[[col]])[1]))
  }
  
  # Check for missing values
  cat("\nMissing values per column:\n")
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  for (col in names(missing_counts)) {
    if (missing_counts[col] > 0) {
      cat(sprintf("%-10s: %d missing (%.1f%%)\n", 
                  col, missing_counts[col], 
                  100 * missing_counts[col] / nrow(data)))
    }
  }
  
  # Check group distribution if grupa column exists
  if ("grupa" %in% names(data)) {
    cat("\nGroup distribution:\n")
    print(table(data$grupa))
  }
  
  # Check for potential data quality issues
  cat("\nPotential issues detected:\n")
  
  # Check for outliers in numeric columns
  numeric_cols <- sapply(data, is.numeric)
  for (col in names(data)[numeric_cols]) {
    if (sum(!is.na(data[[col]])) > 0) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- sum(data[[col]] < (q1 - 1.5 * iqr) | 
                      data[[col]] > (q3 + 1.5 * iqr), na.rm = TRUE)
      if (outliers > 0) {
        cat(sprintf("- %s: %d potential outliers\n", col, outliers))
      }
    }
  }
  
  cat("\n=== END INSPECTION ===\n\n")
  
  # Return summary for further use
  return(list(
    dimensions = c(nrow(data), ncol(data)),
    column_types = sapply(data, class),
    missing_values = missing_counts,
    group_counts = if("grupa" %in% names(data)) table(data$grupa) else NULL
  ))
}
