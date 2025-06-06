# Data Fetching Module
# Functions for loading CSV datasets with Polish format support
# Handles file reading, encoding detection, and basic data type conversion

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
    
    # Convert data types appropriately
    data <- convert_data_types(data)
    
    cat("Successfully loaded dataset with", nrow(data), "rows and", ncol(data), "columns\n")
    
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

# Convert data types for medical data
convert_data_types <- function(data) {
  
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
  
  return(data)
}

# Get basic dataset information (for logging/reporting)
get_dataset_info <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      valid = FALSE,
      dimensions = c(0, 0),
      column_types = NULL,
      message = "Dataset is empty or null"
    ))
  }
  
  return(list(
    valid = TRUE,
    dimensions = c(nrow(data), ncol(data)),
    column_names = names(data),
    column_types = sapply(data, class),
    numeric_columns = sum(sapply(data, is.numeric)),
    factor_columns = sum(sapply(data, is.factor)),
    total_cells = nrow(data) * ncol(data)
  ))
}
