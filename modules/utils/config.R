# Configuration Module
# Global settings and parameters for the statistical analysis tool
# Centralized configuration management

# Statistical analysis settings
STATISTICAL_SETTINGS <- list(
  alpha_level = 0.05,          # Significance level
  confidence_level = 0.95,     # Confidence interval level
  multiple_comparison_method = "holm",  # Multiple comparison correction
  normality_test = "shapiro",  # Default normality test
  variance_test = "levene",    # Default variance homogeneity test
  effect_size_threshold = 0.5  # Minimum effect size for practical significance
)

# Data processing settings
DATA_SETTINGS <- list(
  missing_data_threshold = 0.1,    # Maximum proportion of missing data
  outlier_method = "iqr",          # Outlier detection method
  outlier_threshold = 1.5,         # IQR multiplier for outliers
  min_group_size = 5,              # Minimum sample size per group
  decimal_places = 3               # Decimal places for results
)

# Visualization settings
PLOT_SETTINGS <- list(
  theme = "minimal",               # ggplot2 theme
  color_palette = "viridis",       # Default color palette
  figure_width = 8,                # Default figure width (inches)
  figure_height = 6,               # Default figure height (inches)
  dpi = 300,                       # Resolution for saved plots
  font_size = 12                   # Base font size
)

# Output settings
OUTPUT_SETTINGS <- list(
  create_subdirectories = TRUE,    # Create organized output structure
  save_intermediate_results = TRUE, # Save intermediate analysis steps
  generate_plots = TRUE,           # Generate visualization plots
  export_raw_data = FALSE,         # Export processed raw data
  report_format = "pdf"            # Default report format
)

# Global variable to track loaded packages
.loaded_packages <- character(0)

# Load required packages centrally to avoid conflicts and warnings
load_required_packages <- function(packages = NULL, quiet = TRUE) {
  # Set CRAN mirror if not set
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }
  
  # Complete list of packages used in the project (ordered to minimize conflicts)
  all_packages <- c(
    # Base packages first
    "dplyr", "ggplot2",
    
    # Data manipulation and repair packages
    "VIM", "mice", "Hmisc", 
    
    # Statistical analysis packages
    "car", "dunn.test", "effsize", "psych", "broom", "nortest",
    
    # Visualization packages
    "ggpubr", "gridExtra", "corrplot", "GGally", "ggcorrplot",
    
    # Reporting packages
    "rmarkdown", "knitr", "htmltools", "DT", "plotly",
    
    # Export packages
    "writexl", "readr"
  )
  
  # Use specified packages or all packages
  packages_to_load <- if (!is.null(packages)) packages else all_packages
  
  # Only load packages that aren't already loaded
  packages_to_load <- setdiff(packages_to_load, .loaded_packages)
  
  if (length(packages_to_load) == 0) {
    return(invisible(TRUE))
  }
  
  success <- TRUE
  failed_packages <- character(0)
  
  for (pkg in packages_to_load) {
    tryCatch({
      if (quiet) {
        suppressPackageStartupMessages(
          suppressWarnings({
            library(pkg, character.only = TRUE, quietly = FALSE, warn.conflicts = FALSE)
          })
        )
      } else {
        library(pkg, character.only = TRUE)
      }
      .loaded_packages <<- c(.loaded_packages, pkg)
      
      # Special handling for key packages to ensure functions are available
      if (pkg == "dplyr") {
        # Ensure the pipe operator is available in the global environment
        if (!exists("%>%", envir = .GlobalEnv)) {
          # Import the pipe operator from dplyr
          `%>%` <- get("%>%", envir = asNamespace("dplyr"))
          assign("%>%", `%>%`, envir = .GlobalEnv)
        }
      }
      
      # Ensure ggplot2 functions are available globally
      if (pkg == "ggplot2") {
        if (!exists("ggplot", envir = .GlobalEnv)) {
          # Import key ggplot2 functions to global environment
          ggplot <- get("ggplot", envir = asNamespace("ggplot2"))
          assign("ggplot", ggplot, envir = .GlobalEnv)
        }
      }
      
      # Ensure ggpubr functions are available globally  
      if (pkg == "ggpubr") {
        if (!exists("ggboxplot", envir = .GlobalEnv)) {
          # Import key ggpubr functions to global environment
          ggboxplot <- get("ggboxplot", envir = asNamespace("ggpubr"))
          assign("ggboxplot", ggboxplot, envir = .GlobalEnv)
        }
      }
      
    }, error = function(e) {
      # Try to install if not available
      tryCatch({
        if (quiet) {
          suppressMessages(install.packages(pkg, repos = "https://cran.rstudio.com/", quiet = TRUE))
          suppressPackageStartupMessages(
            suppressWarnings({
              library(pkg, character.only = TRUE, quietly = FALSE, warn.conflicts = FALSE)
            })
          )
        } else {
          install.packages(pkg, repos = "https://cran.rstudio.com/")
          library(pkg, character.only = TRUE)
        }
        .loaded_packages <<- c(.loaded_packages, pkg)
        
        # Special handling for dplyr to ensure %>% is available (error recovery case)
        if (pkg == "dplyr") {
          # Ensure the pipe operator is available in the global environment
          if (!exists("%>%", envir = .GlobalEnv)) {
            # Import the pipe operator from dplyr
            `%>%` <- get("%>%", envir = asNamespace("dplyr"))
            assign("%>%", `%>%`, envir = .GlobalEnv)
          }
        }
        
      }, error = function(e2) {
        failed_packages <<- c(failed_packages, pkg)
        success <<- FALSE
        if (!quiet) {
          warning(sprintf("Could not load package '%s': %s", pkg, e2$message))
        }
      })
    })
  }
  
  if (length(failed_packages) > 0 && !quiet) {
    cat("Note: Some packages could not be loaded:", paste(failed_packages, collapse = ", "), "\n")
    cat("Some advanced features may be limited, but basic functionality will work.\n")
  }
  
  return(success)
}

# Helper function to load packages safely for individual modules
load_packages_safely <- function(packages, quiet = TRUE) {
  return(load_required_packages(packages, quiet))
}

# Function to check if packages are available
check_package_availability <- function(packages) {
  available <- sapply(packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })
  return(available)
}

# Validate configuration
validate_config <- function() {
  # Check parameter values are within expected ranges
  issues <- character(0)
  
  if (STATISTICAL_SETTINGS$alpha_level <= 0 || STATISTICAL_SETTINGS$alpha_level >= 1) {
    issues <- c(issues, "Alpha level must be between 0 and 1")
  }
  
  if (DATA_SETTINGS$missing_data_threshold < 0 || DATA_SETTINGS$missing_data_threshold > 1) {
    issues <- c(issues, "Missing data threshold must be between 0 and 1")
  }
  
  if (DATA_SETTINGS$min_group_size < 1) {
    issues <- c(issues, "Minimum group size must be at least 1")
  }
  
  if (PLOT_SETTINGS$figure_width <= 0 || PLOT_SETTINGS$figure_height <= 0) {
    issues <- c(issues, "Figure dimensions must be positive")
  }
  
  if (length(issues) > 0) {
    warning("Configuration validation issues found:\n", paste(issues, collapse = "\n"))
    return(FALSE)
  }
  
  return(TRUE)
} 