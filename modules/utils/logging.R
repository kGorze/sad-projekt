# Analysis progress and error logging
# Analysis tracking and console output logging

# Global logging variables
.log_data <- new.env()
.log_data$log_file <- NULL
.log_data$log_level <- "INFO"
.log_data$session_start <- NULL
.log_data$steps <- list()
.log_data$warnings <- list()
.log_data$errors <- list()
.log_data$console_output <- list()
.log_data$r_warnings <- list()

# Initialize logging system
init_logging <- function(log_file = NULL, log_level = "INFO") {
  
  # Create logs directory if it doesn't exist
  if (!dir.exists("output/logs")) {
    dir.create("output/logs", recursive = TRUE)
  }
  
  # Generate timestamp-based log file name if not provided
  if (is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path("output/logs", paste0("analysis_log_", timestamp, ".log"))
  }
  
  # Initialize logging session
  .log_data$log_file <- log_file
  .log_data$log_level <- log_level
  .log_data$session_start <- Sys.time()
  .log_data$steps <- list()
  .log_data$warnings <- list()
  .log_data$errors <- list()
  .log_data$console_output <- list()
  .log_data$r_warnings <- list()
  .log_data$original_warning_handler <- NULL
  
  # Store old warning option for restoration
  .log_data$old_warn <- getOption("warn")
  
  # Enable automatic warning capture
  enable_warning_capture()
  
  # Write session header
  write_log_header()
  
  cat("Logging initialized. Log file:", log_file, "\n")
  return(log_file)
}

# Capture R warnings directly
log_r_warning_direct <- function(warning_msg) {
  if (!is.null(warning_msg) && nchar(warning_msg) > 0) {
    timestamp <- Sys.time()
    warning_info <- list(
      timestamp = timestamp,
      message = as.character(warning_msg),
      call = "R Warning"
    )
    
    .log_data$r_warnings[[length(.log_data$r_warnings) + 1]] <- warning_info
    
    # Also write to log file immediately
    if (!is.null(.log_data$log_file)) {
      log_line <- paste0(
        "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
        "[R_WARNING] ",
        as.character(warning_msg)
      )
      write(log_line, file = .log_data$log_file, append = TRUE)
    }
  }
}

# Write log header with session information
write_log_header <- function() {
  if (is.null(.log_data$log_file)) {
    return()
  }
  
  header_lines <- c(
    "================================================================================",
    paste("R STATISTICAL ANALYSIS SESSION LOG"),
    paste("Session started:", .log_data$session_start),
    paste("R Version:", R.version.string),
    paste("Working directory:", getwd()),
    paste("Command line args:", paste(commandArgs(trailingOnly = TRUE), collapse = " ")),
    paste("Log level:", .log_data$log_level),
    "================================================================================",
    ""
  )
  
  writeLines(header_lines, .log_data$log_file)
}

# Console output capture
capture_console_output <- function(output_text, level = "CONSOLE") {
  timestamp <- Sys.time()
  
  # Store console output
  console_entry <- list(
    timestamp = timestamp,
    level = level,
    content = output_text
  )
  
  .log_data$console_output[[length(.log_data$console_output) + 1]] <- console_entry
  
  # Write to log file immediately
  if (!is.null(.log_data$log_file)) {
    if (is.character(output_text) && length(output_text) > 0) {
      for (line in output_text) {
        if (nchar(trimws(line)) > 0) {  # Skip empty lines
          log_line <- paste0(
            "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
            "[", level, "] ",
            line
          )
          write(log_line, file = .log_data$log_file, append = TRUE)
        }
      }
    }
  }
}

# Log analysis steps
log_analysis_step <- function(step_name, details = NULL, level = "INFO") {
  
  timestamp <- Sys.time()
  
  # Store step information
  step_info <- list(
    timestamp = timestamp,
    step_name = step_name,
    details = details,
    level = level
  )
  
  .log_data$steps[[length(.log_data$steps) + 1]] <- step_info
  
  # Write to log file
  if (!is.null(.log_data$log_file)) {
    log_line <- paste0(
      "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
      "[", level, "] ",
      "STEP: ", step_name
    )
    
    if (!is.null(details)) {
      log_line <- paste0(log_line, " - ", details)
    }
    
    write(log_line, file = .log_data$log_file, append = TRUE)
  }
  
  # Also print to console with formatting
  cat(paste0("=== ", step_name, " ===\n"))
  if (!is.null(details)) {
    cat(paste0("Details: ", details, "\n"))
  }
}

# Log data quality issues
log_data_quality <- function(issue_type, details, severity = "WARNING") {
  
  timestamp <- Sys.time()
  
  # Store data quality issue
  quality_issue <- list(
    timestamp = timestamp,
    issue_type = issue_type,
    details = details,
    severity = severity
  )
  
  .log_data$warnings[[length(.log_data$warnings) + 1]] <- quality_issue
  
  # Write to log file
  if (!is.null(.log_data$log_file)) {
    log_line <- paste0(
      "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
      "[", severity, "] ",
      "DATA QUALITY: ", issue_type, " - ", details
    )
    
    write(log_line, file = .log_data$log_file, append = TRUE)
  }
  
  # Also print to console
  cat(paste0("⚠ DATA QUALITY ", severity, ": ", issue_type, " - ", details, "\n"))
}

# Log statistical test results
log_statistical_results <- function(test_name, results, interpretation = NULL) {
  
  timestamp <- Sys.time()
  
  # Store statistical result
  stat_result <- list(
    timestamp = timestamp,
    test_name = test_name,
    results = results,
    interpretation = interpretation
  )
  
  .log_data$steps[[length(.log_data$steps) + 1]] <- stat_result
  
  # Write to log file
  if (!is.null(.log_data$log_file)) {
    log_line <- paste0(
      "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
      "[INFO] ",
      "STATISTICAL TEST: ", test_name
    )
    
    if (!is.null(results) && is.list(results)) {
      if (!is.null(results$p.value)) {
        log_line <- paste0(log_line, " - p-value: ", results$p.value)
      }
      if (!is.null(results$statistic)) {
        log_line <- paste0(log_line, " - statistic: ", results$statistic)
      }
    }
    
    if (!is.null(interpretation)) {
      log_line <- paste0(log_line, " - ", interpretation)
    }
    
    write(log_line, file = .log_data$log_file, append = TRUE)
  }
  
  # Also print to console
  cat(paste0("📊 Statistical Test: ", test_name, "\n"))
  if (!is.null(interpretation)) {
    cat(paste0("   Interpretation: ", interpretation, "\n"))
  }
}

# Log warnings and errors
log_warning <- function(message, context = NULL) {
  
  timestamp <- Sys.time()
  
  # Store warning
  warning_info <- list(
    timestamp = timestamp,
    message = message,
    context = context
  )
  
  .log_data$warnings[[length(.log_data$warnings) + 1]] <- warning_info
  
  # Write to log file
  if (!is.null(.log_data$log_file)) {
    log_line <- paste0(
      "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
      "[WARNING] ",
      message
    )
    
    if (!is.null(context)) {
      log_line <- paste0(log_line, " - Context: ", context)
    }
    
    write(log_line, file = .log_data$log_file, append = TRUE)
  }
  
  # Also print to console
  cat(paste0("⚠ WARNING: ", message, "\n"))
  if (!is.null(context)) {
    cat(paste0("   Context: ", context, "\n"))
  }
}

log_error <- function(message, context = NULL) {
  
  timestamp <- Sys.time()
  
  # Store error
  error_info <- list(
    timestamp = timestamp,
    message = message,
    context = context
  )
  
  .log_data$errors[[length(.log_data$errors) + 1]] <- error_info
  
  # Write to log file
  if (!is.null(.log_data$log_file)) {
    log_line <- paste0(
      "[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ",
      "[ERROR] ",
      message
    )
    
    if (!is.null(context)) {
      log_line <- paste0(log_line, " - Context: ", context)
    }
    
    write(log_line, file = .log_data$log_file, append = TRUE)
  }
  
  # Also print to console
  cat(paste0("❌ ERROR: ", message, "\n"))
  if (!is.null(context)) {
    cat(paste0("   Context: ", context, "\n"))
  }
}

# Console output capture with real-time logging
capture_and_log_expression <- function(expr, description = "Operation") {
  
  # Create temporary files for capturing output and messages
  temp_output <- tempfile()
  temp_messages <- tempfile()
  
  result <- tryCatch({
    
    # Redirect both output and messages
    output_conn <- file(temp_output, "w")
    messages_conn <- file(temp_messages, "w")
    
    sink(output_conn, type = "output")
    sink(messages_conn, type = "message")
    
    # Execute the expression
    eval_result <- eval(expr, envir = parent.frame())
    
    # Close sinks
    sink(type = "output")
    sink(type = "message")
    
    close(output_conn)
    close(messages_conn)
    
    # Read captured output
    if (file.exists(temp_output)) {
      captured_output <- readLines(temp_output, warn = FALSE)
      if (length(captured_output) > 0) {
        capture_console_output(captured_output, "STDOUT")
      }
    }
    
    # Read captured messages/warnings
    if (file.exists(temp_messages)) {
      captured_messages <- readLines(temp_messages, warn = FALSE)
      if (length(captured_messages) > 0) {
        capture_console_output(captured_messages, "STDERR")
      }
    }
    
    # Clean up
    if (file.exists(temp_output)) unlink(temp_output)
    if (file.exists(temp_messages)) unlink(temp_messages)
    
    return(eval_result)
    
  }, warning = function(w) {
    # Make sure sinks are closed
    sink(type = "output")
    sink(type = "message")
    
    # Log the warning
    log_warning(paste("Warning in", description, ":", w$message), "Expression execution")
    
    # Clean up
    if (file.exists(temp_output)) unlink(temp_output)
    if (file.exists(temp_messages)) unlink(temp_messages)
    
    # Re-throw the warning
    warning(w)
    
  }, error = function(e) {
    # Make sure sinks are closed
    sink(type = "output")
    sink(type = "message")
    
    # Log the error
    log_error(paste("Error in", description, ":", e$message), "Expression execution")
    
    # Clean up
    if (file.exists(temp_output)) unlink(temp_output)
    if (file.exists(temp_messages)) unlink(temp_messages)
    
    stop(e)
  })
  
  return(result)
}

# Warning capture management
enable_warning_capture <- function() {
  # Store the original warning option
  .log_data$old_warn <- getOption("warn")
  
  # Set warnings to be shown immediately (not accumulated)
  options(warn = 1)
  
  cat("Real-time warning display enabled for logging.\n")
}

# Disable warning capture
disable_warning_capture <- function() {
  # Restore original warn option
  if (!is.null(.log_data$old_warn)) {
    options(warn = .log_data$old_warn)
  }
  
  cat("Warning display restored.\n")
}

# Alternative approach: Wrap execution with warning monitoring
execute_with_warning_capture <- function(expr) {
  
  # Set up warning collection
  old_warn <- getOption("warn")
  options(warn = 1)  # Show warnings immediately
  
  # Set up a custom warning handler
  warning_count <- 0
  
  # Execute with warning capture
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      warning_count <<- warning_count + 1
      log_r_warning_direct(conditionMessage(w))
      # Let the warning continue to be displayed normally
    }
  )
  
  # Restore warning option
  options(warn = old_warn)
  
  if (warning_count > 0) {
    cat(paste("Captured", warning_count, "warnings during execution.\n"))
  }
  
  return(result)
}

# Generate analysis summary log
generate_summary_log <- function() {
  
  if (is.null(.log_data$log_file)) {
    return()
  }
  
  # Calculate session duration
  if (!is.null(.log_data$session_start)) {
    session_duration <- as.numeric(difftime(Sys.time(), .log_data$session_start, units = "mins"))
  } else {
    session_duration <- NA
  }
  
  # Create detailed summary
  summary_lines <- c(
    "",
    "================================================================================",
    "SESSION SUMMARY WITH DEBUGGING INFORMATION",
    "================================================================================",
    paste("Session duration:", round(session_duration, 2), "minutes"),
    paste("Total analysis steps:", length(.log_data$steps)),
    paste("Total custom warnings:", length(.log_data$warnings)),
    paste("Total custom errors:", length(.log_data$errors)),
    paste("Total R warnings captured:", length(.log_data$r_warnings)),
    paste("Total console output entries:", length(.log_data$console_output)),
    paste("Session ended:", Sys.time()),
    ""
  )
  
  # Add step summary
  if (length(.log_data$steps) > 0) {
    summary_lines <- c(summary_lines, "ANALYSIS STEPS COMPLETED:")
    for (i in seq_along(.log_data$steps)) {
      step <- .log_data$steps[[i]]
      summary_lines <- c(summary_lines, 
                        paste0("  ", i, ". ", step$step_name, 
                              " (", format(step$timestamp, "%H:%M:%S"), ")"))
    }
    summary_lines <- c(summary_lines, "")
  }
  
  # Add R warnings summary (the real debugging gold!)
  if (length(.log_data$r_warnings) > 0) {
    summary_lines <- c(summary_lines, "R WARNINGS ENCOUNTERED (DEBUGGING INFO):")
    warning_counts <- table(sapply(.log_data$r_warnings, function(x) x$message))
    warning_counts <- sort(warning_counts, decreasing = TRUE)
    
    for (i in seq_along(warning_counts)) {
      warning_msg <- names(warning_counts)[i]
      count <- warning_counts[i]
      summary_lines <- c(summary_lines, 
                        paste0("  ", count, "x: ", warning_msg))
    }
    summary_lines <- c(summary_lines, "")
  }
  
  # Add custom warnings summary
  if (length(.log_data$warnings) > 0) {
    summary_lines <- c(summary_lines, "CUSTOM WARNINGS LOGGED:")
    for (i in seq_along(.log_data$warnings)) {
      warning <- .log_data$warnings[[i]]
      summary_lines <- c(summary_lines, 
                        paste0("  ", i, ". ", warning$message,
                              " (", format(warning$timestamp, "%H:%M:%S"), ")"))
    }
    summary_lines <- c(summary_lines, "")
  }
  
  # Add errors summary
  if (length(.log_data$errors) > 0) {
    summary_lines <- c(summary_lines, "ERRORS ENCOUNTERED:")
    for (i in seq_along(.log_data$errors)) {
      error <- .log_data$errors[[i]]
      summary_lines <- c(summary_lines, 
                        paste0("  ", i, ". ", error$message,
                              " (", format(error$timestamp, "%H:%M:%S"), ")"))
    }
    summary_lines <- c(summary_lines, "")
  }
  
  # Add debugging recommendations
  summary_lines <- c(summary_lines, "DEBUGGING RECOMMENDATIONS:")
  if (length(.log_data$r_warnings) > 50) {
    summary_lines <- c(summary_lines, "  ⚠ High number of R warnings - investigate data quality")
  }
  if (length(.log_data$r_warnings) == 0) {
    summary_lines <- c(summary_lines, "  ✅ No R warnings - clean execution")
  }
  if (session_duration > 5) {
    summary_lines <- c(summary_lines, "  ⏱ Long execution time - consider performance optimization")
  }
  
  summary_lines <- c(summary_lines, "", "================================================================================")
  
  # Write summary to log file
  write(summary_lines, file = .log_data$log_file, append = TRUE)
  
  # Also print key summary to console
  cat("\n=== SESSION SUMMARY ===\n")
  cat(paste("Analysis completed in", round(session_duration, 2), "minutes\n"))
  cat(paste("Steps completed:", length(.log_data$steps), "\n"))
  cat(paste("R warnings captured:", length(.log_data$r_warnings), "\n"))
  cat(paste("Custom warnings:", length(.log_data$warnings), "\n"))
  cat(paste("Errors:", length(.log_data$errors), "\n"))
  cat(paste("Log file saved:", .log_data$log_file, "\n"))
}

# Export log to file (alias for backward compatibility)
export_log <- function(output_path = NULL) {
  
  if (is.null(output_path)) {
    if (!is.null(.log_data$log_file)) {
      cat("Log already saved to:", .log_data$log_file, "\n")
      return(.log_data$log_file)
    } else {
      cat("No active log session.\n")
      return(NULL)
    }
  }
  
  # Copy current log to new location
  if (!is.null(.log_data$log_file) && file.exists(.log_data$log_file)) {
    file.copy(.log_data$log_file, output_path, overwrite = TRUE)
    cat("Log exported to:", output_path, "\n")
    return(output_path)
  } else {
    cat("No log file to export.\n")
    return(NULL)
  }
}

# Log file access
get_log_file <- function() {
  return(.log_data$log_file)
}

# R warning capture
capture_accumulated_warnings <- function() {
  # Try to capture any accumulated warnings
  tryCatch({
    w <- warnings()
    if (length(w) > 0) {
      for (i in seq_along(w)) {
        warning_msg <- names(w)[i]
        if (!is.null(warning_msg) && nchar(warning_msg) > 0) {
          log_r_warning_direct(warning_msg)
        }
      }
    }
  }, error = function(e) {
    # Silently ignore errors when trying to capture warnings
  })
}

# Session termination
close_logging <- function() {
  
  # Disable warning capture
  disable_warning_capture()
  
  # Capture any accumulated warnings before closing
  capture_accumulated_warnings()
  
  # Generate final summary
  generate_summary_log()
  
  # Reset logging data
  .log_data$log_file <- NULL
  .log_data$session_start <- NULL
  .log_data$steps <- list()
  .log_data$warnings <- list()
  .log_data$errors <- list()
  .log_data$console_output <- list()
  .log_data$r_warnings <- list()
  .log_data$original_warning_handler <- NULL
  
      cat("Logging session closed.\n")
} 