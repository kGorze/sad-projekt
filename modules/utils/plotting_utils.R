# Graphics environment management

ensure_clean_graphics_environment <- function() {
  # Close any open graphics devices
  if (length(dev.list()) > 0) {
    graphics.off()
  }
  
  # Remove any existing Rplots.pdf file
  if (file.exists("Rplots.pdf")) {
    tryCatch({
      file.remove("Rplots.pdf")
    }, error = function(e) {
      # Silently ignore if file cannot be removed (might be in use)
    })
  }
}

# Safe plot saving
safe_ggsave <- function(filename, plot, width = 10, height = 8, dpi = 300, ...) {
  # Ensure clean environment before saving
  ensure_clean_graphics_environment()
  
  # Create directory if it doesn't exist
  plot_dir <- dirname(filename)
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # Save the plot
  tryCatch({
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      ...
    )
    return(filename)
  }, error = function(e) {
    cat("Warning: Could not save plot to", filename, ":", e$message, "\n")
    return(NULL)
  })
}

# Plot creation and saving
safe_plot_creation <- function(plot_creation_function, filename, width = 10, height = 8, dpi = 300) {
  # Ensure clean environment
  ensure_clean_graphics_environment()
  
  # Create the plot
  plot_obj <- tryCatch({
    plot_creation_function()
  }, error = function(e) {
    cat("Error creating plot:", e$message, "\n")
    return(NULL)
  })
  
  # Save if successful
  if (!is.null(plot_obj)) {
    saved_file <- safe_ggsave(filename, plot_obj, width, height, dpi)
    
    # Clean up afterwards
    ensure_clean_graphics_environment()
    
    return(list(plot = plot_obj, file = saved_file))
  }
  
  return(NULL)
}

# Graphics cleanup
cleanup_graphics_after_analysis <- function() {
  # Close any remaining graphics devices
  if (length(dev.list()) > 0) {
    graphics.off()
  }
  
  # Remove Rplots.pdf if it exists
  if (file.exists("Rplots.pdf")) {
    tryCatch({
      file.remove("Rplots.pdf")
      cat("Cleaned up unwanted Rplots.pdf file\n")
    }, error = function(e) {
      cat("Note: Could not remove Rplots.pdf (may be in use)\n")
    })
  }
  
  # Force garbage collection to free up graphics resources
  gc()
}

# Graphics session initialization
init_graphics_session <- function() {
  # Ensure clean start
  ensure_clean_graphics_environment()
  
  # Set default theme for consistency
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_set(ggplot2::theme_minimal())
  }
  
  cat("Graphics session initialized with clean environment\n")
}

# Session cleanup
end_graphics_session <- function() {
  cleanup_graphics_after_analysis()
  cat("Graphics session ended cleanly\n")
} 