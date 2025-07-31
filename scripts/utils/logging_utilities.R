# Logging Utilities
# Author: Nicholas Camarda
# Description: Enhanced logging functions for the analysis pipeline

# =============================================================================
# LOGGING FUNCTIONS
# =============================================================================

#' Log a message with timestamp and optional formatting
#'
#' @param msg Message to log
#' @param level Log level ("INFO", "WARN", "ERROR", "PROGRESS", "SECTION")
#' @param indent Number of spaces to indent (default: 0)
log_enhanced <- function(msg, level = "INFO", indent = 0) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    indent_str <- paste(rep("  ", indent), collapse = "")
    
    # Format based on level
    formatted_msg <- switch(level,
        "SECTION" = sprintf("\n%s[%s] === %s ===\n", indent_str, timestamp, msg),
        "PROGRESS" = sprintf("%s[%s] >>> %s", indent_str, timestamp, msg),
        "INFO" = sprintf("%s[%s] %s", indent_str, timestamp, msg),
        "WARN" = sprintf("%s[%s] WARNING: %s", indent_str, timestamp, msg),
        "ERROR" = sprintf("%s[%s] ERROR: %s", indent_str, timestamp, msg),
        sprintf("%s[%s] %s", indent_str, timestamp, msg)  # default
    )
    
    message(formatted_msg)
}

#' Log progress through a list of items
#'
#' @param current Current item number
#' @param total Total number of items
#' @param item_name Name of current item
#' @param action Action being performed
log_progress <- function(current, total, item_name = NULL, action = "Processing") {
    progress_pct <- round(100 * current / total, 1)
    base_msg <- sprintf("%s (%d/%d - %.1f%%)", action, current, total, progress_pct)
    
    if (!is.null(item_name)) {
        full_msg <- sprintf("%s: %s", base_msg, item_name)
    } else {
        full_msg <- base_msg
    }
    
    log_enhanced(full_msg, level = "PROGRESS")
}

#' Log start of a major analysis section
#'
#' @param section_name Name of the analysis section
#' @param detail_name Optional detail for the section
log_section_start <- function(section_name, detail_name = NULL) {
    if (!is.null(detail_name)) {
        full_name <- sprintf("%s - %s", section_name, detail_name)
    } else {
        full_name <- section_name
    }
    log_enhanced(full_name, level = "SECTION")
}

#' Log completion of a major analysis section with timing
#'
#' @param section_name Name of the analysis section
#' @param start_time Start time from Sys.time()
log_section_complete <- function(section_name, start_time) {
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    log_enhanced(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)", section_name, duration), level = "PROGRESS")
}

#' Log a function call with its purpose
#'
#' @param func_name Name of the function being called
#' @param purpose Description of what the function does
log_function <- function(func_name, purpose) {
    log_enhanced(sprintf("Executing %s: %s", func_name, purpose), level = "INFO", indent = 1)
} 