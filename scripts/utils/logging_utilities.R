# Logging Utilities
# Author: Nicholas Camarda
# Description: Enhanced logging functions for the analysis pipeline

# =============================================================================
# LOGGING FUNCTIONS
# =============================================================================

#' Small formatter for consistent indentation and optional prefixes
#' @param msg character
#' @param indent integer number of two-space indents
#' @param prefix character optional prefix like ">>> "
#' @return character
formatted <- function(msg, indent = 0, prefix = "") {
    indent_str <- paste(rep("  ", indent), collapse = "")
    paste0(indent_str, prefix, msg)
}

#' Initialize logging and progress handlers
#'
#' Sets up logger (console and optional file) and configures progressr handlers.
#' Also configures compact contextual tags for console lines.
#'
#' @param log_path optional file path for a run log; if NULL, console only
#' @param level character log threshold (e.g., "INFO", "WARN", "ERROR")
#' @param progress logical; enable progress bars (default: interactive())
#' @param quiet_html logical; avoid printing HTML artifacts to console
#' @param context_in_console logical; prepend compact tags in console
#' @param context_compact logical; abbreviate tags
#' @param context_max_width integer; cap tag width in console
#' @param context_in_file logical; include context fields in JSON file logs
#' @return invisible(TRUE)
setup_logging <- function(
    log_path = NULL,
    level = "INFO",
    progress = interactive(),
    quiet_html = TRUE,
    context_in_console = TRUE,
    context_compact = TRUE,
    context_max_width = 40,
    context_in_file = TRUE
) {
    # Configure HTML quieting (best-effort)
    if ((quiet_html)) {
        options(gt.html_print = FALSE)
    }

    # Persist context display prefs in options for layout to read
    options(.um_context_compact = context_compact, .um_context_max_width = context_max_width)

    # Console layout with compact tags and level; store last level for JSON
    console_layout <- function(level, msg, namespace = NA, .logcall = NULL, .topcall = NULL, .topenv = NULL) {
        options(.um_last_level = as.character(level))
        ts <- format(Sys.time(), '%H:%M:%S')
        lvl_num <- as.character(level)
        lvl_map <- c("100" = "DEBUG", "200" = "ERROR", "300" = "WARN", "400" = "INFO")
        lvl_txt <- if (!is.na(lvl_map[[lvl_num]])) lvl_map[[lvl_num]] else lvl_num
        if ((context_in_console)) {
            paste0("[", ts, "] [", lvl_txt, "] ", format_log_context(
                compact = getOption('.um_context_compact', TRUE),
                max_width = getOption('.um_context_max_width', 40)
            ), msg)
        } else {
            paste0("[", ts, "] [", lvl_txt, "] ", msg)
        }
    }

    # Custom JSON layout for file logs including context fields
    json_file_layout <- function(level, msg, namespace = NA, .logcall = NULL, .topcall = NULL, .topenv = NULL) {
        ctx <- getOption(".um_log_context", default = list())
        ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
        # Safely extract context
        getc <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else as.character(x)
        entry <- list(
            timestamp = ts,
            level = as.character(level),
            message = as.character(msg),
            cohort = getc(ctx$cohort),
            objective = getc(ctx$objective),
            subobjective = getc(ctx$subobjective)
        )
        jsonlite::toJSON(entry, auto_unbox = TRUE)
    }

    # Idempotence guard: avoid re-initializing to the same configuration
    prev <- getOption(".um_logger_state", NULL)
    state <- list(
        log_path = if (is.null(log_path)) "" else normalizePath(log_path, mustWork = FALSE),
        level = level,
        context_in_console = context_in_console,
        context_in_file = context_in_file
    )
    if (!is.null(prev) && identical(prev, state)) {
        # Still update threshold and progress setting, then return
        logger::log_threshold(level)
        if ((progress)) {
            progressr::handlers(global = TRUE)
            options(progressr.enable = TRUE)
        } else {
            options(progressr.enable = FALSE)
        }
        return(invisible(TRUE))
    }

    # Apply console layout
    logger::log_layout(console_layout)

    # Build appender: console only or tee using custom function to format file lines
    if (!is.null(log_path) && nzchar(log_path)) {
        dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
        # Determine destinations for text and JSON logs
        ext <- tolower(tools::file_ext(log_path))
        is_txt <- identical(ext, "txt")
        if (is_txt) {
            base_dir <- dirname(log_path)
            base_name <- tools::file_path_sans_ext(basename(log_path))
            # Human-readable under txt/
            txt_dir <- file.path(base_dir, "txt")
            dir.create(txt_dir, recursive = TRUE, showWarnings = FALSE)
            text_path <- file.path(txt_dir, paste0(base_name, ".txt"))
            # JSON lines under json/
            json_dir <- file.path(base_dir, "json")
            dir.create(json_dir, recursive = TRUE, showWarnings = FALSE)
            json_path <- file.path(json_dir, paste0(base_name, ".jsonl"))
        } else {
            # Back-compat: non-.txt path receives JSON, plus create sibling human-readable text
            json_path <- log_path
            txt_dir <- file.path(dirname(log_path), "txt")
            dir.create(txt_dir, recursive = TRUE, showWarnings = FALSE)
            text_path <- file.path(
                txt_dir,
                paste0(tools::file_path_sans_ext(basename(log_path)), ".txt")
            )
        }

        text_con <- try(file(text_path, open = "a"), silent = TRUE)
        if (inherits(text_con, "try-error")) text_con <- NULL
        json_con <- try(file(json_path, open = "a"), silent = TRUE)
        if (inherits(json_con, "try-error")) json_con <- NULL

        # Custom appender that writes console via current appender, mirrors to text log, and writes JSON
        appender_dual <- function(line) {
            # Compose JSON entry using captured level and context
            ctx <- getOption(".um_log_context", default = list())
            ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
            safe_val <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else as.character(x)
            entry <- if ((context_in_file)) {
                list(
                    timestamp = ts,
                    level_text = switch(as.character(getOption(".um_last_level", "400")), "100" = "DEBUG", "200" = "ERROR", "300" = "WARN", "400" = "INFO", as.character(getOption(".um_last_level", "INFO"))),
                    level_num = as.integer(getOption(".um_last_level", 400)),
                    message = line,
                    cohort = safe_val(ctx$cohort),
                    objective = safe_val(ctx$objective),
                    subobjective = safe_val(ctx$subobjective)
                )
            } else {
                list(
                    timestamp = ts,
                    level_text = switch(as.character(getOption(".um_last_level", "400")), "100" = "DEBUG", "200" = "ERROR", "300" = "WARN", "400" = "INFO", as.character(getOption(".um_last_level", "INFO"))),
                    level_num = as.integer(getOption(".um_last_level", 400)),
                    message = line
                )
            }
            # Write JSON and text
            if (!is.null(json_con)) {
                writeLines(jsonlite::toJSON(entry, auto_unbox = TRUE), con = json_con)
                flush(json_con)
            }
            if (!is.null(text_con)) {
                writeLines(line, con = text_con)
                flush(text_con)
            }
            # Emit to console
            cat(line, sep = "\n")
        }
        logger::log_appender(appender_dual)
        # Ensure connections close at exit
        reg.finalizer(environment(), function(e) {
            try(close(text_con), silent = TRUE)
            try(close(json_con), silent = TRUE)
        }, onexit = TRUE)
    } else {
        logger::log_appender(logger::appender_console)
    }

    # Configure threshold
    logger::log_threshold(level)

    # Configure progress handlers
    if ((progress)) {
        progressr::handlers(global = TRUE)
        options(progressr.enable = TRUE)
    } else {
        options(progressr.enable = FALSE)
    }

    # Save current logger state for idempotence
    options(.um_logger_state = state)

    invisible(TRUE)
}

#' Format compact context tags for console lines
#'
#' Uses values set via set_log_context()/with_log_context().
#' Returns a string like "[full] [obj1] [OS] " or empty string if no context.
#'
#' @param compact logical abbreviate values
#' @param max_width integer maximum width for the tag block
format_log_context <- function(compact = TRUE, max_width = 40) {
    ctx <- getOption(".um_log_context", default = list())
    if (length(ctx) == 0) return("")

    abbr <- function(key, val) {
        if (!compact || is.null(val) || !nzchar(val)) return(val)
        switch(key,
            cohort = switch(val, uveal_melanoma_full_cohort = "full", uveal_melanoma_restricted_cohort = "restricted", uveal_melanoma_gksrs_only_cohort = "gksrs", val),
            objective = gsub("^objective_", "obj", val),
            subobjective = switch(val,
                c_overall_survival = "OS",
                d_progression_free_survival = "PFS",
                e_tumor_height_primary = "TH1",
                f_tumor_height_sensitivity = "TH2",
                val
            ),
            val
        )
    }

    keys <- c("cohort", "objective", "subobjective")
    parts <- character(0)
    for (k in keys) {
        v <- ctx[[k]]
        if (!is.null(v) && nzchar(v)) {
            v2 <- abbr(k, v)
            parts <- c(parts, sprintf("[%s]", v2))
        }
    }
    if (length(parts) == 0) return("")
    tag_block <- paste0(paste(parts, collapse = " "), " ")
    if (!is.null(max_width) && nchar(tag_block) > max_width) {
        tag_block <- paste0(substr(tag_block, 1, max_width - 1), " ")
    }
    tag_block
}

#' Set or update global log context
#'
#' @param cohort Character value for the cohort context. With `replace = FALSE`,
#'   `NULL` leaves the current field unchanged. With `replace = TRUE`, `NULL`
#'   removes the field from the context.
#' @param objective Character value for the objective context. With
#'   `replace = FALSE`, `NULL` leaves the current field unchanged. With
#'   `replace = TRUE`, `NULL` removes the field from the context.
#' @param subobjective Character value for the subobjective context. With
#'   `replace = FALSE`, `NULL` leaves the current field unchanged. With
#'   `replace = TRUE`, `NULL` removes the field from the context.
#' @param replace Logical; if `TRUE`, rebuild the context from the supplied
#'   arguments instead of updating the current context in place.
#' @return Invisibly returns the updated context list.
set_log_context <- function(cohort = NULL, objective = NULL, subobjective = NULL, replace = FALSE) {
    ctx <- if (isTRUE(replace)) {
        list()
    } else {
        getOption(".um_log_context", default = list())
    }

    if (!is.null(cohort)) {
        ctx$cohort <- cohort
    } else if (isTRUE(replace)) {
        ctx$cohort <- NULL
    }

    if (!is.null(objective)) {
        ctx$objective <- objective
    } else if (isTRUE(replace)) {
        ctx$objective <- NULL
    }

    if (!is.null(subobjective)) {
        ctx$subobjective <- subobjective
    } else if (isTRUE(replace)) {
        ctx$subobjective <- NULL
    }

    options(.um_log_context = ctx)
    invisible(ctx)
}

#' Evaluate an expression with a temporary log context
#'
#' Restores the previous context on exit.
#'
#' @param cohort Character value for the cohort context.
#' @param objective Character value for the objective context.
#' @param subobjective Character value for the subobjective context.
#' @param replace Logical; if `TRUE`, replace the existing context for the
#'   duration of the expression instead of updating it in place.
#' @param expr Expression to evaluate with the temporary context.
#' @return Invisibly returns the evaluated expression result.
with_log_context <- function(cohort = NULL, objective = NULL, subobjective = NULL, replace = FALSE, expr) {
    old <- getOption(".um_log_context", default = list())
    on.exit(options(.um_log_context = old), add = TRUE)
    set_log_context(
        cohort = cohort,
        objective = objective,
        subobjective = subobjective,
        replace = replace
    )
    result <- force(expr)
    invisible(result)
}

#' Emit a prominent phase banner in logs
#' @param title Character title for the phase
#' @return invisible(TRUE)
log_phase <- function(title) {
    logger::log_info(sprintf("=== %s ===", title))
    invisible(TRUE)
}
