# Browse Diagnostics Files Utility
if (!exists("TOOLS_OUTPUT_DIR", inherits = TRUE)) {
    source(here::here("scripts", "load_all.R"))
}

# Usage examples:
# Rscript scripts/tools/browse_diagnostics.R
# Rscript scripts/tools/browse_diagnostics.R --pattern recurrence --max-rows 8
# Rscript scripts/tools/browse_diagnostics.R --list-only
# Rscript scripts/tools/browse_diagnostics.R --dir /absolute/path/to/dir

suppressWarnings(suppressMessages({
    # Load project environment and packages
    if (!requireNamespace("here", quietly = TRUE)) {
        stop("Package 'here' is required. Please install it.")
    }
}))

suppressWarnings(suppressMessages({
    if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required. Please install it.")
    }
    if (!requireNamespace("fs", quietly = TRUE)) {
        stop("Package 'fs' is required. Please install it.")
    }
    if (!requireNamespace("cli", quietly = TRUE)) {
        stop("Package 'cli' is required. Please install it.")
    }
}))

# Default configuration
get_default_output_dir <- function() {
    # Prefer globally defined OUTPUT_DIR if present
    if (exists("OUTPUT_DIR", inherits = TRUE)) {
        return(get("OUTPUT_DIR", inherits = TRUE))
    }
    # Fallback to runtime analysis location
    if (exists("RUNTIME_ROOT", inherits = TRUE)) {
        return(file.path(get("RUNTIME_ROOT", inherits = TRUE), "Analysis"))
    }
    return(file.path(path.expand("~/ProjectsRuntime"), basename(here::here()), "Analysis"))
}

# Simple CLI args parser
parse_args <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    opts <- list(pattern = NULL, max_rows = 10L, list_only = FALSE, dir = NULL)
    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]
        if (arg == "--pattern" && i < length(args)) {
            opts$pattern <- args[[i + 1L]]; i <- i + 1L
        } else if (grepl("^--max-rows=", arg)) {
            val <- sub("^--max-rows=", "", arg)
            opts$max_rows <- suppressWarnings(as.integer(val))
        } else if (arg == "--max-rows" && i < length(args)) {
            opts$max_rows <- suppressWarnings(as.integer(args[[i + 1L]])); i <- i + 1L
        } else if (arg == "--list-only") {
            opts$list_only <- TRUE
        } else if (arg == "--dir" && i < length(args)) {
            opts$dir <- args[[i + 1L]]; i <- i + 1L
        } else if (grepl("^--dir=", arg)) {
            opts$dir <- sub("^--dir=", "", arg)
        }
        i <- i + 1L
    }
    if (is.na(opts$max_rows) || is.null(opts$max_rows) || opts$max_rows < 1L) opts$max_rows <- 10L
    opts
}

# Find diagnostics files recursively under a base directory
find_diagnostics_files <- function(base_dir, pattern = NULL) {
    # Only search within the analysis output directory; ignore test_output per request
    all_files <- fs::dir_ls(base_dir, recurse = TRUE, type = "file", glob = "*_*diagnostics.xlsx")
    # Additional pattern filter if provided
    if (!is.null(pattern) && nzchar(pattern)) {
        keep <- grepl(pattern, all_files, ignore.case = TRUE)
        all_files <- all_files[keep]
    }
    # Ensure uniqueness and stable ordering
    unique(sort(all_files))
}

# Pretty print a single workbook summary and optional preview
preview_workbook <- function(path, max_rows = 10L, list_only = FALSE) {
    cli::cli_rule(left = basename(path), right = fs::path_rel(path, start = getwd()))
    # Basic file info
    info <- fs::file_info(path)
    cli::cli_text("Size: {round(as.numeric(info$size) / 1024, 1)} KB  |  Modified: {format(info$modification_time, '%Y-%m-%d %H:%M:%S')}")

    # List sheets
    sheets <- try(readxl::excel_sheets(path), silent = TRUE)
    if (inherits(sheets, "try-error")) {
        cli::cli_alert_danger("Failed to read sheets: {conditionMessage(attr(sheets, 'condition'))}")
        return(invisible(NULL))
    }
    cli::cli_text("Sheets: {paste(sheets, collapse = ', ')}")

    if (list_only) return(invisible(NULL))

    # Preview each sheet with first N rows
    for (sheet in sheets) {
        cli::cli_h3(paste0("Sheet: ", sheet))
        df <- try(suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = max_rows)), silent = TRUE)
        if (inherits(df, "try-error")) {
            cli::cli_alert_warning("Unable to preview sheet '{sheet}': {conditionMessage(attr(df, 'condition'))}")
            next
        }
        # Print as tibble for nicer formatting
        if (!inherits(df, "tbl_df")) {
            df <- tibble::as_tibble(df)
        }
        print(df, n = nrow(df), width = Inf)
        cat("\n")
    }
}

# Main execution
main <- function() {
    opts <- parse_args()
    base_dir <- if (!is.null(opts$dir) && nzchar(opts$dir)) opts$dir else get_default_output_dir()

    if (!fs::dir_exists(base_dir)) {
        stop(sprintf("Base directory does not exist: %s", base_dir))
    }

    cli::cli_inform(c(
        i = sprintf("Scanning for diagnostics under: %s", base_dir)
    ))

    files <- find_diagnostics_files(base_dir, pattern = opts$pattern)

    if (length(files) == 0) {
        cli::cli_alert_info("No diagnostics files found matching criteria.")
        return(invisible(NULL))
    }

    cli::cli_inform(c(
        v = sprintf("Found %d diagnostics file(s).", length(files))
    ))

    for (path in files) {
        preview_workbook(path, max_rows = opts$max_rows, list_only = opts$list_only)
    }

    invisible(NULL)
}

if (sys.nframe() == 0L) {
    main()
}
