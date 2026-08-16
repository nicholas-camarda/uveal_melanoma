#' Find and validate the newest full analysis-run log
select_publish_analysis_log <- function() {
    log_dir <- file.path(LOGS_DIR, "txt")
    log_paths <- if (dir.exists(log_dir)) {
        list.files(
            log_dir,
            pattern = "^run_log_[0-9]{8}_[0-9]{6}\\.txt$",
            full.names = TRUE
        )
    } else {
        character()
    }

    current_log <- if (exists("log_file", inherits = TRUE)) {
        normalizePath(
            file.path(log_dir, basename(get("log_file", inherits = TRUE))),
            winslash = "/",
            mustWork = FALSE
        )
    } else {
        character()
    }
    log_paths <- setdiff(normalizePath(log_paths, winslash = "/", mustWork = FALSE), current_log)
    if (length(log_paths) == 0) {
        stop(sprintf("Cannot publish: no analysis logs found under %s", log_dir), call. = FALSE)
    }

    readable_logs <- lapply(log_paths, function(log_path) {
        tryCatch(readLines(log_path, warn = FALSE), error = function(e) NULL)
    })
    is_full_attempt <- vapply(
        readable_logs,
        function(lines) !is.null(lines) && any(grepl("=== MAIN EXECUTION PHASE ===", lines, fixed = TRUE)),
        logical(1)
    )
    full_logs <- log_paths[is_full_attempt]
    full_lines <- readable_logs[is_full_attempt]
    if (length(full_logs) == 0) {
        stop(sprintf("Cannot publish: no full analysis attempt found under %s", log_dir), call. = FALSE)
    }

    latest_index <- order(basename(full_logs), decreasing = TRUE)[[1]]
    latest_log <- full_logs[[latest_index]]
    log_lines <- full_lines[[latest_index]]
    error_lines <- grepl(
        "\\[ERROR\\]|\\bERROR\\b|\\bError in\\b|\\bExecution halted\\b|ANALYSES COMPLETED WITH ERRORS",
        log_lines,
        ignore.case = TRUE,
        perl = TRUE
    )
    completed <- any(grepl("COMPLETED MAIN EXECUTION PHASE", log_lines, fixed = TRUE))
    analyzed_three <- any(grepl("Datasets analyzed: 3", log_lines, fixed = TRUE))
    terminal_success <- any(grepl(
        "ALL ANALYSES COMPLETED SUCCESSFULLY|ANALYSES COMPLETED WITH WARNINGS",
        log_lines,
        perl = TRUE
    ))
    if (length(log_lines) == 0 || any(error_lines) || !completed || !analyzed_three || !terminal_success) {
        stop(
            paste(
                sprintf("Cannot publish: newest full analysis attempt failed or is incomplete: %s", latest_log),
                paste(head(log_lines[error_lines], 20L), collapse = "\n"),
                sep = "\n"
            ),
            call. = FALSE
        )
    }

    latest_log
}

#' Normalize a path for stable publish comparisons
#'
#' @param path Character scalar file or directory path.
#' @return Character scalar normalized path.
#' @examples
#' normalize_publish_path("analysis")
normalize_publish_path <- function(path) {
    normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

#' Read file size in bytes as numeric with NA fallback
#'
#' @param path Character scalar file path.
#' @return Numeric scalar bytes or NA.
#' @examples
#' get_file_size_bytes("summary.xlsx")
get_file_size_bytes <- function(path) {
    size_value <- file.info(path)$size
    if (length(size_value) == 0 || is.na(size_value[[1]])) {
        return(NA_real_)
    }
    as.numeric(size_value[[1]])
}

#' Find the next available date-based publish snapshot identifier
#'
#' Uses the current date by default, then appends `-a`, `-b`, and so on if a
#' snapshot for that date already exists.
#'
#' @param default_snapshot_id Character date-based identifier to start from.
#' @return Character scalar available snapshot identifier.
#' @examples
#' next_available_publish_snapshot_id("2026-08-04")
next_available_publish_snapshot_id <- function(default_snapshot_id = format(Sys.Date(), "%Y-%m-%d")) {
    candidate_id <- default_snapshot_id
    suffix_index <- 0L

    while (dir.exists(get_export_snapshot_dir(snapshot_id = candidate_id))) {
        suffix_index <- suffix_index + 1L
        if (suffix_index > length(letters)) {
            stop("No alphabetical publish snapshot suffixes remain for this date.", call. = FALSE)
        }
        candidate_id <- paste0(default_snapshot_id, "-", letters[[suffix_index]])
    }

    candidate_id
}

#' Map cohort identifiers to runtime output directory names
#'
#' @param cohort_name Character scalar cohort identifier or directory name.
#' @return Character scalar cohort directory name under `OUTPUT_DIR`.
#' @examples
#' map_publish_cohort_dir("uveal_melanoma_full_cohort")
map_publish_cohort_dir <- function(cohort_name) {
    cohort_name <- as.character(cohort_name)
    if (grepl("full", cohort_name, ignore.case = TRUE)) {
        return("uveal_full")
    }
    if (grepl("restricted", cohort_name, ignore.case = TRUE)) {
        return("uveal_restricted")
    }
    if (grepl("gksrs", cohort_name, ignore.case = TRUE)) {
        return("gksrs")
    }
    cohort_name
}

#' Map a cohort selector to its canonical analytic dataset identifier
map_publish_dataset_id <- function(cohort_name) {
    cohort_dir <- map_publish_cohort_dir(cohort_name)
    switch(
        cohort_dir,
        uveal_full = "uveal_melanoma_full_cohort",
        uveal_restricted = "uveal_melanoma_restricted_cohort",
        gksrs = "uveal_melanoma_gksrs_only_cohort",
        stop(sprintf("Unknown publish cohort: %s", cohort_name), call. = FALSE)
    )
}

#' Normalize one analytic column for RDS/XLSX value comparison
normalize_publish_analytic_column <- function(values) {
    if (all(is.na(values))) {
        return(rep(NA_character_, length(values)))
    }
    if (is.factor(values)) {
        return(as.character(values))
    }
    if (inherits(values, "Date")) {
        return(ifelse(is.na(values), NA_character_, format(values, "%Y-%m-%d")))
    }
    if (inherits(values, "POSIXt")) {
        return(ifelse(is.na(values), NA_character_, format(values, "%Y-%m-%d %H:%M:%OS6", tz = "UTC")))
    }
    if (is.numeric(values)) {
        return(ifelse(is.na(values), NA_character_, sprintf("%.15g", as.numeric(values))))
    }
    as.character(values)
}

#' Compare an RDS column with its XLSX review representation
publish_analytic_columns_equal <- function(rds_values, xlsx_values) {
    if (length(rds_values) != length(xlsx_values)) {
        return(FALSE)
    }
    if (all(is.na(rds_values)) && all(is.na(xlsx_values))) {
        return(TRUE)
    }
    if (inherits(rds_values, c("Date", "POSIXt")) && inherits(xlsx_values, c("Date", "POSIXt"))) {
        rds_dates <- as.Date(rds_values, tz = "UTC")
        xlsx_dates <- as.Date(xlsx_values, tz = "UTC")
        return(identical(as.character(rds_dates), as.character(xlsx_dates)))
    }
    if (is.numeric(rds_values) && is.numeric(xlsx_values)) {
        return(isTRUE(all.equal(
            as.numeric(rds_values),
            as.numeric(xlsx_values),
            tolerance = 1e-10,
            check.attributes = FALSE
        )))
    }
    identical(
        normalize_publish_analytic_column(rds_values),
        normalize_publish_analytic_column(xlsx_values)
    )
}

#' Validate and collect selected canonical analytic-data pairs
collect_publish_analytic_data <- function(cohorts = NULL) {
    dataset_ids <- if (is.null(cohorts)) {
        PUBLISH_ANALYTIC_DATASET_IDS
    } else {
        unique(vapply(cohorts, map_publish_dataset_id, character(1)))
    }

    rows <- lapply(dataset_ids, function(dataset_id) {
        rds_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_id, ".rds"))
        xlsx_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_id, ".xlsx"))
        for (required_path in c(rds_path, xlsx_path)) {
            if (!file.exists(required_path) || dir.exists(required_path) || is.na(file.info(required_path)$size)) {
                stop(sprintf("Cannot publish: required analytic data file is missing or unreadable: %s", required_path), call. = FALSE)
            }
        }

        rds_data <- tryCatch(
            readRDS(rds_path),
            error = function(e) stop(sprintf("Cannot read analytic RDS %s: %s", rds_path, e$message), call. = FALSE)
        )
        xlsx_data <- tryCatch(
            openxlsx::read.xlsx(xlsx_path, check.names = FALSE, detectDates = TRUE),
            error = function(e) stop(sprintf("Cannot read analytic XLSX %s: %s", xlsx_path, e$message), call. = FALSE)
        )
        structure_matches <- is.data.frame(rds_data) && is.data.frame(xlsx_data) &&
            nrow(rds_data) == nrow(xlsx_data) && identical(names(rds_data), names(xlsx_data))
        values_match <- structure_matches && all(vapply(names(rds_data), function(column_name) {
            publish_analytic_columns_equal(rds_data[[column_name]], xlsx_data[[column_name]])
        }, logical(1)))
        if (!values_match) {
            stop(sprintf(
                "Cannot publish: analytic XLSX does not match its authoritative RDS for %s.",
                dataset_id
            ), call. = FALSE)
        }

        data.frame(
            source_path = c(rds_path, xlsx_path),
            destination_relative_path = file.path("analytic_data", basename(c(rds_path, xlsx_path))),
            publish_kind = "analytic_data",
            stringsAsFactors = FALSE
        )
    })
    dplyr::bind_rows(rows)
}

#' Reject selected files produced after the validated full-run log
assert_publish_files_not_newer_than_log <- function(paths, log_path) {
    log_mtime <- file.info(log_path)$mtime
    path_info <- file.info(paths)
    newer <- !is.na(path_info$mtime) & path_info$mtime > log_mtime
    if (any(newer)) {
        stop(
            paste(
                sprintf("Cannot publish: selected files are newer than the validated analysis log %s:", log_path),
                paste(paths[newer], collapse = "\n"),
                sep = "\n"
            ),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

#' Collect candidate publish files from known runtime analysis roots
#'
#' @param cohorts Optional character vector of cohorts to publish.
#' @param include_merged_tables Logical indicating whether merged tables should
#'   be included.
#' @return List containing `roots`, `missing_roots`, and `files`.
#' @examples
#' collect_publish_candidates()
collect_publish_candidates <- function(cohorts = NULL, include_merged_tables = TRUE) {
    root_specs <- list()

    if (is.null(cohorts)) {
        default_roots <- list.dirs(OUTPUT_DIR, recursive = FALSE, full.names = TRUE)
        default_roots <- default_roots[basename(default_roots) != basename(MERGED_TABLES_DIR)]
        for (root_path in default_roots) {
            root_specs[[length(root_specs) + 1L]] <- list(
                root_path = root_path,
                root_kind = "cohort",
                root_label = basename(root_path),
                optional = FALSE
            )
        }
    } else {
        cohort_dirs <- unique(vapply(cohorts, map_publish_cohort_dir, character(1)))
        for (cohort_dir in cohort_dirs) {
            root_specs[[length(root_specs) + 1L]] <- list(
                root_path = file.path(OUTPUT_DIR, cohort_dir),
                root_kind = "cohort",
                root_label = cohort_dir,
                optional = FALSE
            )
        }
    }

    if (isTRUE(include_merged_tables)) {
        root_specs[[length(root_specs) + 1L]] <- list(
            root_path = MERGED_TABLES_DIR,
            root_kind = "merged_tables",
            root_label = "merged_tables",
            optional = TRUE
        )
    }

    if (length(root_specs) == 0) {
        return(list(
            roots = data.frame(),
            missing_roots = data.frame(),
            files = data.frame()
        ))
    }

    roots_df <- unique(dplyr::bind_rows(lapply(root_specs, as.data.frame)))
    existing_roots <- roots_df[dir.exists(roots_df$root_path), , drop = FALSE]
    missing_roots <- roots_df[!dir.exists(roots_df$root_path), , drop = FALSE]

    files <- data.frame()
    if (nrow(existing_roots) > 0) {
        file_rows <- vector("list", nrow(existing_roots))
        for (row_index in seq_len(nrow(existing_roots))) {
            root_path <- existing_roots$root_path[[row_index]]
            discovered_files <- list.files(root_path, recursive = TRUE, full.names = TRUE)
            discovered_files <- discovered_files[file.exists(discovered_files) & !dir.exists(discovered_files)]

            if (length(discovered_files) == 0) {
                file_rows[[row_index]] <- NULL
                next
            }

            root_relative_paths <- sub(
                paste0("^", normalize_publish_path(root_path), "/?"),
                "",
                normalize_publish_path(discovered_files)
            )
            destination_relative_paths <- if (identical(existing_roots$root_kind[[row_index]], "merged_tables")) {
                file.path("merged_tables", root_relative_paths)
            } else {
                file.path(existing_roots$root_label[[row_index]], root_relative_paths)
            }

            file_rows[[row_index]] <- data.frame(
                source_path = discovered_files,
                root_path = root_path,
                root_kind = existing_roots$root_kind[[row_index]],
                root_label = existing_roots$root_label[[row_index]],
                root_relative_path = root_relative_paths,
                destination_relative_path = destination_relative_paths,
                stringsAsFactors = FALSE
            )
        }
        files <- dplyr::bind_rows(file_rows)
    }

    list(
        roots = existing_roots,
        missing_roots = missing_roots,
        files = files
    )
}

#' Publish runtime outputs to a dated synced snapshot
#'
#' Copies selected final deliverables from runtime analysis output directories to
#' a dated snapshot under synced export storage.
#'
#' @param cohorts Optional character vector of cohort names to publish.
#' @param snapshot_id Optional character snapshot identifier. When omitted, the
#'   current date is used, with alphabetical suffixes for additional snapshots
#'   on the same day.
#' @param include_merged_tables Logical indicating whether merged tables are
#'   included in publishing.
#' @param dry_run Logical indicating whether to simulate without file copies.
#' @param log_summary Logical indicating whether to write the result summary to
#'   the configured logger.
#' @return List containing `snapshot_dir`, `dry_run`, `summary`, and `manifest`.
#' @examples
#' publish_outputs(dry_run = TRUE)
publish_outputs <- function(
    cohorts = NULL,
    snapshot_id = NULL,
    include_merged_tables = TRUE,
    dry_run = TRUE,
    log_summary = TRUE
) {
    if (!dir.exists(OUTPUT_DIR)) {
        stop(sprintf("Runtime output directory does not exist: %s", OUTPUT_DIR), call. = FALSE)
    }

    latest_analysis_log <- select_publish_analysis_log()

    if (is.null(snapshot_id)) {
        snapshot_id <- next_available_publish_snapshot_id()
    }
    snapshot_dir <- get_export_snapshot_dir(snapshot_id = snapshot_id)
    snapshot_exists <- dir.exists(snapshot_dir)
    if (snapshot_exists && !isTRUE(dry_run)) {
        stop(
            sprintf(
                "Snapshot target already exists: %s\nChoose a different snapshot_id to avoid overwriting prior snapshots.",
                snapshot_dir
            ),
            call. = FALSE
        )
    }

    candidate_info <- collect_publish_candidates(
        cohorts = cohorts,
        include_merged_tables = include_merged_tables
    )

    file_candidates <- candidate_info$files
    publishable_mask <- if (nrow(file_candidates) > 0) {
        vapply(seq_len(nrow(file_candidates)), function(row_index) {
            is_publishable_relative_artifact(
                relative_path = file_candidates$root_relative_path[[row_index]],
                root_kind = file_candidates$root_kind[[row_index]]
            )
        }, logical(1))
    } else {
        logical()
    }
    publishable_files <- file_candidates[publishable_mask, , drop = FALSE]
    skipped_files <- file_candidates[!publishable_mask, , drop = FALSE]
    publishable_files$publish_kind <- "analysis_output"

    analytic_files <- collect_publish_analytic_data(cohorts = cohorts)
    assert_publish_files_not_newer_than_log(
        paths = c(publishable_files$source_path, analytic_files$source_path),
        log_path = latest_analysis_log
    )
    log_file <- data.frame(
        source_path = latest_analysis_log,
        destination_relative_path = basename(latest_analysis_log),
        publish_kind = "analysis_log",
        stringsAsFactors = FALSE
    )
    publishable_files <- dplyr::bind_rows(publishable_files, analytic_files, log_file)

    manifest <- data.frame(
        source_path = character(),
        destination_path = character(),
        status = character(),
        bytes = numeric(),
        message = character(),
        stringsAsFactors = FALSE
    )

    if (!isTRUE(dry_run) && !dir.exists(snapshot_dir)) {
        dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
    }

    for (row_index in seq_len(nrow(publishable_files))) {
        source_path <- publishable_files$source_path[[row_index]]
        destination_path <- file.path(snapshot_dir, publishable_files$destination_relative_path[[row_index]])

        if (!file.exists(source_path)) {
            manifest <- rbind(manifest, data.frame(
                source_path = source_path,
                destination_path = destination_path,
                status = "missing_source",
                bytes = NA_real_,
                message = "Source file disappeared before copy.",
                stringsAsFactors = FALSE
            ))
            next
        }

        if (isTRUE(dry_run)) {
            manifest <- rbind(manifest, data.frame(
                source_path = source_path,
                destination_path = destination_path,
                status = "would_copy",
                bytes = get_file_size_bytes(source_path),
                message = if (snapshot_exists) {
                    "Dry run only. Target snapshot currently exists."
                } else {
                    "Dry run only."
                },
                stringsAsFactors = FALSE
            ))
            next
        }

        destination_parent <- dirname(destination_path)
        if (!dir.exists(destination_parent)) {
            dir.create(destination_parent, recursive = TRUE, showWarnings = FALSE)
        }

        copy_ok <- file.copy(source_path, destination_path, overwrite = FALSE)
        manifest <- rbind(manifest, data.frame(
            source_path = source_path,
            destination_path = destination_path,
            status = if (isTRUE(copy_ok)) "copied" else "copy_failed",
            bytes = get_file_size_bytes(source_path),
            message = if (isTRUE(copy_ok)) "Copied successfully." else "Copy failed.",
            stringsAsFactors = FALSE
        ))
    }

    skipped_manifest <- if (nrow(skipped_files) > 0) {
        data.frame(
            source_path = skipped_files$source_path,
            destination_path = rep(NA_character_, nrow(skipped_files)),
            status = rep("skipped_not_publishable", nrow(skipped_files)),
            bytes = file.info(skipped_files$source_path)$size,
            message = rep("Artifact registry excluded this file from synced publishing.", nrow(skipped_files)),
            stringsAsFactors = FALSE
        )
    } else {
        data.frame()
    }
    if (nrow(skipped_manifest) > 0) {
        manifest <- rbind(manifest, skipped_manifest)
    }

    if (nrow(candidate_info$missing_roots) > 0) {
        missing_manifest <- data.frame(
            source_path = candidate_info$missing_roots$root_path,
            destination_path = NA_character_,
            status = ifelse(candidate_info$missing_roots$optional, "optional_root_absent", "missing_root"),
            bytes = NA_real_,
            message = ifelse(
                candidate_info$missing_roots$optional,
                "Optional publish root is absent for this run.",
                "Required publish root is missing."
            ),
            stringsAsFactors = FALSE
        )
        manifest <- rbind(manifest, missing_manifest)
    }

    if (!isTRUE(dry_run)) {
        manifest_path <- file.path(snapshot_dir, "publish_manifest.csv")
        utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
    }

    summary <- list(
        publishable_files = nrow(publishable_files),
        analytic_data_files = sum(publishable_files$publish_kind == "analytic_data"),
        analysis_log_basename = basename(latest_analysis_log),
        copied = sum(manifest$status == "copied"),
        would_copy = sum(manifest$status == "would_copy"),
        skipped = sum(manifest$status == "skipped_not_publishable"),
        missing = sum(manifest$status %in% c("missing_source", "missing_root")),
        failed = sum(manifest$status == "copy_failed"),
        snapshot_exists = snapshot_exists,
        snapshot_dir = snapshot_dir
    )

    if (isTRUE(log_summary) && exists("USE_LOGS", inherits = TRUE) && isTRUE(USE_LOGS)) {
        logger::log_info(sprintf(
            "Publish summary (dry_run=%s): publishable=%d analytic_data=%d copied=%d would_copy=%d skipped=%d missing=%d failed=%d",
            isTRUE(dry_run),
            summary$publishable_files,
            summary$analytic_data_files,
            summary$copied,
            summary$would_copy,
            summary$skipped,
            summary$missing,
            summary$failed
        ))
        logger::log_info(sprintf("Publish snapshot target: %s", summary$snapshot_dir))
    }

    list(
        snapshot_dir = snapshot_dir,
        dry_run = isTRUE(dry_run),
        summary = summary,
        manifest = manifest,
        latest_analysis_log = latest_analysis_log
    )
}

# Direct-run usage examples:
# Rscript scripts/workflow/publish_outputs.R
# Rscript scripts/workflow/publish_outputs.R --snapshot-id 2026-04-02
# Rscript scripts/workflow/publish_outputs.R --execute --snapshot-id 2026-04-02
# Rscript scripts/workflow/publish_outputs.R --cohorts uveal_melanoma_full_cohort,gksrs
# Rscript scripts/workflow/publish_outputs.R --execute --no-merged-tables

#' Parse command-line arguments for direct `publish_outputs.R` execution
#'
#' Supports a lightweight `Rscript` interface for dry runs, real publish
#' execution, custom snapshot ids, cohort filtering, and merged-table toggles.
#'
#' @return Named list containing `snapshot_id`, `dry_run`,
#'   `include_merged_tables`, `cohorts`, and `help`.
#' @examples
#' \dontrun{
#' parse_publish_outputs_args()
#' }
parse_publish_outputs_args <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    opts <- list(
        snapshot_id = NULL,
        snapshot_id_supplied = FALSE,
        dry_run = TRUE,
        include_merged_tables = TRUE,
        cohorts = NULL,
        help = FALSE
    )

    i <- 1L
    while (i <= length(args)) {
        arg <- args[[i]]

        if (identical(arg, "--help") || identical(arg, "-h")) {
            opts$help <- TRUE
        } else if (identical(arg, "--execute")) {
            opts$dry_run <- FALSE
        } else if (identical(arg, "--dry-run")) {
            opts$dry_run <- TRUE
        } else if (identical(arg, "--no-merged-tables")) {
            opts$include_merged_tables <- FALSE
        } else if (grepl("^--snapshot-id=", arg)) {
            opts$snapshot_id <- sub("^--snapshot-id=", "", arg)
            opts$snapshot_id_supplied <- TRUE
        } else if (identical(arg, "--snapshot-id") && i < length(args)) {
            opts$snapshot_id <- args[[i + 1L]]
            opts$snapshot_id_supplied <- TRUE
            i <- i + 1L
        } else if (grepl("^--cohorts=", arg)) {
            cohort_value <- sub("^--cohorts=", "", arg)
            opts$cohorts <- trimws(unlist(strsplit(cohort_value, ",", fixed = TRUE)))
        } else if (identical(arg, "--cohorts") && i < length(args)) {
            cohort_value <- args[[i + 1L]]
            opts$cohorts <- trimws(unlist(strsplit(cohort_value, ",", fixed = TRUE)))
            i <- i + 1L
        } else {
            stop(sprintf("Unrecognized argument: %s", arg), call. = FALSE)
        }

        i <- i + 1L
    }

    if (!is.null(opts$cohorts)) {
        opts$cohorts <- opts$cohorts[nzchar(opts$cohorts)]
        if (length(opts$cohorts) == 0) {
            opts$cohorts <- NULL
        }
    }

    opts
}

#' Print CLI usage text for direct `publish_outputs.R` execution
#'
#' @return Invisibly returns `NULL` after writing usage text to stdout.
#' @examples
#' \dontrun{
#' print_publish_outputs_usage()
#' }
print_publish_outputs_usage <- function() {
    cat(
        paste(
            "Usage:",
            "  Rscript scripts/workflow/publish_outputs.R [--dry-run] [--execute]",
            "      [--snapshot-id YYYY-MM-DD-or-label]",
            "      [--cohorts cohort1,cohort2]",
            "      [--no-merged-tables]",
            "",
            "Notes:",
            "  --dry-run is the default. Snapshots use YYYY-MM-DD, then -a, -b, etc. on the same day.",
            "  --execute performs the actual copy and writes publish_manifest.csv.",
            "  cohort values can be runtime dirs (uveal_full, uveal_restricted, gksrs)",
            "  or dataset ids (uveal_melanoma_full_cohort, etc.).",
            sep = "\n"
        )
    )
}

#' Quote a publish CLI value only when the shell requires it
#'
#' @param value Character scalar command-line value.
#' @return Character scalar safe to include in a shell command.
#' @examples
#' format_publish_cli_value("2026-04-02")
format_publish_cli_value <- function(value) {
    value <- as.character(value)
    if (grepl("^[A-Za-z0-9._,/-]+$", value)) {
        return(value)
    }
    shQuote(value)
}

#' Build the execute command corresponding to publish CLI options
#'
#' @param opts Parsed publish CLI options.
#' @return Character scalar command that performs the publish.
#' @examples
#' publish_execute_command(list(snapshot_id = "2026-04-02"))
publish_execute_command <- function(opts) {
    command_parts <- c(
        "Rscript scripts/workflow/publish_outputs.R",
        "--execute"
    )

    if (isTRUE(opts$snapshot_id_supplied)) {
        command_parts <- c(
            command_parts,
            "--snapshot-id",
            format_publish_cli_value(opts$snapshot_id)
        )
    }

    if (!is.null(opts$cohorts)) {
        command_parts <- c(
            command_parts,
            "--cohorts",
            format_publish_cli_value(paste(opts$cohorts, collapse = ","))
        )
    }
    if (!isTRUE(opts$include_merged_tables)) {
        command_parts <- c(command_parts, "--no-merged-tables")
    }

    paste(command_parts, collapse = " ")
}

#' Format a concise completion report for direct publish CLI execution
#'
#' @param result Result returned by `publish_outputs()`.
#' @param opts Parsed publish CLI options.
#' @return Character scalar CLI report.
#' @examples
#' format_publish_outputs_cli_report(
#'     result = list(dry_run = TRUE, snapshot_dir = "outputs/2026-04-02", summary = list()),
#'     opts = list(snapshot_id = "2026-04-02", cohorts = NULL, include_merged_tables = TRUE)
#' )
format_publish_outputs_cli_report <- function(result, opts) {
    summary <- result$summary
    selected_cohorts <- if (is.null(opts$cohorts)) {
        "all detected runtime cohorts"
    } else {
        paste(opts$cohorts, collapse = ", ")
    }
    mode_line <- if (isTRUE(result$dry_run)) {
        "DRY RUN: no files were copied"
    } else {
        "EXECUTED: files were copied and a manifest was written"
    }

    report_lines <- c(
        "Publish outputs",
        mode_line,
        "",
        sprintf("Snapshot ID: %s", basename(result$snapshot_dir)),
        sprintf("Snapshot target: %s", result$snapshot_dir),
        sprintf("Cohorts: %s", selected_cohorts),
        sprintf("Merged tables: %s", if (isTRUE(opts$include_merged_tables)) "included" else "excluded"),
        "",
        "Results:",
        sprintf("  Publishable: %d", summary$publishable_files),
        sprintf("  Analytic data files: %d", summary$analytic_data_files),
        sprintf("  Analysis log: %s", summary$analysis_log_basename),
        sprintf("  Copied: %d", summary$copied),
        sprintf("  Would copy: %d", summary$would_copy),
        sprintf("  Excluded by registry: %d", summary$skipped),
        sprintf("  Missing: %d", summary$missing),
        sprintf("  Failed: %d", summary$failed)
    )

    if (isTRUE(result$dry_run) && !isTRUE(summary$snapshot_exists)) {
        report_lines <- c(
            report_lines,
            "",
            "Next step (performs the copy):",
            paste0("  ", publish_execute_command(opts))
        )
    } else if (isTRUE(result$dry_run)) {
        report_lines <- c(
            report_lines,
            "",
            "Next step: this snapshot target already exists and will not be overwritten."
        )
    } else {
        report_lines <- c(
            report_lines,
            sprintf("  Manifest: %s", file.path(result$snapshot_dir, "publish_manifest.csv"))
        )
    }

    paste(report_lines, collapse = "\n")
}

#' Run the direct `Rscript` entry point for publishing outputs
#'
#' Parses CLI arguments, loads the project environment when needed, executes
#' `publish_outputs()`, and prints a concise publish report to stdout.
#'
#' @param opts Optional parsed publish CLI options. Defaults to the command-line
#'   arguments for direct execution.
#' @return Invisibly returns the list produced by `publish_outputs()`, or
#'   `NULL` when showing help text.
#' @examples
#' \dontrun{
#' main()
#' }
main <- function(opts = parse_publish_outputs_args()) {
    if (isTRUE(opts$help)) {
        print_publish_outputs_usage()
        return(invisible(NULL))
    }

    if (!exists("OUTPUT_DIR", inherits = TRUE)) {
        suppressPackageStartupMessages(source(here::here("scripts", "load_all.R")))
    }

    result <- publish_outputs(
        cohorts = opts$cohorts,
        snapshot_id = opts$snapshot_id,
        include_merged_tables = opts$include_merged_tables,
        dry_run = opts$dry_run,
        log_summary = FALSE
    )

    cat(format_publish_outputs_cli_report(result, opts), "\n", sep = "")

    invisible(result)
}

if (sys.nframe() == 0L) {
    main()
}
