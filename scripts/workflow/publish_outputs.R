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
#' @param snapshot_id Character snapshot identifier (default date stamp).
#' @param include_merged_tables Logical indicating whether merged tables are
#'   included in publishing.
#' @param dry_run Logical indicating whether to simulate without file copies.
#' @return List containing `snapshot_dir`, `dry_run`, `summary`, and `manifest`.
#' @examples
#' publish_outputs(dry_run = TRUE)
publish_outputs <- function(
    cohorts = NULL,
    snapshot_id = format(Sys.Date(), "%Y-%m-%d"),
    include_merged_tables = TRUE,
    dry_run = TRUE
) {
    if (!dir.exists(OUTPUT_DIR)) {
        stop(sprintf("Runtime output directory does not exist: %s", OUTPUT_DIR), call. = FALSE)
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
        copied = sum(manifest$status == "copied"),
        would_copy = sum(manifest$status == "would_copy"),
        skipped = sum(manifest$status == "skipped_not_publishable"),
        missing = sum(manifest$status %in% c("missing_source", "missing_root")),
        failed = sum(manifest$status == "copy_failed"),
        snapshot_exists = snapshot_exists,
        snapshot_dir = snapshot_dir
    )

    if (exists("USE_LOGS", inherits = TRUE) && isTRUE(USE_LOGS)) {
        logger::log_info(sprintf(
            "Publish summary (dry_run=%s): publishable=%d copied=%d would_copy=%d skipped=%d missing=%d failed=%d",
            isTRUE(dry_run),
            summary$publishable_files,
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
        manifest = manifest
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
        snapshot_id = format(Sys.Date(), "%Y-%m-%d"),
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
        } else if (identical(arg, "--snapshot-id") && i < length(args)) {
            opts$snapshot_id <- args[[i + 1L]]
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
            "  --dry-run is the default and only previews the publish manifest.",
            "  --execute performs the actual copy and writes publish_manifest.csv.",
            "  cohort values can be runtime dirs (uveal_full, uveal_restricted, gksrs)",
            "  or dataset ids (uveal_melanoma_full_cohort, etc.).",
            sep = "\n"
        )
    )
}

#' Run the direct `Rscript` entry point for publishing outputs
#'
#' Parses CLI arguments, loads the project environment when needed, executes
#' `publish_outputs()`, and prints a short manifest preview to stdout.
#'
#' @return Invisibly returns the list produced by `publish_outputs()`, or
#'   `NULL` when showing help text.
#' @examples
#' \dontrun{
#' main()
#' }
main <- function() {
    opts <- parse_publish_outputs_args()
    if (isTRUE(opts$help)) {
        print_publish_outputs_usage()
        return(invisible(NULL))
    }

    if (!exists("OUTPUT_DIR", inherits = TRUE)) {
        source(here::here("scripts", "load_all.R"))
    }

    result <- publish_outputs(
        cohorts = opts$cohorts,
        snapshot_id = opts$snapshot_id,
        include_merged_tables = opts$include_merged_tables,
        dry_run = opts$dry_run
    )

    cat(sprintf("Snapshot target: %s\n", result$snapshot_dir))
    cat(sprintf("Mode: %s\n", if (isTRUE(result$dry_run)) "dry_run" else "execute"))
    print(result$summary)

    preview_cols <- intersect(
        c("status", "source_path", "destination_path", "message"),
        names(result$manifest)
    )
    if (length(preview_cols) > 0 && nrow(result$manifest) > 0) {
        preview_manifest <- utils::head(result$manifest[, preview_cols, drop = FALSE], 10)
        print(preview_manifest, row.names = FALSE)
        if (nrow(result$manifest) > 10) {
            cat(sprintf("... %d more manifest rows\n", nrow(result$manifest) - 10))
        }
    }

    invisible(result)
}

if (sys.nframe() == 0L) {
    main()
}
