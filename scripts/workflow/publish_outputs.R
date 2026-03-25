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
    roots <- character()

    if (is.null(cohorts)) {
        default_roots <- list.dirs(OUTPUT_DIR, recursive = FALSE, full.names = TRUE)
        roots <- default_roots[dir.exists(default_roots)]
        if (length(roots) == 0 && dir.exists(OUTPUT_DIR)) {
            roots <- OUTPUT_DIR
        }
    } else {
        cohort_dirs <- unique(vapply(cohorts, map_publish_cohort_dir, character(1)))
        roots <- file.path(OUTPUT_DIR, cohort_dirs)
    }

    if (isTRUE(include_merged_tables)) {
        roots <- c(roots, MERGED_TABLES_DIR)
    } else {
        roots <- roots[normalize_publish_path(roots) != normalize_publish_path(MERGED_TABLES_DIR)]
    }

    roots <- unique(roots)
    missing_roots <- roots[!dir.exists(roots)]
    existing_roots <- roots[dir.exists(roots)]

    files <- character()
    if (length(existing_roots) > 0) {
        files <- unique(unlist(lapply(existing_roots, function(root) {
            list.files(root, recursive = TRUE, full.names = TRUE)
        }), use.names = FALSE))
        files <- files[file.exists(files) & !dir.exists(files)]
    }

    list(
        roots = existing_roots,
        missing_roots = unique(missing_roots),
        files = unique(files)
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

    publishable_files <- candidate_info$files[
        vapply(candidate_info$files, is_publishable_artifact, logical(1))
    ]
    skipped_files <- setdiff(candidate_info$files, publishable_files)

    output_root_normalized <- normalize_publish_path(OUTPUT_DIR)
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

    for (source_path in publishable_files) {
        source_normalized <- normalize_publish_path(source_path)
        relative_path <- sub(
            paste0("^", output_root_normalized, "/?"),
            "",
            source_normalized
        )
        if (identical(relative_path, source_normalized)) {
            relative_path <- basename(source_path)
        }

        destination_path <- file.path(snapshot_dir, relative_path)

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

    skipped_manifest <- data.frame(
        source_path = skipped_files,
        destination_path = rep(NA_character_, length(skipped_files)),
        status = rep("skipped_not_publishable", length(skipped_files)),
        bytes = if (length(skipped_files) > 0) file.info(skipped_files)$size else numeric(),
        message = rep("File extension or directory policy excluded this artifact.", length(skipped_files)),
        stringsAsFactors = FALSE
    )
    if (nrow(skipped_manifest) > 0) {
        manifest <- rbind(manifest, skipped_manifest)
    }

    if (length(candidate_info$missing_roots) > 0) {
        missing_manifest <- data.frame(
            source_path = candidate_info$missing_roots,
            destination_path = NA_character_,
            status = "missing_root",
            bytes = NA_real_,
            message = "Configured publish root is missing.",
            stringsAsFactors = FALSE
        )
        manifest <- rbind(manifest, missing_manifest)
    }

    if (!isTRUE(dry_run)) {
        manifest_path <- file.path(snapshot_dir, "publish_manifest.csv")
        utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
    }

    summary <- list(
        publishable_files = length(publishable_files),
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
