#!/usr/bin/env Rscript

# To run:
# Rscript scripts/tools/export_gep_objective4_to_downloads.R

#' Resolve the repository root for this exporter
#'
#' @return Character scalar absolute path to the repository root.
resolve_repo_root <- function() {
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd_args, value = TRUE)

    if (length(file_arg) > 0) {
        script_path <- sub("^--file=", "", file_arg[[1]])
        return(normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = FALSE))
    }

    cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(cwd, "scripts", "load_all.R"))) {
        return(cwd)
    }

    stop(
        "Unable to resolve the repository root. Run this script from the project root or via Rscript.",
        call. = FALSE
    )
}

#' Resolve the runtime analysis root for this project
#'
#' @param project_slug Character scalar analysis slug.
#' @return Character scalar absolute path to the runtime analysis root.
resolve_runtime_analysis_root <- function(project_slug) {
    output_dir <- Sys.getenv("OUTPUT_DIR", unset = "")
    if (nzchar(trimws(output_dir))) {
        return(normalizePath(path.expand(output_dir), winslash = "/", mustWork = FALSE))
    }

    runtime_root <- Sys.getenv("OCULAR_RUNTIME_ROOT", unset = "")
    if (!nzchar(trimws(runtime_root))) {
        runtime_root <- file.path(path.expand("~/Workspaces"), "uveal-melanoma", "runtime")
    }

    file.path(normalizePath(path.expand(runtime_root), winslash = "/", mustWork = FALSE), "Analysis")
}

#' Map a cohort directory name to the analysis filename prefix
#'
#' @param cohort_dir_name Character scalar runtime cohort directory name.
#' @return Character scalar prefix used in Objective 4 artifact filenames.
make_objective4_prefix <- function(cohort_dir_name) {
    if (grepl("full", cohort_dir_name, ignore.case = TRUE)) {
        return("full_cohort_")
    }
    if (grepl("restricted", cohort_dir_name, ignore.case = TRUE)) {
        return("restricted_cohort_")
    }
    if (grepl("gksrs", cohort_dir_name, ignore.case = TRUE)) {
        return("gksrs_only_cohort_")
    }

    paste0(cohort_dir_name, "_")
}

#' Map a cohort directory name to a short human-readable label
#'
#' @param cohort_dir_name Character scalar runtime cohort directory name.
#' @return Character scalar label suitable for folder names.
make_objective4_cohort_label <- function(cohort_dir_name) {
    if (grepl("full", cohort_dir_name, ignore.case = TRUE)) {
        return("full")
    }
    if (grepl("restricted", cohort_dir_name, ignore.case = TRUE)) {
        return("restricted")
    }
    if (grepl("gksrs", cohort_dir_name, ignore.case = TRUE)) {
        return("gksrs")
    }

    cohort_dir_name
}

#' Return the cohorts that should be exported
#'
#' @return Named character vector mapping short cohort labels to runtime cohort directories.
get_objective4_cohort_map <- function() {
    c(
        full = "uveal_full",
        restricted = "uveal_restricted",
        gksrs = "gksrs"
    )
}

#' Determine the destination subdirectory for an exported artifact
#'
#' @param source_path Character scalar source file path.
#' @return One of `text`, `png`, `excel`, or `other`.
get_objective4_artifact_category <- function(source_path) {
    extension <- tolower(tools::file_ext(source_path))
    if (extension %in% c("txt", "md")) {
        return("text")
    }
    if (extension %in% c("png")) {
        return("png")
    }
    if (extension %in% c("xlsx", "xls")) {
        return("excel")
    }
    "other"
}

#' Build the curated Objective 4 export manifest
#'
#' The manifest intentionally keeps only the paper-facing summaries and a small
#' set of figures. It excludes the many intermediate diagnostic outputs so the
#' destination folder stays drag-and-drop friendly.
#'
#' @param objective4_root Character scalar path to the Objective 4 output root.
#' @param prefix Character scalar filename prefix for the cohort.
#' @param include_prame Logical; whether to include the PRAME delta-C figures.
#' @return Character vector of source file paths named by export label.
build_objective4_export_manifest <- function(objective4_root, prefix, include_prame = FALSE) {
    manifest <- c(
        unified_workbook = file.path(objective4_root, paste0(prefix, "unified_gep_validation_summary.xlsx")),
        simple_workbook = file.path(objective4_root, "unified_summary", paste0(prefix, "simple_gep_validation.xlsx")),
        sensitivity_workbook = file.path(objective4_root, "unified_summary", paste0(prefix, "mfs_sensitivity_summary.xlsx")),
        simple_report = file.path(objective4_root, "unified_summary", paste0(prefix, "simple_gep_validation_report.md")),
        sensitivity_report = file.path(objective4_root, "unified_summary", paste0(prefix, "mfs_sensitivity_summary.md")),
        mfs_narrative = file.path(objective4_root, "a_metastasis_free_survival", "05_summary_tables", paste0(prefix, "mfs_validation_narrative_summary.md")),
        mss_narrative = file.path(objective4_root, "b_melanoma_specific_survival", "03_summary_tables", paste0(prefix, "mss_validation_narrative_summary.md")),
        mfs_extrapolation_assumption = file.path(objective4_root, "a_metastasis_free_survival", "05_summary_tables", paste0(prefix, "mfs_extrapolation_assumption_summary.md")),
        mss_extrapolation_assumption = file.path(objective4_root, "b_melanoma_specific_survival", "03_summary_tables", paste0(prefix, "mss_extrapolation_assumption_summary.md")),
        mfs_calibration = file.path(objective4_root, "a_metastasis_free_survival", "04_validation", paste0(prefix, "mfs_calibration_full.png")),
        mfs_simple = file.path(objective4_root, "a_metastasis_free_survival", "04_validation", paste0(prefix, "simple_mfs_validation.png")),
        mfs_km = file.path(objective4_root, "a_metastasis_free_survival", "01_km_curves", paste0(prefix, "mfs_simplified_gep_km.png")),
        mss_calibration = file.path(objective4_root, "b_melanoma_specific_survival", "02_validation", paste0(prefix, "mss_calibration_full.png")),
        mss_simple = file.path(objective4_root, "b_melanoma_specific_survival", "02_validation", paste0(prefix, "simple_mss_validation.png")),
        mss_cif = file.path(objective4_root, "b_melanoma_specific_survival", "01_cif_curves", paste0(prefix, "mss_cumulative_incidence_curves.png"))
    )

    if (isTRUE(include_prame)) {
        manifest <- c(
            manifest,
            mfs_prame = file.path(objective4_root, "a_metastasis_free_survival", "04_validation", paste0(prefix, "mfs_prame_delta_c.png")),
            mss_prame = file.path(objective4_root, "b_melanoma_specific_survival", "02_validation", paste0(prefix, "mss_prame_delta_c.png"))
        )
    }

    manifest
}

#' Build the exploratory no-GEP export manifest
#'
#' Only the full cohort produces the exploratory no-GEP analysis outputs. These
#' are kept small and paper-facing so the export remains easy to browse.
#'
#' @param objective4_root Character scalar path to the Objective 4 output root.
#' @param prefix Character scalar filename prefix for the cohort.
#' @return Named character vector of source file paths.
build_exploratory_no_gep_export_manifest <- function(objective4_root, prefix) {
    c(
        no_gep_workbook = file.path(objective4_root, "d_exploratory_no_gep", paste0(prefix, "exploratory_no_gep_report.xlsx")),
        no_gep_summary = file.path(objective4_root, "d_exploratory_no_gep", paste0(prefix, "exploratory_no_gep_summary.md")),
        no_gep_mfs_km = file.path(objective4_root, "d_exploratory_no_gep", "plots", paste0(prefix, "exploratory_no_gep_mfs_km_corrected.png")),
        no_gep_mss_cif = file.path(objective4_root, "d_exploratory_no_gep", "plots", paste0(prefix, "exploratory_no_gep_mss_cif_corrected.png"))
    )
}

#' Copy curated Objective 4 artifacts into a flat destination folder
#'
#' @param source_paths Named character vector of source file paths.
#' @param export_root Character scalar target directory.
#' @param cohort_label Character scalar cohort subdirectory name.
#' @param dry_run Logical; when `TRUE`, report intended copies without writing.
#' @return Data frame describing copy outcomes.
copy_objective4_artifacts <- function(source_paths, export_root, cohort_label, dry_run = FALSE) {
    cohort_root <- file.path(export_root, cohort_label)
    if (!isTRUE(dry_run) && !dir.exists(cohort_root)) {
        dir.create(cohort_root, recursive = TRUE, showWarnings = FALSE)
    }

    rows <- lapply(names(source_paths), function(label) {
        source_path <- unname(source_paths[[label]])
        category <- get_objective4_artifact_category(source_path)
        destination_path <- file.path(export_root, cohort_label, category, basename(source_path))

        if (!file.exists(source_path)) {
            return(data.frame(
                label = label,
                cohort = cohort_label,
                category = category,
                source_path = source_path,
                destination_path = destination_path,
                status = "missing",
                stringsAsFactors = FALSE
            ))
        }

        if (isTRUE(dry_run)) {
            return(data.frame(
                label = label,
                cohort = cohort_label,
                category = category,
                source_path = source_path,
                destination_path = destination_path,
                status = "would_copy",
                stringsAsFactors = FALSE
            ))
        }

        destination_parent <- dirname(destination_path)
        if (!dir.exists(destination_parent)) {
            dir.create(destination_parent, recursive = TRUE, showWarnings = FALSE)
        }
        copied <- file.copy(source_path, destination_path, overwrite = FALSE)
        data.frame(
            label = label,
            cohort = cohort_label,
            category = category,
            source_path = source_path,
            destination_path = destination_path,
            status = if (isTRUE(copied)) "copied" else "copy_failed",
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, rows)
}

#' Export the Objective 4 paper pack to Downloads
#'
#' @param destination_dir Optional destination directory. Defaults to a fresh
#'   folder under `~/Downloads`.
#' @param include_prame Logical; whether to include the PRAME delta-C figures.
#' @param dry_run Logical; when `TRUE`, the script only reports what would be
#'   copied.
#' @return Invisibly returns the copy manifest.
export_gep_objective4_to_downloads <- function(
    destination_dir = NULL,
    include_prame = FALSE,
    dry_run = identical(Sys.getenv("GEP_OBJECTIVE4_DRY_RUN", unset = "0"), "1")
) {
    repo_root <- resolve_repo_root()
    project_slug <- basename(repo_root)
    runtime_analysis_root <- resolve_runtime_analysis_root(project_slug)

    if (is.null(destination_dir) || !nzchar(trimws(destination_dir))) {
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
        destination_dir <- file.path(
            path.expand("~/Downloads"),
            sprintf("%s_gep_objective4_%s", project_slug, timestamp)
        )
    } else {
        destination_dir <- normalizePath(path.expand(destination_dir), winslash = "/", mustWork = FALSE)
    }

    cohort_map <- get_objective4_cohort_map()
    manifest_rows <- list()

    for (cohort_label in names(cohort_map)) {
        cohort_dir_name <- cohort_map[[cohort_label]]
        prefix <- make_objective4_prefix(cohort_dir_name)
        objective4_root <- file.path(runtime_analysis_root, cohort_dir_name, "04_GEP_Validation")

        if (!dir.exists(objective4_root)) {
            manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
                label = "objective4_root",
                cohort = cohort_label,
                category = "other",
                source_path = objective4_root,
                destination_path = file.path(destination_dir, cohort_label),
                status = "missing_root",
                stringsAsFactors = FALSE
            )
            next
        }

        source_paths <- build_objective4_export_manifest(
            objective4_root = objective4_root,
            prefix = prefix,
            include_prame = include_prame
        )

        if (identical(cohort_label, "full")) {
            source_paths <- c(
                source_paths,
                build_exploratory_no_gep_export_manifest(
                    objective4_root = objective4_root,
                    prefix = prefix
                )
            )
        }

        manifest_rows[[length(manifest_rows) + 1L]] <- copy_objective4_artifacts(
            source_paths = source_paths,
            export_root = destination_dir,
            cohort_label = cohort_label,
            dry_run = dry_run
        )
    }

    manifest <- do.call(rbind, manifest_rows)

    copied_n <- sum(manifest$status %in% c("copied", "would_copy"))
    missing_n <- sum(manifest$status == "missing")
    failed_n <- sum(manifest$status == "copy_failed")
    missing_root_n <- sum(manifest$status == "missing_root")

    cat(sprintf("Objective 4 export target: %s\n", destination_dir))
    cat(sprintf("Files selected: %d\n", nrow(manifest)))
    cat(sprintf("Would/copies: %d\n", copied_n))
    cat(sprintf("Missing: %d\n", missing_n + missing_root_n))
    cat(sprintf("Failed: %d\n", failed_n))

    visible_manifest <- manifest[manifest$status %in% c("copied", "would_copy"), , drop = FALSE]
    if (nrow(visible_manifest) > 0) {
        print(visible_manifest[, c("cohort", "category", "label", "status", "destination_path")], row.names = FALSE)
    }

    invisible(manifest)
}

if (sys.nframe() == 0L) {
    include_prame <- identical(Sys.getenv("GEP_OBJECTIVE4_INCLUDE_PRAME", unset = "0"), "1")
    destination_dir <- Sys.getenv("GEP_OBJECTIVE4_EXPORT_DIR", unset = "")
    if (!nzchar(trimws(destination_dir))) {
        destination_dir <- NULL
    }

    export_gep_objective4_to_downloads(
        destination_dir = destination_dir,
        include_prame = include_prame
    )
}
