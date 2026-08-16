# =============================================================================
# CORE DATA PATHS AND DIRECTORIES
# =============================================================================
# CRITICAL: Enforce the canonical workspace and Project Vault architecture:
# - Code root (source controlled): PROJECT_ROOT / CODE_ROOT
# - Runtime root (local non-synced): RUNTIME_ROOT
# - Raw input root (synced): RAW_DATA_DIR = EXPORT_ROOT / Original Files
# - Durable publish root (synced): EXPORT_ANALYSIS_DIR = EXPORT_ROOT / outputs
# Analysis identifiers use PROJECT_SLUG; filesystem paths use REPOSITORY_SLUG.
PROJECT_ROOT <- here::here()
PROJECT_SLUG <- "uveal_melanoma"
REPOSITORY_SLUG <- "uveal-melanoma"
WORKSPACE_ROOT <- normalizePath(
    file.path(PROJECT_ROOT, ".."),
    winslash = "/",
    mustWork = FALSE
)
CODE_ROOT <- PROJECT_ROOT
DEFAULT_RUNTIME_ROOT <- "~/Workspaces/uveal-melanoma/runtime"
DEFAULT_EXPORT_PARENT_DIR <- "~/Library/CloudStorage/OneDrive-Personal/Project Vault/Research"
DEFAULT_EXPORT_ROOT <- file.path(DEFAULT_EXPORT_PARENT_DIR, REPOSITORY_SLUG)
DEFAULT_RAW_DATA_DIR <- file.path(DEFAULT_EXPORT_ROOT, "Original Files")
DEFAULT_PUBLISH_ROOT <- file.path(DEFAULT_EXPORT_ROOT, "outputs")

#' Resolve a configured filesystem path with fallback behavior
#'
#' Returns a normalized absolute path from a configured value. Empty or missing
#' values fall back to `default_path`. Relative paths are rejected so runtime
#' and export roots cannot silently collapse back into the repository tree.
#'
#' @param path_value Character scalar configured path (often from env vars).
#' @param default_path Character scalar default path when `path_value` is empty.
#' @param allow_relative Logical indicating whether relative paths are allowed.
#' @return Character scalar absolute path.
#' @examples
#' resolve_config_path("", "~/Workspaces/uveal-melanoma/runtime")
resolve_config_path <- function(path_value, default_path, allow_relative = FALSE) {
    candidate_path <- path_value
    if (is.null(candidate_path) || !nzchar(trimws(candidate_path))) {
        candidate_path <- default_path
    }

    if (is.null(candidate_path) || !nzchar(trimws(candidate_path))) {
        stop("Path resolution failed: both configured and default paths are empty.", call. = FALSE)
    }

    expanded_path <- path.expand(trimws(candidate_path))
    is_absolute <- grepl("^(/|[A-Za-z]:[/\\\\])", expanded_path)
    if (!is_absolute) {
        if (!isTRUE(allow_relative)) {
            stop(
                sprintf(
                    "Configured path '%s' must be absolute. Relative runtime/export overrides are not allowed.",
                    candidate_path
                ),
                call. = FALSE
            )
        }
        expanded_path <- file.path(PROJECT_ROOT, expanded_path)
    }

    normalizePath(expanded_path, winslash = "/", mustWork = FALSE)
}

#' Resolve the canonical Project Vault root for this project
#'
#' `OCULAR_EXPORT_PARENT_DIR` may redirect the Project Vault research parent for
#' isolated tests or another machine. The repository slug remains fixed.
#'
#' @return A list containing `export_parent_dir` and `export_root`.
resolve_export_root_config <- function() {
    configured_export_parent_dir <- Sys.getenv("OCULAR_EXPORT_PARENT_DIR", unset = "")
    export_parent_dir <- resolve_config_path(
        configured_export_parent_dir,
        DEFAULT_EXPORT_PARENT_DIR
    )

    list(
        export_parent_dir = export_parent_dir,
        export_root = file.path(export_parent_dir, REPOSITORY_SLUG)
    )
}

RUNTIME_ROOT <- resolve_config_path(
    Sys.getenv("OCULAR_RUNTIME_ROOT", unset = ""),
    DEFAULT_RUNTIME_ROOT
)
RUNTIME_PARENT_DIR <- dirname(RUNTIME_ROOT)

export_root_config <- resolve_export_root_config()
EXPORT_PARENT_DIR <- export_root_config$export_parent_dir
EXPORT_ROOT <- normalizePath(export_root_config$export_root, winslash = "/", mustWork = FALSE)
EXPORT_ANALYSIS_DIR <- file.path(EXPORT_ROOT, "outputs")

# Export-backed raw input paths (authoritative source files)
RAW_DATA_DIR <- resolve_config_path(
    Sys.getenv("RAW_DATA_DIR", unset = ""),
    file.path(EXPORT_ROOT, "Original Files")
)
DATA_DICTIONARY_PATH <- resolve_config_path(
    Sys.getenv("DATA_DICTIONARY_PATH", unset = ""),
    file.path(RAW_DATA_DIR, "Data Dictionary.xlsx")
)

# Runtime-only generated artifacts
PROCESSED_DATA_DIR <- resolve_config_path(
    Sys.getenv("PROCESSED_DATA_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "Analytic Dataset")
)
OUTPUT_DIR <- resolve_config_path(
    Sys.getenv("OUTPUT_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "Analysis")
)
LOGS_DIR <- resolve_config_path(
    Sys.getenv("LOGS_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "logs")
)
TOOLS_OUTPUT_DIR <- resolve_config_path(
    Sys.getenv("TOOLS_OUTPUT_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "tools_output")
)
TEST_OUTPUT_DIR <- resolve_config_path(
    Sys.getenv("TEST_OUTPUT_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "test_output")
)
MERGED_TABLES_DIR <- resolve_config_path(
    Sys.getenv("MERGED_TABLES_DIR", unset = ""),
    file.path(OUTPUT_DIR, "merged_tables")
)
SHARE_PACKETS_DIR <- resolve_config_path(
    Sys.getenv("SHARE_PACKETS_DIR", unset = ""),
    file.path(RUNTIME_ROOT, "share_packets")
)
PEER_REVIEW_REVISION_AUDITS_DIR <- file.path(
    SHARE_PACKETS_DIR,
    "peer_review_revision_audits"
)

# Project Vault project root exposed for existing analysis code.
DATA_DIR <- EXPORT_ROOT

#' Create runtime directories required for analysis execution
#'
#' Initializes all runtime-only directories under `RUNTIME_ROOT` so analyses do
#' not write intermediate artifacts into repository or synced export trees.
#'
#' @return Character vector of initialized runtime directories.
#' @examples
#' initialize_runtime_dirs()
initialize_runtime_dirs <- function() {
    runtime_dirs <- unique(c(
        RUNTIME_ROOT,
        PROCESSED_DATA_DIR,
        OUTPUT_DIR,
        LOGS_DIR,
        TOOLS_OUTPUT_DIR,
        TEST_OUTPUT_DIR,
        MERGED_TABLES_DIR,
        SHARE_PACKETS_DIR,
        PEER_REVIEW_REVISION_AUDITS_DIR
    ))

    for (dir_path in runtime_dirs) {
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        }
    }

    invisible(runtime_dirs)
}

#' Assert that required raw input paths are available
#'
#' Fails fast when raw input directories or required files are missing from the
#' synced export root.
#'
#' @param input_filename Character scalar input Excel filename.
#' @param require_data_dictionary Logical; whether to require dictionary presence.
#' @return Invisibly returns TRUE when all checks pass.
#' @examples
#' assert_required_input_paths()
assert_required_input_paths <- function(input_filename = INPUT_FILENAME, require_data_dictionary = FALSE) {
    path_issues <- character()

    if (!dir.exists(RAW_DATA_DIR)) {
        path_issues <- c(path_issues, sprintf("Raw data directory is missing: %s", RAW_DATA_DIR))
    }

    source_file_path <- file.path(RAW_DATA_DIR, input_filename)
    if (!file.exists(source_file_path)) {
        path_issues <- c(path_issues, sprintf("Required input file is missing: %s", source_file_path))
    }

    if (isTRUE(require_data_dictionary) && !file.exists(DATA_DICTIONARY_PATH)) {
        path_issues <- c(path_issues, sprintf("Data dictionary is missing: %s", DATA_DICTIONARY_PATH))
    }

    if (length(path_issues) > 0) {
        stop(
            paste(
                "Required raw input path checks failed:",
                paste(paste0("- ", path_issues), collapse = "\n"),
                sprintf("Configured Project Vault root: %s", EXPORT_ROOT),
                sep = "\n"
            ),
            call. = FALSE
        )
    }

    invisible(TRUE)
}

#' Construct the dated export snapshot directory path
#'
#' @param snapshot_id Character scalar snapshot identifier.
#' @return Character scalar export snapshot directory path.
#' @examples
#' get_export_snapshot_dir()
get_export_snapshot_dir <- function(snapshot_id = format(Sys.Date(), "%Y-%m-%d")) {
    if (is.null(snapshot_id) || !nzchar(trimws(snapshot_id))) {
        stop("Snapshot id must be a non-empty string.", call. = FALSE)
    }

    snapshot_id <- trimws(snapshot_id)
    if (grepl("[/\\\\]", snapshot_id)) {
        stop(sprintf("Snapshot id '%s' cannot contain path separators.", snapshot_id), call. = FALSE)
    }

    file.path(EXPORT_ANALYSIS_DIR, snapshot_id)
}

#' Determine whether a file path is publishable to synced exports
#'
#' Publishable artifacts are final deliverables (tables, figures, summaries) and
#' explicitly exclude runtime-only formats and directories.
#'
#' @param path Character scalar file path to evaluate.
#' @return Logical scalar indicating publish eligibility.
#' @examples
#' is_publishable_artifact("summary.xlsx")
is_publishable_artifact <- function(path) {
    if (is.null(path) || is.na(path) || !nzchar(trimws(path)) || dir.exists(path)) {
        return(FALSE)
    }

    normalized_path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
    extension <- tolower(tools::file_ext(normalized_path))
    publishable_extensions <- c("xlsx", "xls", "html", "htm", "md", "txt", "png", "pdf", "csv", "tsv")
    if (!(extension %in% publishable_extensions)) {
        return(FALSE)
    }

    excluded_directories <- c("logs", "cache", "caches", "tools_output", "test_output", "tmp", "temp")
    path_segments <- strsplit(normalized_path, "/", fixed = TRUE)[[1]]
    if (any(path_segments %in% excluded_directories)) {
        return(FALSE)
    }

    file_name <- basename(normalized_path)
    if (grepl("(^\\.|~$|\\.tmp$|\\.temp$)", file_name, ignore.case = TRUE)) {
        return(FALSE)
    }

    TRUE
}

PUBLISH_ARTIFACT_REGISTRY <- list(
    cohort = c(
        "^00_General/.+\\.(xlsx|html|txt|csv|tsv)$",
        "^01_Efficacy/.+\\.(xlsx|html|png|pdf|md|txt|csv|tsv)$",
        "^02_Safety/.+\\.(xlsx|html|png|pdf|txt|csv|tsv)$",
        "^03_Repeat_Radiation/.+\\.(xlsx|html|png|pdf|txt|csv|tsv)$",
        "^04_GEP_Validation/.+\\.(xlsx|html|png|pdf|md|txt|csv|tsv)$"
    ),
    merged_tables = c(
        "^.+\\.(xlsx|html|png|pdf|md|csv|tsv|txt)$"
    ),
    excluded = c(
        "(^|/)(logs|cache|caches|tools_output|test_output|tmp|temp)(/|$)",
        "(^|/).*(?:_diagnostics\\.xlsx|_SKIPPED\\.html|_NO_CONTENT_DIAGNOSTIC\\.html|publish_manifest\\.csv)$",
        "(^|/)01_Efficacy/h_propensity_score_sensitivity/.+\\.(csv|tsv|rds)$",
        "(^|/)04_GEP_Validation/.+(?:_validation_narrative_summary|_extrapolation_assumption_summary|_mfs_sensitivity_summary|_simple_gep_validation_report|_exploratory_no_gep_summary)\\.txt$"
    )
)

PUBLISH_ANALYTIC_DATASET_IDS <- c(
    "uveal_melanoma_full_cohort",
    "uveal_melanoma_restricted_cohort",
    "uveal_melanoma_gksrs_only_cohort"
)

#' Determine whether a relative publish path is approved by the artifact registry
#'
#' @param relative_path Character scalar path relative to a publish root.
#' @param root_kind Character scalar publish root type.
#'
#' @return Logical scalar indicating whether the artifact is allowed.
is_publishable_relative_artifact <- function(relative_path, root_kind = c("cohort", "merged_tables")) {
    root_kind <- match.arg(root_kind)
    relative_path <- gsub("^\\./", "", relative_path)

    if (is.null(relative_path) || is.na(relative_path) || !nzchar(relative_path)) {
        return(FALSE)
    }

    if (any(vapply(PUBLISH_ARTIFACT_REGISTRY$excluded, grepl, logical(1), x = relative_path, perl = TRUE))) {
        return(FALSE)
    }

    any(vapply(PUBLISH_ARTIFACT_REGISTRY[[root_kind]], grepl, logical(1), x = relative_path, perl = TRUE))
}
