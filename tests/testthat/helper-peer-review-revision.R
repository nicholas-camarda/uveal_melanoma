expect_workbook_has_sheets <- function(path, required_sheets) {
    expect_true(file.exists(path), info = paste("Missing workbook:", path))
    sheets <- readxl::excel_sheets(path)
    expect_true(
        all(required_sheets %in% sheets),
        info = paste("Workbook", path, "missing sheets:", paste(setdiff(required_sheets, sheets), collapse = ", "))
    )
    invisible(sheets)
}

expect_artifact_fresh_after <- function(path, started_at) {
    expect_true(file.exists(path), info = paste("Missing artifact:", path))
    artifact_time <- file.info(path)$mtime
    expect_true(
        !is.na(artifact_time) && artifact_time >= started_at,
        info = paste("Artifact is stale or has missing mtime:", path)
    )
}

expect_no_reviewer_facing_paths <- function(path) {
    text <- readLines(path, warn = FALSE)
    forbidden <- grep(
        "/Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision|/Users/ncamarda/Projects/uveal_melanoma/docs|/Users/ncamarda/Projects/uveal_melanoma/scripts",
        text,
        value = TRUE
    )
    expect_equal(
        length(forbidden),
        0,
        info = paste("Committed reviewer-facing doc contains source-machine absolute paths:", paste(forbidden, collapse = "\n"))
    )
}

#' Build typed endpoint output directories for tests
#'
#' Mirrors `create_output_structure()` registration for a route-prefix family.
#'
#' @param test_output_dir Cohort output root for the test harness.
#' @param route_prefix_pattern Regex matching route keys to retain.
#' @return Named list of base and artifact subdirectories.
build_subdivided_output_dirs <- function(test_output_dir, route_prefix_pattern) {
    dirs <- create_output_structure(test_output_dir)
    dirs[grepl(route_prefix_pattern, names(dirs))]
}

build_peer_review_objective1_output_dirs <- function(test_output_dir) {
    build_subdivided_output_dirs(test_output_dir, "^obj1_")
}

build_peer_review_objective2_output_dirs <- function(test_output_dir) {
    build_subdivided_output_dirs(test_output_dir, "^obj2_")
}

run_objective1_test <- function(data, output_tag = "objective1_peer_review") {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, output_tag)
    output_dirs <- build_peer_review_objective1_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    result <- testthat::expect_no_error(
        run_objective_1(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = output_dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
    )
    list(results = result, output_dirs = output_dirs, test_output_dir = test_output_dir)
}

run_objective2_test <- function(data, output_tag = "objective2_peer_review") {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, output_tag)
    output_dirs <- build_peer_review_objective2_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    result <- testthat::expect_no_error(
        run_objective_2(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = output_dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
    )
    list(results = result, output_dirs = output_dirs, test_output_dir = test_output_dir)
}
