# The cache is installed by setup-objective-fixtures.R after production code is
# loaded. Keeping only definitions in this helper makes its suite lifecycle
# explicit and prevents helper sourcing from mutating production entrypoints.
.objective_fixture_entrypoints <- c(
    run_objective_1 = "objective1",
    run_objective_2 = "objective2",
    run_objective_3 = "objective3",
    run_objective_4 = "objective4",
    merge_baseline_tables_with_data = "merged_tables"
)
.objective_fixture_state <- NULL

#' Create isolated state for cached objective fixtures
#'
#' @param target_env Environment containing the production entrypoints.
#' @param entrypoints Named character vector mapping function names to execution
#'   counter keys.
#' @return A private environment containing results, counters, originals, and
#'   wrapper metadata. The entrypoints are not modified until installation.
create_objective_fixture_state <- function(
    target_env = .GlobalEnv,
    entrypoints = .objective_fixture_entrypoints
) {
    if (!is.environment(target_env) ||
        is.null(names(entrypoints)) ||
        any(!nzchar(names(entrypoints))) ||
        any(!nzchar(entrypoints))) {
        stop("Objective fixture entrypoint configuration is invalid.", call. = FALSE)
    }
    missing_entrypoints <- names(entrypoints)[!vapply(
        names(entrypoints),
        exists,
        logical(1),
        envir = target_env,
        inherits = FALSE
    )]
    if (length(missing_entrypoints) > 0L) {
        stop(
            sprintf(
                "Objective fixture entrypoints are missing: %s",
                paste(missing_entrypoints, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    state <- new.env(parent = emptyenv())
    state$target_env <- target_env
    state$entrypoints <- entrypoints
    state$originals <- mget(
        names(entrypoints),
        envir = target_env,
        inherits = FALSE
    )
    state$wrappers <- list()
    state$results <- list()
    state$counts <- setNames(integer(length(entrypoints)), unname(entrypoints))
    state$active <- FALSE
    state
}

#' Return the active suite-scoped objective fixture state
#'
#' @return The active private cache environment.
require_objective_fixture_state <- function() {
    if (is.null(.objective_fixture_state) ||
        !is.environment(.objective_fixture_state) ||
        !isTRUE(.objective_fixture_state$active)) {
        stop(
            paste(
                "Objective fixtures are unavailable outside the suite setup",
                "lifecycle."
            ),
            call. = FALSE
        )
    }
    .objective_fixture_state
}

#' Record one production objective entrypoint execution
#'
#' @param key Character counter key declared by the entrypoint map.
#' @return Invisibly returns the updated integer count.
record_objective_execution <- function(key) {
    state <- require_objective_fixture_state()
    if (!key %in% names(state$counts)) {
        stop(sprintf("Unknown objective execution key: %s", key), call. = FALSE)
    }
    state$counts[[key]] <- state$counts[[key]] + 1L
    invisible(state$counts[[key]])
}

#' Install execution-counting wrappers around production entrypoints
#'
#' @param state Private state returned by `create_objective_fixture_state()`.
#' @return Invisibly returns `state` after installing all wrappers.
install_objective_entrypoint_wrappers <- function(state) {
    if (!is.environment(state) || isTRUE(state$active)) {
        stop("Objective fixture state is invalid or already active.", call. = FALSE)
    }

    installed <- character()
    completed <- FALSE
    on.exit({
        if (!completed) {
            for (function_name in installed) {
                assign(
                    function_name,
                    state$originals[[function_name]],
                    envir = state$target_env
                )
            }
            state$wrappers <- list()
            state$active <- FALSE
        }
    }, add = TRUE)

    for (function_name in names(state$entrypoints)) {
        # local() freezes the original and counter key independently for each
        # wrapper instead of closing over the loop variables by reference.
        wrapper <- local({
            original <- state$originals[[function_name]]
            key <- unname(state$entrypoints[[function_name]])
            wrapper_state <- state
            function(...) {
                wrapper_state$counts[[key]] <-
                    wrapper_state$counts[[key]] + 1L
                original(...)
            }
        })
        state$wrappers[[function_name]] <- wrapper
        assign(function_name, wrapper, envir = state$target_env)
        installed <- c(installed, function_name)
    }
    state$active <- TRUE
    completed <- TRUE
    invisible(state)
}

#' Restore the exact production entrypoints saved before wrapper installation
#'
#' @param state Private objective fixture state.
#' @return Invisibly returns `TRUE` after restoration, including when the state
#'   was already inactive.
restore_objective_entrypoints <- function(state) {
    if (!is.environment(state)) {
        stop("Objective fixture state is invalid.", call. = FALSE)
    }
    if (isTRUE(state$active)) {
        for (function_name in names(state$originals)) {
            assign(
                function_name,
                state$originals[[function_name]],
                envir = state$target_env
            )
        }
        state$active <- FALSE
    }
    invisible(TRUE)
}

#' Initialize the suite-scoped objective fixture cache and wrappers
#'
#' @return Invisibly returns the newly active private cache environment.
initialize_objective_fixture_state <- function() {
    if (!is.null(.objective_fixture_state) &&
        is.environment(.objective_fixture_state) &&
        isTRUE(.objective_fixture_state$active)) {
        restore_objective_entrypoints(.objective_fixture_state)
    }
    state <- create_objective_fixture_state()
    .objective_fixture_state <<- state
    install_objective_entrypoint_wrappers(state)
    invisible(state)
}

#' Return full-pipeline execution counts for the active suite
#'
#' @return Named integer vector containing Objective 1--4 and merged-table
#'   execution counts.
objective_execution_counts <- function() {
    require_objective_fixture_state()$counts
}

#' Tear down the suite cache and restore production entrypoints
#'
#' @param state Private objective fixture state to dispose.
#' @param assert_counts Whether to require exactly one execution of every
#'   configured full-pipeline entrypoint before cleanup.
#' @return Invisibly returns `TRUE`. Cleanup and restoration occur even if the
#'   execution-count assertion fails.
dispose_objective_fixture_state <- function(state, assert_counts = FALSE) {
    if (!is.environment(state)) {
        stop("Objective fixture state is invalid.", call. = FALSE)
    }
    on.exit({
        restore_objective_entrypoints(state)
        state$results <- list()
        state$wrappers <- list()
        state$originals <- list()
    }, add = TRUE)

    if (isTRUE(assert_counts)) {
        expected <- setNames(
            rep(1L, length(state$counts)),
            names(state$counts)
        )
        if (!identical(state$counts, expected)) {
            stop(
                sprintf(
                    "Full-pipeline execution counts rejected: %s",
                    paste(
                        sprintf(
                            "%s=%d",
                            names(state$counts),
                            state$counts
                        ),
                        collapse = ", "
                    )
                ),
                call. = FALSE
            )
        }
    }
    invisible(TRUE)
}

#' Tear down the active suite cache and clear its lifecycle binding
#'
#' @param assert_counts Whether to require exactly one execution of every
#'   configured full-pipeline entrypoint before cleanup.
#' @return Invisibly returns `TRUE`; also clears the active state when count
#'   validation raises an error.
teardown_objective_fixture_state <- function(
    assert_counts = identical(Sys.getenv("OCULAR_PORTABLE_SUITE"), "true")
) {
    state <- .objective_fixture_state
    if (is.null(state)) {
        return(invisible(TRUE))
    }
    on.exit(.objective_fixture_state <<- NULL, add = TRUE)
    dispose_objective_fixture_state(state, assert_counts = assert_counts)
}

#' Create the output-directory subset needed by one objective fixture
#'
#' @param output_tag Directory name below the suite's temporary output root.
#' @param objective_pattern Regular expression selecting objective-specific
#'   entries from `create_output_structure()`.
#' @return List containing the fixture root and selected output directories.
build_objective_fixture_dirs <- function(output_tag, objective_pattern) {
    output_root <- file.path(TEST_OUTPUT_DIR, output_tag)
    output_dirs <- create_output_structure(output_root)
    output_dirs <- output_dirs[grepl(objective_pattern, names(output_dirs))]
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    list(root = output_root, dirs = output_dirs)
}

#' Return the cached deterministic Objective 1 full-pipeline fixture
#'
#' @return List containing Objective 1 results, paths, and synthetic input data.
get_objective1_pipeline <- function() {
    state <- require_objective_fixture_state()
    if (is.null(state$results$objective1)) {
        data <- create_pipeline_test_dataset()
        data$age_at_diagnosis_binned <- factor(
            cut(
                data$age_at_diagnosis,
                breaks = c(-Inf, 40, 50, 60, 70, 80, Inf),
                right = FALSE,
                labels = c(
                    "< 40 years", "40-49 years", "50-59 years",
                    "60-69 years", "70-79 years", "≥ 80 years"
                )
            ),
            levels = c(
                "< 40 years", "40-49 years", "50-59 years",
                "60-69 years", "70-79 years", "≥ 80 years"
            )
        )
        paths <- build_objective_fixture_dirs("objective1_pipeline", "^obj1_")
        results <- run_objective_1(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
        state$results$objective1 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    state$results$objective1
}

#' Return the canonical Objective 1 test fixture
#'
#' @return The cached value from `get_objective1_pipeline()`.
run_objective1_test <- function() {
    get_objective1_pipeline()
}

#' Return the cached deterministic Objective 2 full-pipeline fixture
#'
#' @return List containing Objective 2 results, paths, and synthetic input data.
get_objective2_pipeline <- function() {
    state <- require_objective_fixture_state()
    if (is.null(state$results$objective2)) {
        data <- create_test_dataset()
        paths <- build_objective_fixture_dirs("objective2_pipeline", "^obj2_")
        results <- run_objective_2(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis")
        )
        state$results$objective2 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    state$results$objective2
}

#' Return the cached deterministic Objective 3 full-pipeline fixture
#'
#' @return List containing Objective 3 results, paths, and synthetic input data.
get_objective3_pipeline <- function() {
    state <- require_objective_fixture_state()
    if (is.null(state$results$objective3)) {
        data <- tibble::tibble(
            id = seq_len(12),
            tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13, 15, 17, 19),
            recurrence1_treatment_clean = factor(rep(c("GKSRS", "Plaque"), each = 6)),
            recurrence1_treatment = rep(c("GKSRS", "Plaque"), each = 6),
            treatment_group = factor(rep(c("PBT", "GKSRS"), each = 6)),
            pfs2_event = c(1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L),
            age_at_diagnosis = rep(c(60, 68, 72), length.out = 12),
            sex = factor(c(
                "Male", "Female", "Male", "Female", "Male", "Male",
                "Female", "Male", "Female", "Male", "Female", "Female"
            )),
            location = factor(c(
                "Choroidal", "Ciliary Body", "Choroidal", "Ciliary Body",
                "Choroidal", "Ciliary Body", "Ciliary Body", "Choroidal",
                "Ciliary Body", "Choroidal", "Ciliary Body", "Choroidal"
            ))
        )
        paths <- build_objective_fixture_dirs("objective3_pipeline", "^obj3_")
        results <- run_objective_3(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex", "location")
        )
        state$results$objective3 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    state$results$objective3
}

#' Return the cached deterministic Objective 4 full-pipeline fixture
#'
#' @return List containing Objective 4 results, paths, and synthetic input data.
get_objective4_pipeline <- function() {
    state <- require_objective_fixture_state()
    if (is.null(state$results$objective4)) {
        data <- create_synthetic_ci_dataset()
        paths <- build_objective_fixture_dirs("objective4_pipeline", "^obj4_")
        results <- run_objective_4(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
        state$results$objective4 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    state$results$objective4
}

#' Return the cached merged-table integration artifact
#'
#' @return Path to the generated merged baseline-characteristics workbook.
get_merged_tables_fixture <- function() {
    state <- require_objective_fixture_state()
    if (is.null(state$results$merged_tables)) {
        data <- create_pipeline_test_dataset()
        merge_baseline_tables_with_data(data, data, data)
        artifact <- file.path(MERGED_TABLES_DIR, "merged_baseline_characteristics.xlsx")
        if (!file.exists(artifact)) {
            stop("Merged-table fixture did not create merged_baseline_characteristics.xlsx.")
        }
        state$results$merged_tables <- artifact
    }
    state$results$merged_tables
}
