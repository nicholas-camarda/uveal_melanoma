.objective_fixture_state <- new.env(parent = emptyenv())
.objective_fixture_state$results <- list()
.objective_fixture_state$counts <- setNames(
    integer(5),
    c("objective1", "objective2", "objective3", "objective4", "merged_tables")
)

objective_execution_counts <- function() {
    .objective_fixture_state$counts
}

record_objective_execution <- function(key) {
    .objective_fixture_state$counts[[key]] <-
        .objective_fixture_state$counts[[key]] + 1L
}

wrap_objective_entrypoint <- function(function_name, key) {
    original <- get(function_name, envir = .GlobalEnv, inherits = FALSE)
    wrapper <- function(...) {
        record_objective_execution(key)
        original(...)
    }
    environment(wrapper) <- environment()
    assign(function_name, wrapper, envir = .GlobalEnv)
}

wrap_objective_entrypoint("run_objective_1", "objective1")
wrap_objective_entrypoint("run_objective_2", "objective2")
wrap_objective_entrypoint("run_objective_3", "objective3")
wrap_objective_entrypoint("run_objective_4", "objective4")
wrap_objective_entrypoint("merge_baseline_tables_with_data", "merged_tables")

build_objective_fixture_dirs <- function(output_tag, objective_pattern) {
    output_root <- file.path(TEST_OUTPUT_DIR, output_tag)
    output_dirs <- create_output_structure(output_root)
    output_dirs <- output_dirs[grepl(objective_pattern, names(output_dirs))]
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    list(root = output_root, dirs = output_dirs)
}

get_objective1_pipeline <- function() {
    if (is.null(.objective_fixture_state$results$objective1)) {
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
        .objective_fixture_state$results$objective1 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    .objective_fixture_state$results$objective1
}

run_objective1_test <- function() {
    get_objective1_pipeline()
}

get_objective2_pipeline <- function() {
    if (is.null(.objective_fixture_state$results$objective2)) {
        data <- create_test_dataset()
        paths <- build_objective_fixture_dirs("objective2_pipeline", "^obj2_")
        results <- run_objective_2(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis")
        )
        .objective_fixture_state$results$objective2 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    .objective_fixture_state$results$objective2
}

get_objective3_pipeline <- function() {
    if (is.null(.objective_fixture_state$results$objective3)) {
        data <- tibble::tibble(
            id = seq_len(12),
            tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13, 15, 17, 19),
            recurrence1_treatment_clean = factor(rep(c("GKSRS", "Plaque"), each = 6)),
            recurrence1_treatment = rep(c("GKSRS", "Plaque"), each = 6),
            treatment_group = factor(rep(c("PBT", "GKSRS"), each = 6)),
            pfs2_event = c(1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L),
            age_at_diagnosis = rep(c(60, 68, 72), length.out = 12),
            sex = factor(rep(c("Male", "Female"), length.out = 12))
        )
        paths <- build_objective_fixture_dirs("objective3_pipeline", "^obj3_")
        results <- run_objective_3(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = character()
        )
        .objective_fixture_state$results$objective3 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    .objective_fixture_state$results$objective3
}

get_objective4_pipeline <- function() {
    if (is.null(.objective_fixture_state$results$objective4)) {
        data <- create_synthetic_ci_dataset()
        paths <- build_objective_fixture_dirs("objective4_pipeline", "^obj4_")
        results <- run_objective_4(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = paths$dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
        .objective_fixture_state$results$objective4 <- list(
            results = results,
            output_dirs = paths$dirs,
            test_output_dir = paths$root,
            input_data = data
        )
    }
    .objective_fixture_state$results$objective4
}

get_merged_tables_fixture <- function() {
    if (is.null(.objective_fixture_state$results$merged_tables)) {
        data <- create_pipeline_test_dataset()
        merge_baseline_tables_with_data(data, data, data)
        artifact <- file.path(MERGED_TABLES_DIR, "merged_baseline_characteristics.xlsx")
        if (!file.exists(artifact)) {
            stop("Merged-table fixture did not create merged_baseline_characteristics.xlsx.")
        }
        .objective_fixture_state$results$merged_tables <- artifact
    }
    .objective_fixture_state$results$merged_tables
}

withr::defer(
    {
        if (identical(Sys.getenv("OCULAR_PORTABLE_SUITE"), "true")) {
            expected <- setNames(
                rep(1L, 5),
                c("objective1", "objective2", "objective3", "objective4", "merged_tables")
            )
            if (!identical(objective_execution_counts(), expected)) {
                stop(sprintf(
                    "Full-pipeline execution counts rejected: %s",
                    paste(
                        sprintf(
                            "%s=%d",
                            names(objective_execution_counts()),
                            objective_execution_counts()
                        ),
                        collapse = ", "
                    )
                ), call. = FALSE)
            }
        }
    },
    envir = testthat::teardown_env()
)
