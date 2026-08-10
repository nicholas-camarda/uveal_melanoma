skip_if_integration_disabled()
skip_if_local_data_unavailable()

# Load project functions for logging and plotting regression tests

#' Resolve the human-readable text log created by `setup_logging()`
#'
#' @param log_path Character path passed to `setup_logging()`.
#' @return Character path to the mirrored text log.
resolve_text_log_path <- function(log_path) {
    file.path(
        dirname(log_path),
        "txt",
        paste0(tools::file_path_sans_ext(basename(log_path)), ".txt")
    )
}

#' Read text log lines from a configured logging path
#'
#' @param log_path Character path passed to `setup_logging()`.
#' @return Character vector of log lines.
read_text_log_lines <- function(log_path) {
    readLines(resolve_text_log_path(log_path), warn = FALSE)
}

test_that("main_execution clears stale cohort context before global phases", {
    log_path <- tempfile("logging-main-", fileext = ".txt")
    setup_logging(log_path = log_path, level = "INFO", progress = FALSE, context_in_file = TRUE)

    temp_processed_dir <- tempfile("processed-main-")
    dir.create(temp_processed_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(tibble::tibble(id = 1), file.path(temp_processed_dir, "uveal_melanoma_full_cohort.rds"))
    saveRDS(tibble::tibble(id = 2), file.path(temp_processed_dir, "uveal_melanoma_restricted_cohort.rds"))

    old_processed_data_dir <- PROCESSED_DATA_DIR
    old_list_available_datasets <- list_available_datasets
    old_run_objective_0 <- run_objective_0
    old_run_my_analysis <- run_my_analysis
    old_merge_baseline_tables_with_data <- merge_baseline_tables_with_data

    on.exit({
        assign("PROCESSED_DATA_DIR", old_processed_data_dir, envir = .GlobalEnv)
        assign("list_available_datasets", old_list_available_datasets, envir = .GlobalEnv)
        assign("run_objective_0", old_run_objective_0, envir = .GlobalEnv)
        assign("run_my_analysis", old_run_my_analysis, envir = .GlobalEnv)
        assign("merge_baseline_tables_with_data", old_merge_baseline_tables_with_data, envir = .GlobalEnv)
        set_log_context(replace = TRUE)
    }, add = TRUE)

    assign("PROCESSED_DATA_DIR", temp_processed_dir, envir = .GlobalEnv)
    assign("list_available_datasets", function() {
        c("uveal_melanoma_full_cohort.rds", "uveal_melanoma_restricted_cohort.rds")
    }, envir = .GlobalEnv)
    assign("run_objective_0", function() {
        log_phase("DATA PREPROCESSING PHASE")
        list(
            success = TRUE,
            validated_cohorts = c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort"),
            validation_errors = character(),
            created_datasets = character()
        )
    }, envir = .GlobalEnv)
    assign("run_my_analysis", function(dataset_name, objectives_to_run = c(0, 1, 2, 3, 4)) {
        set_log_context(cohort = dataset_name, replace = TRUE)
        list(
            fatal_issues = character(),
            warning_issues = character(),
            run_state = "success",
            had_errors = FALSE,
            had_warnings = FALSE
        )
    }, envir = .GlobalEnv)
    assign("merge_baseline_tables_with_data", function(full_data, restricted_data, gksrs_only_data = NULL) {
        logger::log_info("Mock merge invoked")
        invisible(NULL)
    }, envir = .GlobalEnv)

    set_log_context(
        cohort = "uveal_melanoma_restricted_cohort",
        objective = "objective_4_gep_analysis",
        replace = TRUE
    )

    expect_no_error(main_execution())

    log_lines <- read_text_log_lines(log_path)
    main_phase_line <- log_lines[grepl("=== MAIN EXECUTION PHASE ===", log_lines)][1]
    objective_0_lines <- log_lines[grepl("obj0_data_processing", log_lines)]

    expect_false(grepl("\\[(full|restricted|gksrs)\\]", main_phase_line))
    expect_true(length(objective_0_lines) > 0)
    expect_false(any(grepl("\\[(full|restricted|gksrs)\\]", objective_0_lines)))
})

test_that("run_my_analysis objective 0 logs without cohort context", {
    log_path <- tempfile("logging-obj0-", fileext = ".txt")
    setup_logging(log_path = log_path, level = "INFO", progress = FALSE, context_in_file = TRUE)

    old_run_objective_0 <- run_objective_0
    on.exit({
        assign("run_objective_0", old_run_objective_0, envir = .GlobalEnv)
        set_log_context(replace = TRUE)
    }, add = TRUE)

    assign("run_objective_0", function() {
        log_phase("DATA PREPROCESSING PHASE")
        list(
            success = TRUE,
            validated_cohorts = "uveal_melanoma_restricted_cohort",
            validation_errors = character(),
            created_datasets = character()
        )
    }, envir = .GlobalEnv)

    set_log_context(
        cohort = "uveal_melanoma_restricted_cohort",
        objective = "objective_4_gep_analysis",
        replace = TRUE
    )

    expect_no_error(run_my_analysis("uveal_melanoma_restricted_cohort", objectives_to_run = 0))

    objective_0_lines <- read_text_log_lines(log_path)
    objective_0_lines <- objective_0_lines[grepl("obj0_data_processing", objective_0_lines)]

    expect_true(length(objective_0_lines) > 0)
    expect_false(any(grepl("\\[restricted\\]", objective_0_lines)))
})

test_that("cohort-specific objectives retain cohort tags", {
    log_path <- tempfile("logging-obj1234-", fileext = ".txt")
    setup_logging(log_path = log_path, level = "INFO", progress = FALSE, context_in_file = TRUE)

    dataset_name <- "uveal_melanoma_full_cohort"
    temp_processed_dir <- tempfile("processed-objectives-")
    dir.create(temp_processed_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(tibble::tibble(id = 1), file.path(temp_processed_dir, paste0(dataset_name, ".rds")))

    old_processed_data_dir <- PROCESSED_DATA_DIR
    old_setup_cohort_outputs <- setup_cohort_outputs
    old_validate_naming_consistency <- validate_naming_consistency
    old_run_objective_1 <- run_objective_1
    old_run_objective_2 <- run_objective_2
    old_run_objective_3 <- run_objective_3
    old_run_objective_4 <- run_objective_4
    old_validate_existing_objective0_rds <- validate_existing_objective0_rds

    on.exit({
        assign("PROCESSED_DATA_DIR", old_processed_data_dir, envir = .GlobalEnv)
        assign("setup_cohort_outputs", old_setup_cohort_outputs, envir = .GlobalEnv)
        assign("validate_naming_consistency", old_validate_naming_consistency, envir = .GlobalEnv)
        assign("run_objective_1", old_run_objective_1, envir = .GlobalEnv)
        assign("run_objective_2", old_run_objective_2, envir = .GlobalEnv)
        assign("run_objective_3", old_run_objective_3, envir = .GlobalEnv)
        assign("run_objective_4", old_run_objective_4, envir = .GlobalEnv)
        assign("validate_existing_objective0_rds", old_validate_existing_objective0_rds, envir = .GlobalEnv)
        set_log_context(replace = TRUE)
    }, add = TRUE)

    assign("PROCESSED_DATA_DIR", temp_processed_dir, envir = .GlobalEnv)
    assign("setup_cohort_outputs", function(dataset_name) {
        list(
            prefix = "unit_",
            cohort_base_dir = temp_processed_dir,
            output_dirs = list()
        )
    }, envir = .GlobalEnv)
    assign("validate_naming_consistency", function(dataset_name, prefix, cohort_dir_name) {
        TRUE
    }, envir = .GlobalEnv)
    assign("run_objective_1", function(data, dataset_name, output_dirs, prefix, confounders = confounders) {
        logger::log_info("Objective 1 marker")
        list()
    }, envir = .GlobalEnv)
    assign("run_objective_2", function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
        logger::log_info("Objective 2 marker")
        list()
    }, envir = .GlobalEnv)
    assign("run_objective_3", function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
        logger::log_info("Objective 3 marker")
        list()
    }, envir = .GlobalEnv)
    assign("run_objective_4", function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
        logger::log_info("Objective 4 marker")
        list()
    }, envir = .GlobalEnv)
    # This test isolates logging context and deliberately uses a one-column
    # placeholder RDS; the Objective 0 validation gate has its own contract
    # coverage and is bypassed here so the logging assertions remain focused.
    assign("validate_existing_objective0_rds", function(dataset_name) {
        list(success = TRUE, dataset_name = dataset_name, validation_errors = character())
    }, envir = .GlobalEnv)

    expect_no_error(run_my_analysis(dataset_name, objectives_to_run = c(1, 2, 3, 4)))

    log_lines <- read_text_log_lines(log_path)
    expect_true(any(grepl("\\[full\\] \\[obj1_primary_outcomes\\]", log_lines)))
    expect_true(any(grepl("\\[full\\] \\[obj2_safety_toxicity\\]", log_lines)))
    expect_true(any(grepl("\\[full\\] \\[obj3_repeat_radiation\\]", log_lines)))
    expect_true(any(grepl("\\[full\\] \\[obj4_gep_analysis\\]", log_lines)))
})

test_that("confidence interval helpers suppress profiling messages", {
    test_data <- tibble::tibble(
        outcome = c(0, 0, 0, 1, 1, 1, 1, 0),
        treatment_group = factor(c("PBT", "PBT", "PBT", "PBT", "GKSRS", "GKSRS", "GKSRS", "GKSRS")),
        age_at_diagnosis = c(55, 57, 59, 61, 63, 65, 67, 69),
        sex = factor(c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"))
    )

    model_fit <- glm(outcome ~ treatment_group + age_at_diagnosis, data = test_data, family = binomial())
    table_result <- gtsummary::tbl_regression(model_fit)

    expect_no_message(
        extract_model_confidence_intervals(
            model = model_fit,
            coefficient_names = names(coef(model_fit)),
            model_type = "logistic"
        )
    )

    expect_no_message(
        suppressWarnings(
            create_comprehensive_diagnostics(
                model_fit = model_fit,
                data = test_data,
                outcome_var = "outcome",
                predictor_vars = "treatment_group",
                confounders = "age_at_diagnosis",
                analysis_name = "unit_test_logistic",
                dataset_name = "unit_test_dataset",
                table_result = table_result
            )
        )
    )
})

test_that("representative survival and CIF plots do not emit duplicate-scale messages", {
    km_test_data <- tibble::tibble(
        tt_mets_months = seq(6, 72, by = 6),
        mets_event = rep(c(0, 1), length.out = 12),
        biopsy1_gep = factor(
            rep(
                c("Class 1 PRAME Negative", "Class 1 PRAME Positive", "Class 2 PRAME Negative"),
                each = 4
            )
        )
    )

    expect_no_message(
        suppressWarnings(
            analyze_time_to_event_outcomes(
                data = km_test_data,
                time_var = "tt_mets_months",
                event_var = "mets_event",
                group_var = "biopsy1_gep",
                model_group_var = "biopsy1_gep",
                confounders = NULL,
                ylab = "Metastasis-Free Survival Probability",
                analysis_type = "post_treatment_only",
                dataset_name = "unit_test_duplicate_scale",
                output_dirs = NULL,
                prefix = NULL
            )
        )
    )

    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    cif_test_data <- actual_data %>% dplyr::filter(mss_analysis_eligible)
    cif_output_dir <- tempfile("cif-noise-")
    dir.create(cif_output_dir, recursive = TRUE, showWarnings = FALSE)

    simplified_output_dir <- tempfile("simplified-km-noise-")
    dir.create(simplified_output_dir, recursive = TRUE, showWarnings = FALSE)

    expect_no_message(
        create_mss_cumulative_incidence_curves(
            data = cif_test_data,
            timepoint = 5,
            output_dir = cif_output_dir,
            prefix = "quiet_",
            group_var = "biopsy1_gep"
        )
    )

    expect_no_message(
        suppressWarnings(
            create_mfs_simplified_survival_curves(
                data = actual_data,
                output_dir = simplified_output_dir,
                prefix = "quiet_",
                save_plot = TRUE
            )
        )
    )
})

test_that("quiet table stacking suppresses intentional header mismatch chatter", {
    table_data <- tibble::tibble(
        treatment_group = factor(c("PBT", "PBT", "GKSRS", "GKSRS")),
        binary_outcome = factor(c("Yes", "No", "Yes", "No"))
    )

    tbl_one <- table_data %>%
        gtsummary::tbl_summary(by = treatment_group, include = binary_outcome, missing = "no") %>%
        gtsummary::add_overall() %>%
        gtsummary::modify_header(label = "**Outcome**", stat_0 = "**Overall**")

    tbl_two <- table_data %>%
        gtsummary::tbl_summary(by = treatment_group, include = binary_outcome, missing = "no") %>%
        gtsummary::add_overall() %>%
        gtsummary::modify_header(label = "**Characteristic**", stat_0 = "**Total**")

    expect_no_message(
        combined_tbl <- quiet_tbl_stack(list(tbl_one, tbl_two))
    )

    combined_header <- combined_tbl$table_styling$header
    expect_equal(
        combined_header$label[match("label", combined_header$column)],
        "**Outcome**"
    )
    expect_equal(
        combined_header$label[match("stat_0", combined_header$column)],
        "**Overall**"
    )
})
