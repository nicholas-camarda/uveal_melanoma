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

build_peer_review_objective1_output_dirs <- function(test_output_dir) {
    list(
        obj1_recurrence = file.path(test_output_dir, "01_Efficacy", "a_recurrence"),
        obj1_recurrence_1a1 = file.path(test_output_dir, "01_Efficacy", "a_recurrence", "1a1_recurrence_stratified_os"),
        obj1_recurrence_1a2 = file.path(test_output_dir, "01_Efficacy", "a_recurrence", "1a2_recurrence_stratified_pfs"),
        obj1_mets = file.path(test_output_dir, "01_Efficacy", "b_metastatic_progression"),
        obj1_mets_2a1 = file.path(test_output_dir, "01_Efficacy", "b_metastatic_progression", "2a1_metastasis_stratified_os"),
        obj1_mets_2a2 = file.path(test_output_dir, "01_Efficacy", "b_metastatic_progression", "2a2_metastasis_stratified_pfs"),
        obj1_os = file.path(test_output_dir, "01_Efficacy", "c_overall_survival"),
        obj1_pfs = file.path(test_output_dir, "01_Efficacy", "d_progression_free_survival"),
        obj1_height_primary = file.path(test_output_dir, "01_Efficacy", "e_tumor_height_primary"),
        obj1_height_sensitivity = file.path(test_output_dir, "01_Efficacy", "f_tumor_height_sensitivity"),
        obj1_subgroup_primary = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_primary"),
        obj1_subgroup_sensitivity = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_sensitivity"),
        obj1_forest_plots = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "forest_plots"),
        obj1_ph_diagnostics = file.path(test_output_dir, "01_Efficacy", "h_proportional_hazards_diagnostics")
    )
}

build_peer_review_objective2_output_dirs <- function(test_output_dir) {
    list(
        obj2_vision = file.path(test_output_dir, "02_Safety", "a_vision_changes"),
        obj2_retinopathy = file.path(test_output_dir, "02_Safety", "b_retinopathy"),
        obj2_nvg = file.path(test_output_dir, "02_Safety", "c_neovascular_glaucoma"),
        obj2_srd = file.path(test_output_dir, "02_Safety", "d_serous_retinal_detachment")
    )
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
