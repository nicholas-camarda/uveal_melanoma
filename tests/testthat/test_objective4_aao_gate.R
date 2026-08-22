contract_path <- here::here(
    "docs", "maintenance", "objective4_aao_accepted_abstract_contract.yaml"
)
gate_path <- here::here("scripts", "tools", "evaluate_objective4_aao_gate.R")

# A production change that breaks workbook-label parsing, immutable accepted
# values, method validation, conclusion checks, threshold classification, or
# CLI status propagation must make at least one test below fail.

write_aao_candidate_workbook <- function(path,
                                         mfs_auc = 0.686,
                                         mss_auc = 0.663,
                                         surrogate_auc = 0.515,
                                         rates = c(
                                             class1_mfs = 0.029,
                                             class1_mss = 0,
                                             not_tested_mfs = 0.150,
                                             not_tested_mss = 0.091,
                                             failed_mfs = 0.600,
                                             failed_mss = 0.333,
                                             class2_mfs = 0.537,
                                             class2_mss = 0.383
                                         ),
                                         mfs_method = "ipcw_horizon_mfs",
                                         surrogate_metric_status = NA_character_,
                                         surrogate_takeaway = "Clinical features only weakly approximated definitive molecular class.") {
    workbook <- openxlsx::createWorkbook()
    sheets <- list(
        Model_Performance = data.frame(
            model = c(
                "Surrogate Class 2-like",
                "Direct 5-year MFS",
                "Direct 5-year MFS",
                "Direct 60-month melanoma-death cumulative-incidence risk",
                "Direct 60-month melanoma-death cumulative-incidence risk"
            ),
            performance_scope = c(NA, "Overall", "No GEP", "Overall", "No GEP"),
            model_method = c(
                "surrogate_binary",
                mfs_method,
                mfs_method,
                "ipcw_horizon_competing_risk_mss",
                "ipcw_horizon_competing_risk_mss"
            ),
            evaluation_method = c(
                "repeated out-of-fold binary AUC/Brier/calibration",
                rep("outer-training-fold IPCW weighted OOF AUC/Brier/calibration", 4)
            ),
            metric_status = c(surrogate_metric_status, rep("ok", 4)),
            cv_auc = c(surrogate_auc, mfs_auc, mfs_auc, mss_auc, mss_auc),
            stringsAsFactors = FALSE
        ),
        Risk_Ladder_5yr = data.frame(
            group = c("Class 1", "GEP Not Tested", "GEP Failed/Indeterminate", "Class 2"),
            n = c(100L, 80L, 20L, 60L),
            observed_5yr_mfs_event_rate = unname(rates[c(
                "class1_mfs", "not_tested_mfs", "failed_mfs", "class2_mfs"
            )]),
            mfs_observed_method = "kaplan_meier_at_horizon",
            observed_5yr_mss_event_rate = unname(rates[c(
                "class1_mss", "not_tested_mss", "failed_mss", "class2_mss"
            )]),
            mss_observed_method = "aalen_johansen_cif_at_horizon",
            stringsAsFactors = FALSE
        ),
        Start_Here = data.frame(
            row_order = 1:4,
            section = "bottom_line",
            label = paste0("takeaway_", 1:4),
            value = c(
                "Baseline clinical features provided prognostic support for 60-month post-treatment metastasis risk and melanoma-death cumulative-incidence risk.",
                surrogate_takeaway,
                "Do not relabel no-GEP patients into molecular classes based on the surrogate output.",
                "Do not present no-GEP patients as one homogeneous intermediate-risk group; the failed/indeterminate subgroup is higher risk than the larger not-tested subgroup."
            ),
            stringsAsFactors = FALSE
        )
    )
    for (sheet in names(sheets)) {
        openxlsx::addWorksheet(workbook, sheet)
        openxlsx::writeData(workbook, sheet, sheets[[sheet]])
    }
    openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
    invisible(path)
}

run_gate_fixture <- function(workbook, report = tempfile(fileext = ".json")) {
    status <- evaluate_objective4_aao_gate(contract_path, workbook, report)
    list(
        status = status,
        report = jsonlite::read_json(report, simplifyVector = FALSE),
        report_path = report
    )
}

source(gate_path, local = TRUE)

test_that("accepted AAO contract is immutable and complete", {
    expect_true(file.exists(contract_path))
    contract <- yaml::read_yaml(contract_path)

    expect_identical(contract$version, 1L)
    expect_identical(contract$accepted_abstract$id, "30085896")
    expect_identical(contract$accepted_abstract$submitted_cohort_n, 260L)
    expect_false(contract$accepted_abstract$subgroup_counts_reported)
    expect_identical(
        unlist(contract$accepted_abstract$auc, use.names = TRUE),
        c(direct_mfs = 0.686, direct_mss = 0.663, molecular_surrogate = 0.515)
    )
    expect_identical(contract$review_thresholds$absolute_auc_change, 0.02)
    expect_identical(contract$review_thresholds$absolute_rate_change_percentage_points, 5)
    expect_setequal(
        vapply(contract$accepted_abstract$conclusions, `[[`, character(1), "id"),
        c(
            "moderate_direct_prognostic_stratification",
            "failure_to_recover_molecular_class",
            "no_gep_groups_non_homogeneous"
        )
    )
    expect_false("candidate" %in% names(contract))
    expect_false(any(grepl("candidate", names(contract$accepted_abstract), ignore.case = TRUE)))
})

test_that("unchanged accepted values pass the AAO presentation gate", {
    workbook <- write_aao_candidate_workbook(tempfile(fileext = ".xlsx"))

    result <- run_gate_fixture(workbook)

    expect_identical(result$status, "pass")
    expect_identical(result$report$status, "pass")
    expect_length(result$report$reasons, 0L)
    expect_identical(result$report$accepted_abstract_id, "30085896")
    expect_false(any(grepl(
        "patient_id|medical_record|mrn|patient_name|date_of_birth",
        names(result$report),
        ignore.case = TRUE
    )))
})

test_that("numeric changes beyond accepted thresholds require review", {
    auc_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        mfs_auc = 0.707
    )
    auc_result <- run_gate_fixture(auc_book)
    expect_identical(auc_result$status, "review")
    expect_true(any(vapply(auc_result$report$reasons, function(reason) {
        identical(reason$id, "auc_delta_direct_mfs")
    }, logical(1))))

    changed_rates <- c(
        class1_mfs = 0.029,
        class1_mss = 0,
        not_tested_mfs = 0.201,
        not_tested_mss = 0.091,
        failed_mfs = 0.600,
        failed_mss = 0.333,
        class2_mfs = 0.537,
        class2_mss = 0.383
    )
    rate_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        rates = changed_rates
    )
    rate_result <- run_gate_fixture(rate_book)
    expect_identical(rate_result$status, "review")
    expect_true(any(vapply(rate_result$report$reasons, function(reason) {
        identical(reason$id, "rate_delta_not_tested_mfs")
    }, logical(1))))
})

test_that("numeric changes exactly at accepted thresholds still pass", {
    boundary_rates <- c(
        class1_mfs = 0.029,
        class1_mss = 0,
        not_tested_mfs = 0.200,
        not_tested_mss = 0.091,
        failed_mfs = 0.600,
        failed_mss = 0.333,
        class2_mfs = 0.537,
        class2_mss = 0.383
    )
    workbook <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        mfs_auc = 0.706,
        rates = boundary_rates
    )

    result <- run_gate_fixture(workbook)

    expect_identical(result$status, "pass")
})

test_that("ordering and surrogate conclusion reversals fail", {
    reversed_rates <- c(
        class1_mfs = 0.029,
        class1_mss = 0,
        not_tested_mfs = 0.610,
        not_tested_mss = 0.091,
        failed_mfs = 0.590,
        failed_mss = 0.333,
        class2_mfs = 0.537,
        class2_mss = 0.383
    )
    ordering_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        rates = reversed_rates
    )
    ordering_result <- run_gate_fixture(ordering_book)
    expect_identical(ordering_result$status, "fail")
    expect_true(any(vapply(ordering_result$report$reasons, function(reason) {
        identical(reason$id, "ordering_reversal_mfs_failed_vs_not_tested")
    }, logical(1))))

    surrogate_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        surrogate_takeaway = "Clinical features recovered definitive molecular class."
    )
    surrogate_result <- run_gate_fixture(surrogate_book)
    expect_identical(surrogate_result$status, "fail")
    expect_true(any(vapply(surrogate_result$report$reasons, function(reason) {
        identical(reason$id, "conclusion_reversal_failure_to_recover_molecular_class")
    }, logical(1))))
})

test_that("missing or undeclared required methods fail closed", {
    wrong_method_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        mfs_method = "raw_binary_fallback"
    )
    wrong_method <- run_gate_fixture(wrong_method_book)
    expect_identical(wrong_method$status, "fail")
    expect_true(any(vapply(wrong_method$report$reasons, function(reason) {
        identical(reason$id, "required_method_direct_mfs")
    }, logical(1))))

    missing_sheet_book <- write_aao_candidate_workbook(tempfile(fileext = ".xlsx"))
    workbook <- openxlsx::loadWorkbook(missing_sheet_book)
    openxlsx::removeWorksheet(workbook, "Model_Performance")
    openxlsx::saveWorkbook(workbook, missing_sheet_book, overwrite = TRUE)
    missing_sheet <- run_gate_fixture(missing_sheet_book)
    expect_identical(missing_sheet$status, "fail")
    expect_true(any(vapply(missing_sheet$report$reasons, function(reason) {
        identical(reason$id, "candidate_workbook")
    }, logical(1))))

    missing_method_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        mfs_method = NA_character_
    )
    missing_method <- run_gate_fixture(missing_method_book)
    expect_identical(missing_method$status, "fail")
    expect_true(file.exists(missing_method$report_path))

    missing_value_book <- write_aao_candidate_workbook(
        tempfile(fileext = ".xlsx"),
        mfs_auc = NA_real_
    )
    missing_value <- run_gate_fixture(missing_value_book)
    expect_identical(missing_value$status, "fail")
    expect_true(any(vapply(missing_value$report$reasons, function(reason) {
        identical(reason$id, "required_value_direct_mfs")
    }, logical(1))))
})

test_that("CLI exits zero only for pass", {
    pass_book <- write_aao_candidate_workbook(tempfile(fileext = ".xlsx"))
    pass_report <- tempfile(fileext = ".json")
    pass_exit <- system2(
        "Rscript",
        c(gate_path, "--contract", contract_path, "--candidate-workbook", pass_book, "--report", pass_report)
    )
    expect_identical(pass_exit, 0L)
    expect_identical(jsonlite::read_json(pass_report)$status, "pass")

    review_book <- write_aao_candidate_workbook(tempfile(fileext = ".xlsx"), mfs_auc = 0.707)
    review_report <- tempfile(fileext = ".json")
    review_exit <- suppressWarnings(system2(
        "Rscript",
        c(gate_path, "--contract", contract_path, "--candidate-workbook", review_book, "--report", review_report)
    ))
    expect_gt(review_exit, 0L)
    expect_identical(jsonlite::read_json(review_report)$status, "review")

    fail_book <- write_aao_candidate_workbook(tempfile(fileext = ".xlsx"), mfs_method = "raw_binary_fallback")
    fail_report <- tempfile(fileext = ".json")
    fail_exit <- suppressWarnings(system2(
        "Rscript",
        c(gate_path, "--contract", contract_path, "--candidate-workbook", fail_book, "--report", fail_report)
    ))
    expect_gt(fail_exit, 0L)
    expect_identical(jsonlite::read_json(fail_report)$status, "fail")
})
