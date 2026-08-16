test_that("full objective and merged-table fixtures execute once and are cached", {
    objective1_first <- get_objective1_pipeline()
    objective1_second <- get_objective1_pipeline()
    objective2_first <- get_objective2_pipeline()
    objective2_second <- get_objective2_pipeline()
    objective3_first <- get_objective3_pipeline()
    objective3_second <- get_objective3_pipeline()
    objective4_first <- get_objective4_pipeline()
    objective4_second <- get_objective4_pipeline()
    merged_first <- get_merged_tables_fixture()
    merged_second <- get_merged_tables_fixture()

    expect_identical(objective1_first, objective1_second)
    expect_identical(objective2_first, objective2_second)
    expect_identical(objective3_first, objective3_second)
    expect_identical(objective4_first, objective4_second)
    expect_identical(merged_first, merged_second)
    expect_false(is.null(objective3_first$results$pfs2_analysis$survival_analysis$cox_model))
    expect_length(
        objective3_first$results$pfs2_analysis$survival_analysis$cox_model$perfect_separation_vars,
        0L
    )
    expect_identical(objective4_first$results$run_state, "success")
    expect_length(objective4_first$results$fatal_issues, 0L)
    expect_length(objective4_first$results$warning_issues, 0L)
    expect_identical(
        objective_execution_counts(),
        c(
            objective1 = 1L,
            objective2 = 1L,
            objective3 = 1L,
            objective4 = 1L,
            merged_tables = 1L
        )
    )
})

test_that("Objective 1 test orchestration has one helper-owned entrypoint", {
    expect_true(is.function(run_objective1_test))
    expect_identical(environmentName(environment(run_objective1_test)), "")
    expect_identical(run_objective1_test(), get_objective1_pipeline())
})
