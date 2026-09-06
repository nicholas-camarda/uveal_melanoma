test_that("Objective 0 orchestration delegates cohort persistence to save_cohorts", {
    orchestration_lines <- readLines(
        here::here("scripts", "data_helper", "cohort_orchestration.R"),
        warn = FALSE
    )

    expect_length(grep(
        "save_cohorts\\(factored_filtered_data\\)",
        orchestration_lines
    ), 1)
    expect_false(any(grepl(
        "write_readable_xlsx\\(factored_filtered_data",
        orchestration_lines
    )))
    expect_false(any(grepl(
        "saveRDS\\(factored_filtered_data",
        orchestration_lines
    )))

    load_all_lines <- readLines(
        here::here("scripts", "load_all.R"),
        warn = FALSE
    )
    source_lines <- load_all_lines[grepl("^source\\(", load_all_lines)]
    expect_lt(
        which(grepl("cohort_creation\\.R", source_lines)),
        which(grepl("cohort_orchestration\\.R", source_lines))
    )
})

test_that("validation_utilities retains only its unique public helper", {
    utility_lines <- readLines(
        here::here("scripts", "utils", "validation_utilities.R"),
        warn = FALSE
    )
    defined_functions <- sub(
        "^([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function.*$",
        "\\1",
        utility_lines[grepl(
            "^([A-Za-z][A-Za-z0-9_.]*)\\s*<-\\s*function",
            utility_lines
        )]
    )

    expect_identical(defined_functions, "get_expected_analytic_cohort_names")
})
