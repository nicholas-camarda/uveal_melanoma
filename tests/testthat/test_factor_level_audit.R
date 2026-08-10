test_that("canonical factor expectations cover the core model-facing variables", {
    expectations <- get_canonical_factor_level_expectations()

    expected_names <- c(
        "treatment_group",
        "recurrence1",
        "sex",
        "location",
        "optic_nerve",
        "internal_reflectivity",
        "srf",
        "initial_overall_stage",
        "initial_stage_binary",
        "biopsy1_gep_raw",
        "biopsy1_gep",
        "gep_class_simple",
        "prame_status",
        "gep12_prame_status",
        "exploratory_gep_group",
        "no_gep_group",
        "vision_line_change_bucket",
        "initial_t_stage_simple"
    )

    expect_true(all(expected_names %in% names(expectations)))
    expect_identical(expectations$treatment_group$levels, TREATMENT_FACTOR_LEVELS)
    expect_identical(expectations$srf$levels, YN_DISPLAY_LABELS)
    expect_identical(expectations$initial_t_stage_simple$levels, c("T1", "T2", "T3", "T4"))
})

test_that("Objective 0 rejects unexpected raw predictor values before coercion", {
    expect_error(
        prepare_factor_levels(tibble::tibble(optic_nerve = "Maybe")),
        "Unexpected raw factor values detected before factor coercion"
    )
    expect_error(
        prepare_factor_levels(tibble::tibble(location = "Unexpected location")),
        "Unexpected raw factor values detected before factor coercion"
    )
})

test_that("stable factor coercion preserves existing factor levels", {
    original <- factor(c("Medium", "Low", "High"), levels = c("Low", "Medium", "High"), ordered = TRUE)

    preserved <- coerce_to_factor_preserving_levels(original)
    expect_true(is.factor(preserved))
    expect_false(is.ordered(preserved))
    expect_identical(levels(preserved), levels(original))

    character_values <- c("Zulu", "Alpha", "Mike")
    derived <- coerce_to_factor_preserving_levels(character_values)
    expect_identical(levels(derived), sort(unique(character_values)))
})

test_that("script sources do not contain risky implicit factor releveling", {
    audit <- scan_factor_level_sites(paths = c("scripts"))
    risky_sites <- dplyr::filter(audit, .data$classification == "needs explicit level preservation")

    expect_equal(nrow(risky_sites), 0, info = paste(capture.output(print(risky_sites)), collapse = "\n"))
    expect_false(any(grepl("\\bas\\.factor\\(", audit$code, fixed = FALSE)))
    expect_false(any(grepl("levels\\s*\\(\\s*factor\\s*\\(", audit$code, perl = TRUE)))
})
