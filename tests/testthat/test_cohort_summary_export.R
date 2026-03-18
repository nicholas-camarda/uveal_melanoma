
test_that("export_cohort_summary writes JSON, TSV, and text summaries for all cohorts", {
    make_cohort_data <- function(treatment_group, recurrence1, mets_progression, death_event, last_known_alive_date) {
        tibble::tibble(
            treatment_group = factor(treatment_group, levels = c("PBT", "GKSRS")),
            recurrence1 = recurrence1,
            mets_progression = mets_progression,
            death_event = death_event,
            last_known_alive_date = as.Date(last_known_alive_date)
        )
    }

    cohort_list <- list(
        uveal_melanoma_full_cohort = make_cohort_data(
            treatment_group = c("PBT", "PBT", "GKSRS", "GKSRS"),
            recurrence1 = c("Yes", "No", "Yes", "No"),
            mets_progression = c("No", "Yes", "No", "Yes"),
            death_event = c(0, 1, 0, 0),
            last_known_alive_date = c("2025-02-20", "2024-06-01", "2023-01-01", "2025-01-15")
        ),
        uveal_melanoma_restricted_cohort = make_cohort_data(
            treatment_group = c("PBT", "GKSRS", "GKSRS"),
            recurrence1 = c("No", "Yes", "No"),
            mets_progression = c("No", "No", "Yes"),
            death_event = c(0, 0, 1),
            last_known_alive_date = c("2025-02-01", "2022-12-31", "2024-07-01")
        ),
        uveal_melanoma_gksrs_only_cohort = make_cohort_data(
            treatment_group = c("PBT", "GKSRS", "GKSRS"),
            recurrence1 = c("Yes", "No", "No"),
            mets_progression = c("Yes", "Yes", "No"),
            death_event = c(0, 0, 1),
            last_known_alive_date = c("2024-12-15", "2023-04-01", "2024-08-01")
        )
    )

    output_root <- tempfile("cohort-summary-export-")
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

    output_dirs <- list(
        full_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_full", "00_General", "baseline_characteristics")
        ),
        restricted_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_restricted", "00_General", "baseline_characteristics")
        ),
        gksrs_only_cohort = list(
            baseline_characteristics = file.path(output_root, "gksrs", "00_General", "baseline_characteristics")
        )
    )

    json_path <- file.path(output_root, "cohort_summary_statistics.json")

    summary_data <- export_cohort_summary(
        cohort_list = cohort_list,
        removal_log = NULL,
        output_path = json_path,
        output_dirs = output_dirs
    )

    expect_true(file.exists(json_path))

    full_tsv_path <- file.path(output_root, "uveal_full", "00_General", "cohort_summary.tsv")
    full_txt_path <- file.path(output_root, "uveal_full", "00_General", "cohort_summary.txt")
    restricted_tsv_path <- file.path(output_root, "uveal_restricted", "00_General", "cohort_summary.tsv")
    restricted_txt_path <- file.path(output_root, "uveal_restricted", "00_General", "cohort_summary.txt")
    gksrs_tsv_path <- file.path(output_root, "gksrs", "00_General", "cohort_summary.tsv")
    gksrs_txt_path <- file.path(output_root, "gksrs", "00_General", "cohort_summary.txt")

    expect_true(all(file.exists(c(
        full_tsv_path,
        full_txt_path,
        restricted_tsv_path,
        restricted_txt_path,
        gksrs_tsv_path,
        gksrs_txt_path
    ))))

    full_tsv <- readr::read_tsv(full_tsv_path, show_col_types = FALSE)
    gksrs_text <- paste(readLines(gksrs_txt_path), collapse = "\n")

    expect_equal(summary_data$cohorts$full_cohort$outcomes$metastasis, 2)
    expect_equal(summary_data$cohorts$full_cohort$outcomes$lost_to_followup, 1)
    expect_equal(full_tsv$total_n[[1]], 4)
    expect_equal(full_tsv$metastasis_n[[1]], 2)
    expect_equal(full_tsv$lost_to_followup_n[[1]], 1)
    expect_equal(full_tsv$pbt_metastasis_n[[1]], 1)
    expect_equal(full_tsv$gksrs_lost_to_followup_n[[1]], 1)
    expect_match(gksrs_text, "Treatment Arm Details:")
    expect_match(gksrs_text, "Metastasis: 1")
    expect_match(gksrs_text, "Lost to follow-up: 1")
})