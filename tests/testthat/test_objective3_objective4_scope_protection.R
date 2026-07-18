test_that("Objective 3 PFS-2 endpoint contract is explicit and isolated from Objective 1 PFS", {
    source_text <- paste(
        readLines(here::here("scripts", "data_helper", "data_derivation.R"), warn = FALSE),
        collapse = "\n"
    )

    expect_true(grepl("pfs2_second_recurrence_observed", source_text, fixed = TRUE))
    expect_true(grepl("recurrence2", source_text, fixed = TRUE))
    expect_true(grepl("pfs2_event", source_text, fixed = TRUE))
    expect_true(grepl("tt_pfs2_months", source_text, fixed = TRUE))
    expect_false(grepl("pfs2_event\\s*=\\s*if_else\\([^\\n]*(mets_event|death_event)", source_text))
})

test_that("Objective 4 GEP MFS/MSS endpoints remain separate from Objective 1 PFS", {
    mfs_text <- paste(
        readLines(here::here("scripts", "gep", "cores", "gep_evaluation_core_mfs.R"), warn = FALSE),
        collapse = "\n"
    )
    mss_text <- paste(
        readLines(here::here("scripts", "gep", "cores", "gep_evaluation_core_mss.R"), warn = FALSE),
        collapse = "\n"
    )

    expect_true(grepl('event_var = "mets_event"', mfs_text, fixed = TRUE))
    expect_true(grepl('melanoma_event_var = "melanoma_death_event"', mss_text, fixed = TRUE))
    expect_true(grepl('competing_event_var = "competing_death_event"', mss_text, fixed = TRUE))
    expect_false(grepl("pfs_event|tt_pfs_months", mfs_text))
    expect_false(grepl("pfs_event|tt_pfs_months", mss_text))
})
