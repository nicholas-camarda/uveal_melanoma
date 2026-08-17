  # Load all functions and packages using the proper R-style.md approach

  # Load everything - functions AND packages

  parse_nonempty_json_lines <- function(path) {
    lines <- readLines(path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines))]
    jsonlite::stream_in(textConnection(paste(lines, collapse = "\n")), verbose = FALSE)
  }

  test_that("Objective 4 logs produce valid JSON entries with context and idempotent setup", {
    dataset <- "uveal_melanoma_full_cohort"

    log_file <- tempfile(fileext = ".log")
    setup_logging(log_path = log_file, level = "INFO", progress = FALSE, context_in_file = TRUE)
    invisible(with_log_context(cohort = dataset, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
      get_actual_objective4_pipeline()
    }))

    # Re-initialize to test idempotence
    setup_logging(log_path = log_file, level = "INFO", progress = FALSE, context_in_file = TRUE)
    logger::log_info("Idempotence check")

    expect_true(file.exists(log_file))
    df <- parse_nonempty_json_lines(log_file)
    # Basic structure checks
    expect_true(all(c("timestamp", "level_text", "level_num", "message", "cohort", "objective", "subobjective") %in% names(df)))
    expect_true(any(grepl("GEP MSS validation analysis completed successfully|GEP MFS validation analysis completed successfully", df$message)))
    expect_true(any(df$cohort == dataset))
    expect_true(any(df$objective == "objective_4_gep_analysis"))
  })
