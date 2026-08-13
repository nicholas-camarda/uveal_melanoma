test_that("fresh R processes use the lockfile-keyed shared project library", {
    lock_hash <- unname(tools::md5sum(here::here("renv.lock")))
    shared_root <- file.path(
        tools::R_user_dir("renv", "cache"),
        "library",
        "uveal-melanoma",
        lock_hash
    )
    expression <- paste(
        "cat(sprintf('library=%s\\n', renv::paths$library()))",
        "cat(sprintf('testthat=%s\\n', requireNamespace('testthat', quietly = TRUE)))",
        sep = ";"
    )

    output <- withr::with_dir(
        here::here(),
        system2(
            file.path(R.home("bin"), "Rscript"),
            c("-e", shQuote(expression)),
            stdout = TRUE,
            stderr = TRUE
        )
    )
    status <- attr(output, "status")
    if (is.null(status)) {
        status <- 0L
    }
    library_line <- grep("^library=", output, value = TRUE)
    testthat_line <- grep("^testthat=", output, value = TRUE)

    expect_equal(status, 0L, info = paste(output, collapse = "\n"))
    expect_length(library_line, 1L)
    expect_true(
        startsWith(
            sub("^library=", "", library_line),
            normalizePath(shared_root, winslash = "/", mustWork = FALSE)
        ),
        info = paste(output, collapse = "\n")
    )
    expect_identical(testthat_line, "testthat=TRUE")
})
