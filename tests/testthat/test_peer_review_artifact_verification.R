test_that("artifact freshness helper fails on missing files", {
    expect_error(
        expect_artifact_fresh_after(file.path(tempdir(), "missing_peer_review_artifact.xlsx"), Sys.time()),
        regexp = "Missing artifact"
    )
})
