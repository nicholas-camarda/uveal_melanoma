test_that("peer-review response document avoids committed source-machine absolute paths", {
    response_path <- testthat::test_path("../../docs/peer_review_revision_response.md")
    if (!file.exists(response_path)) {
        skip("Response document is created later in the revision plan.")
    }
    expect_no_reviewer_facing_paths(response_path)
})

test_that("artifact freshness helper fails on missing files", {
    expect_error(
        expect_artifact_fresh_after(file.path(tempdir(), "missing_peer_review_artifact.xlsx"), Sys.time()),
        regexp = "Missing artifact"
    )
})
