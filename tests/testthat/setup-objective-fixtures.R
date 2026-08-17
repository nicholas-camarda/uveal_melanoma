# Install the suite-owned cache only after setup-bootstrap.R has loaded the
# production entrypoints. The teardown handle is guaranteed to run when the
# serial test lifecycle exits because of success, failure, error, or warning.
initialize_objective_fixture_state()
withr::defer(
    teardown_objective_fixture_state(),
    envir = testthat::teardown_env()
)
