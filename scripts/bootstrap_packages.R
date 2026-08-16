#!/usr/bin/env Rscript

# Restore the exact project environment recorded in renv.lock.
# Run this script from the repository root.

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
lockfile <- file.path(project_root, "renv.lock")

if (!file.exists(lockfile)) {
    stop(
        sprintf("renv.lock was not found under %s.", project_root),
        call. = FALSE
    )
}

if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
        "Project activation did not make the locked renv version available.",
        call. = FALSE
    )
}

renv::restore(
    project = project_root,
    prompt = FALSE,
    clean = FALSE
)

status <- renv::status(project = project_root)
if (isTRUE(status$synchronized)) {
    message(
        "Shared lockfile-keyed renv library restored and synchronized: ",
        renv::paths$library(project = project_root)
    )
} else {
    message("renv restore completed; inspect the status above for any local-only differences.")
}
