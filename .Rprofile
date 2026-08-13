local({
    lockfile <- normalizePath("renv.lock", winslash = "/", mustWork = TRUE)
    lock_hash <- unname(tools::md5sum(lockfile))
    if (is.na(lock_hash) || !nzchar(lock_hash)) {
        stop("Could not hash renv.lock for project library activation.", call. = FALSE)
    }

    Sys.setenv(
        RENV_PATHS_LIBRARY = file.path(
            tools::R_user_dir("renv", "cache"),
            "library",
            "uveal-melanoma",
            lock_hash
        )
    )
})

source("renv/activate.R")
