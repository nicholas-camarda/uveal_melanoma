#' Install required project packages via pak
#'
#' Bootstraps `pak` when needed, then installs the package set required by
#' `scripts/load_all.R`. This script is intentionally side-effectful and should
#' be run explicitly rather than implicitly during sourcing.

required_packages <- c(
    "tidyverse",
    "readxl",
    "writexl",
    "openxlsx",
    "lubridate",
    "janitor",
    "broom.helpers",
    "parameters",
    "gtsummary",
    "survival",
    "survminer",
    "survRM2",
    "gt",
    "cardx",
    "forestploter",
    "grid",
    "gridExtra",
    "cowplot",
    "ggsurvfit",
    "tidycmprsk",
    "logger",
    "progressr",
    "here",
    "usethis",
    "testthat",
    "rms",
    "pec",
    "survcomp",
    "riskRegression",
    "cmprsk",
    "timeROC",
    "pROC",
    "rmda",
    "VIM",
    "mice",
    "glmnet"
)

if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
}

for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
        next
    }

    tryCatch(
        {
            pak::pkg_install(pkg, ask = FALSE)
        },
        error = function(e1) {
            tryCatch(
                pak::pkg_install(paste0("bioc::", pkg), ask = FALSE),
                error = function(e2) {
                    stop(
                        sprintf(
                            "Failed to install package '%s'. Errors: %s | %s",
                            pkg,
                            conditionMessage(e1),
                            conditionMessage(e2)
                        ),
                        call. = FALSE
                    )
                }
            )
        }
    )
}

message("Bootstrap complete.")
