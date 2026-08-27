test_that("configuration modules expose a normalized objective ownership layout", {
    expected_modules <- c(
        "project_paths.R",
        "data_processing_policy.R",
        "modeling_policy.R",
        "objective0_contracts.R",
        "objective1_contracts.R",
        "objective2_contracts.R",
        "objective3_contracts.R",
        "objective4_policy.R",
        "objective4_contracts.R",
        "labels_display.R"
    )
    expect_identical(CONFIG_MODULES, expected_modules)
    expect_true(all(file.exists(here::here("scripts", "config", CONFIG_MODULES))))
    expect_false(file.exists(here::here("scripts", "config", "gep_policy.R")))
})

test_that("objective-specific configuration definitions live in their named modules", {
    config_dir <- here::here("scripts", "config")
    module_text <- function(module) {
        paste(readLines(file.path(config_dir, module), warn = FALSE), collapse = "\n")
    }

    ownership <- list(
        "objective1_contracts.R" = c(
            "OBJECTIVE1_PROPENSITY_DATASET <-",
            "OBJECTIVE1_GENERAL_POP_MEDIAN_AGE_CUTOFF <-",
            "OBJECTIVE1_AGE_SUBGROUP_VAR <-",
            "subgroup_vars <-"
        ),
        "objective2_contracts.R" = c(
            "OBJECTIVE2_TOXICITY_ENDPOINT_MAP <-",
            "OBJECTIVE2_SIMULATED_FISHER_SEED <-"
        ),
        "objective3_contracts.R" = c(
            "OBJECTIVE3_MINIMUM_PFS2_PATIENTS <-",
            "OBJECTIVE3_PFS2_DERIVATION_CONTRACT <-"
        ),
        "objective4_policy.R" = c(
            "GEP_VALIDATION_TIMEPOINTS <-",
            "OBJECTIVE4_GEP_GROUPING <-"
        ),
        "objective4_contracts.R" = c("OBJECTIVE4_GEP_DERIVATION_CONTRACT <-")
    )

    for (module in names(ownership)) {
        text <- module_text(module)
        for (definition in ownership[[module]]) {
            expect_match(text, definition, fixed = TRUE, info = module)
        }
    }

    modeling_text <- module_text("modeling_policy.R")
    objective0_text <- module_text("objective0_contracts.R")
    data_processing_text <- module_text("data_processing_policy.R")
    gep_policy_text <- module_text("objective4_policy.R")

    expect_false(grepl("OBJECTIVE1_PROPENSITY_DATASET <-", modeling_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE1_GENERAL_POP_MEDIAN_AGE_CUTOFF <-", modeling_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE1_AGE_SUBGROUP_VAR <-", modeling_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE2_TOXICITY_ENDPOINT_MAP <-", objective0_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE3_PFS2_DERIVATION_CONTRACT <-", objective0_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE4_GEP_DERIVATION_CONTRACT <-", objective0_text, fixed = TRUE))
    expect_false(grepl("OBJECTIVE3_MINIMUM_PFS2_PATIENTS <-", data_processing_text, fixed = TRUE))
    expect_false(grepl("GEP_OBJECTIVE4_GROUPING <-", gep_policy_text, fixed = TRUE))
})

test_that("normalized objective contracts remain available through the public loader", {
    required_objects <- c(
        "OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES",
        "OBJECTIVE0_DERIVED_OUTPUT_MANIFEST",
        "OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT",
        "OBJECTIVE1_PROPENSITY_DATASET",
        "OBJECTIVE1_GENERAL_POP_MEDIAN_AGE_CUTOFF",
        "OBJECTIVE1_AGE_SUBGROUP_VAR",
        "OBJECTIVE2_TOXICITY_ENDPOINT_MAP",
        "OBJECTIVE2_SIMULATED_FISHER_SEED",
        "OBJECTIVE3_MINIMUM_PFS2_PATIENTS",
        "OBJECTIVE3_PFS2_DERIVATION_CONTRACT",
        "OBJECTIVE4_GEP_GROUPING",
        "OBJECTIVE4_GEP_DERIVATION_CONTRACT",
        "GEP_VALIDATION_TIMEPOINTS"
    )
    expect_true(all(vapply(required_objects, exists, logical(1), inherits = TRUE)))
    expect_false(exists("OBJECTIVE2_TOXICITY_ENDPOINTS", inherits = TRUE))
    expect_false(exists("GEP_OBJECTIVE4_GROUPING", inherits = TRUE))
})
