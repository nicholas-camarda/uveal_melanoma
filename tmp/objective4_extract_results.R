source("scripts/load_all.R")

cohort_paths <- c(
    full = "~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_full_cohort.rds",
    restricted = "~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_restricted_cohort.rds",
    gksrs_only = "~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_gksrs_only_cohort.rds"
)

dataset_names <- c(
    full = "uveal_melanoma_full_cohort",
    restricted = "uveal_melanoma_restricted_cohort",
    gksrs_only = "uveal_melanoma_gksrs_only_cohort"
)

print_section <- function(label, value) {
    cat("\n--- ", label, " ---\n", sep = "")
    if (inherits(value, "tbl_df")) {
        print(value, n = Inf, width = Inf)
    } else if (inherits(value, "data.frame")) {
        print(value)
    } else {
        print(value)
    }
}

for (nm in names(cohort_paths)) {
    cat("\n==============================\n")
    cat("COHORT:", nm, "\n")
    cat("==============================\n")

    dat <- readRDS(path.expand(cohort_paths[[nm]]))
    res <- run_objective4_mfs_sensitivity_summary(
        data = dat,
        dataset_name = dataset_names[[nm]],
        output_dir = tempdir(),
        prefix = "cli_"
    )

    cat("RETURN_NAMES:", paste(names(res), collapse = "|"), "\n")

    print_section("mfs_followup_sensitivity_names", names(res$mfs_followup_sensitivity))
    print_section("mfs_treatment_mix_sensitivity_names", names(res$mfs_treatment_mix_sensitivity))
    print_section("mfs_repeat_radiation_sensitivity_names", names(res$mfs_repeat_radiation_sensitivity))

    followup_wanted <- c(
        "operational_overall",
        "operational_by_class",
        "operational_by_class_treatment",
        "horizon_overall",
        "horizon_by_class",
        "horizon_by_class_treatment"
    )
    for (item in followup_wanted) {
        if (item %in% names(res$mfs_followup_sensitivity)) {
            print_section(
                paste0("mfs_followup_sensitivity$", item),
                res$mfs_followup_sensitivity[[item]]
            )
        }
    }

    mix_wanted <- c("by_class", "by_class_treatment", "pbt_only")
    for (item in mix_wanted) {
        if (item %in% names(res$mfs_treatment_mix_sensitivity)) {
            print_section(
                paste0("mfs_treatment_mix_sensitivity$", item),
                res$mfs_treatment_mix_sensitivity[[item]]
            )
        }
    }

    repeat_wanted <- c("exposure_summary", "comparison_by_class")
    for (item in repeat_wanted) {
        if (item %in% names(res$mfs_repeat_radiation_sensitivity)) {
            print_section(
                paste0("mfs_repeat_radiation_sensitivity$", item),
                res$mfs_repeat_radiation_sensitivity[[item]]
            )
        }
    }

    print_section("guardrail_notes", res$guardrail_notes)
}
