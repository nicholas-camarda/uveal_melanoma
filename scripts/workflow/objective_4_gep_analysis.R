#' Run Objective 4: GEP Validation
#'
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @param other_map List containing treatment group mappings and categorical variable level mappings for consistent analysis
#' @param confounders Character vector of confounder variables to use for statistical adjustment (not used in GEP analysis)
#' @return List of analysis results
run_objective_4 <- function(data, dataset_name, output_dirs, prefix, other_map = list(), confounders = NULL) {
    step4_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 4: GEP PREDICTIVE ACCURACY VALIDATION", display_name)

    # MFS GEP validation
    log_function("analyze_gep_mfs_validation", "MFS GEP validation analysis")
    mfs_gep_results <- analyze_gep_mfs_validation(data, dataset_name)
    log_enhanced("MFS GEP validation completed", level = "INFO", indent = 1)

    # MSS GEP validation
    log_function("analyze_gep_mss_validation", "MSS GEP validation analysis")
    mss_gep_results <- analyze_gep_mss_validation(data, dataset_name)
    log_enhanced("MSS GEP validation completed", level = "INFO", indent = 1)

    # Simple GEP validation
    log_function("simple_gep_validation", "Simple GEP validation - Actual vs Expected rates")
    # Simple GEP outputs go under the MFS directory for consistency
    simple_gep_results <- simple_gep_validation(data, output_dirs$obj4_mfs, prefix)
    log_enhanced("Simple GEP validation completed", level = "INFO", indent = 1)

    log_section_complete("STEP 4: GEP PREDICTIVE ACCURACY VALIDATION", step4_start_time)

    return(list(
        mfs_gep_results = mfs_gep_results,
        mss_gep_results = mss_gep_results,
        simple_gep_results = simple_gep_results
    ))
}
