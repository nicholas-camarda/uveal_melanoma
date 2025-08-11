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
    log_phase(paste("STEP 4: GEP PREDICTIVE ACCURACY VALIDATION", display_name, sep = " - "))

    # Fail-fast validation of GEP variables before any analyses
    val <- validate_gep_variables_with_report(data)
    if (!isTRUE(val$validation_passed)) {
        logger::log_error(formatted("GEP variable validation failed. Missing or inconsistent variables detected.", indent = 1))
        if (!is.null(val$detailed_results)) {
            if (!is.null(val$detailed_results$missing_variables) && length(val$detailed_results$missing_variables) > 0) {
                logger::log_error(formatted(sprintf("Missing variables: %s", paste(val$detailed_results$missing_variables, collapse = ", ")), indent = 2))
            }
        }
        stop("Objective 4 cannot proceed: GEP variable validation failed.")
    }

    # MFS GEP validation
    logger::log_info(formatted("Executing analyze_gep_mfs_validation: MFS GEP validation analysis", indent = 1))
    mfs_gep_results <- analyze_gep_mfs_validation(data, dataset_name)
    logger::log_info(formatted("MFS GEP validation completed", indent = 1))

    # MSS GEP validation
    logger::log_info(formatted("Executing analyze_gep_mss_validation: MSS GEP validation analysis", indent = 1))
    mss_gep_results <- analyze_gep_mss_validation(data, dataset_name)
    logger::log_info(formatted("MSS GEP validation completed", indent = 1))

    # Unified summary and visuals (only once, with both results)
    gep_base_dir <- dirname(output_dirs$obj4_mfs)
    tryCatch({
        create_unified_gep_validation_summary(
            mfs_results = mfs_gep_results,
            mss_results = mss_gep_results,
            dataset_name = dataset_name,
            output_dir = gep_base_dir,
            prefix = prefix
        )
        # Unified artifacts only; no post-hoc file moving
    }, error = function(e) {
        logger::log_warn(sprintf("Unified summary creation/organization failed: %s", e$message))
    })

    # Simple GEP validation
    logger::log_info(formatted("Executing simple_gep_validation: Simple GEP validation - Actual vs Expected rates", indent = 1))
    # Simple GEP outputs: PNGs to MFS/MSS, combined report/XLSX to unified_summary
    simple_gep_results <- simple_gep_validation(data, output_dirs, prefix)
    logger::log_info(formatted("Simple GEP validation completed", indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 4: GEP PREDICTIVE ACCURACY VALIDATION",
        as.numeric(difftime(Sys.time(), step4_start_time, units = "secs"))
    ))

    return(list(
        mfs_gep_results = mfs_gep_results,
        mss_gep_results = mss_gep_results,
        simple_gep_results = simple_gep_results
    ))
}
