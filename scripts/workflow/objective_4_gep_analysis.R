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

    # MFS GEP validation
    logger::log_info(formatted("Executing analyze_gep_mfs_validation: MFS GEP validation analysis", indent = 1))
    mfs_gep_results <- tryCatch({
        analyze_gep_mfs_validation(
            data = data,
            dataset_name = dataset_name,
            other_map = other_map,
            output_dirs = output_dirs,
            prefix = prefix
        )
    }, error = function(e) {
        logger::log_error(formatted(sprintf("MFS GEP validation failed: %s", e$message), indent = 2))
        logger::log_error(formatted("This will prevent complete GEP analysis completion", indent = 2))
        # Return NULL to allow the function to continue
        NULL
    })
    
    if (is.null(mfs_gep_results)) {
        logger::log_warn(formatted("MFS analysis failed - GEP analysis will be incomplete", indent = 1))
    } else {
        logger::log_info(formatted("MFS GEP validation completed", indent = 1))
    }

    # MSS GEP validation
    logger::log_info(formatted("Executing analyze_gep_mss_validation: MSS GEP validation analysis", indent = 1))
    mss_gep_results <- tryCatch({
        analyze_gep_mss_validation(
            data = data,
            dataset_name = dataset_name,
            other_map = other_map,
            output_dirs = output_dirs,
            prefix = prefix
        )
    }, error = function(e) {
        logger::log_error(formatted(sprintf("MSS GEP validation failed: %s", e$message), indent = 2))
        logger::log_error(formatted("This will prevent complete GEP analysis completion", indent = 2))
        # Return NULL to allow the function to continue
        NULL
    })
    
    if (is.null(mss_gep_results)) {
        logger::log_warn(formatted("MSS analysis failed - GEP analysis will be incomplete", indent = 1))
    } else {
        logger::log_info(formatted("MSS GEP validation completed", indent = 1))
    }

    exploratory_no_gep_results <- NULL
    if (identical(dataset_name, "uveal_melanoma_full_cohort")) {
        logger::log_info(formatted("Executing exploratory no-GEP summary integration for the full cohort", indent = 1))
        exploratory_no_gep_results <- tryCatch({
            collected_no_gep_results <- collect_exploratory_no_gep_analysis(
                data = data,
                dataset_name = dataset_name,
                verify_km_fix = FALSE
            )
            run_exploratory_no_gep_report(
                dataset_name = dataset_name,
                output_dir = file.path(dirname(output_dirs$obj4_mfs), "d_exploratory_no_gep"),
                verify_km_fix = FALSE,
                data = data,
                collected_results = collected_no_gep_results
            )
        }, error = function(e) {
            logger::log_warn(formatted(sprintf("Exploratory no-GEP integration failed: %s", e$message), indent = 2))
            NULL
        })
    }

    # Unified summary and visuals (only once, with both results)
    gep_base_dir <- dirname(output_dirs$obj4_mfs)
    tryCatch({
        create_unified_gep_validation_summary(
            mfs_results = mfs_gep_results,
            mss_results = mss_gep_results,
            no_gep_results = exploratory_no_gep_results,
            output_dir = gep_base_dir,
            prefix = prefix
        )
        # Create unified visualization (no survival curves here; per-outcome only)
        create_unified_gep_visuals(
            mfs_results = mfs_gep_results,
            mss_results = mss_gep_results,
            output_dir = file.path(gep_base_dir, "unified_summary"),
            prefix = prefix
        )
        # Unified artifacts only; no post-hoc file moving
    }, error = function(e) {
        logger::log_warn(sprintf("Unified summary creation/organization failed: %s", e$message))
    })

    # Simple GEP validation
    logger::log_info(formatted("Executing simple_gep_validation: Simple GEP validation - Actual vs Expected rates", indent = 1))
    # Simple GEP outputs: PNGs to MFS/MSS, combined report/XLSX to unified_summary
    simple_gep_results <- simple_gep_validation(data, output_dirs, prefix, dataset_name = dataset_name)
    logger::log_info(formatted("Simple GEP validation completed", indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 4: GEP PREDICTIVE ACCURACY VALIDATION",
        as.numeric(difftime(Sys.time(), step4_start_time, units = "secs"))
    ))

    return(list(
        mfs_gep_results = mfs_gep_results,
        mss_gep_results = mss_gep_results,
        simple_gep_results = simple_gep_results,
        exploratory_no_gep_results = exploratory_no_gep_results
    ))
}
