#' Run Objective 3: Repeat Radiation Efficacy
#'
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @param other_map List containing treatment group mappings and categorical variable level mappings for consistent analysis
#' @param confounders Character vector of confounder variables to use for statistical adjustment
#' @return List of analysis results
run_objective_3 <- function(data, dataset_name, output_dirs, prefix, other_map = list(), confounders = NULL) {
    step3_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_phase(paste("STEP 3: REPEAT RADIATION EFFICACY", display_name, sep = " - "))

    # Use provided confounders or fall back to global confounders
    if (is.null(confounders)) {
        confounders <- get("confounders", envir = .GlobalEnv)
    }

    # PFS-2 analysis (freedom from second recurrence)
    logger::log_info(formatted("Executing analyze_pfs2: PFS-2 analysis (freedom from second recurrence)", indent = 1))
    pfs2_analysis <- analyze_pfs2(data, confounders, dataset_name, other_map, output_dirs, prefix)
    logger::log_info(formatted("PFS-2 analysis completed", indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 3: REPEAT RADIATION EFFICACY",
        as.numeric(difftime(Sys.time(), step3_start_time, units = "secs"))
    ))

    return(list(
        pfs2_analysis = pfs2_analysis
    ))
}
