#' Run Objective 3: Repeat Radiation Efficacy
#'
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @param other_map List. Additional mapping or arguments to pass to the analysis. Default is empty list.
#' @param confounders Character vector of confounders to adjust for in the analysis. Default is NULL.
#' @return List of analysis results
run_objective_3 <- function(data, dataset_name, output_dirs, prefix, other_map = list(), confounders = NULL) {
    step3_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 3: REPEAT RADIATION EFFICACY", display_name)

    # PFS-2 analysis (freedom from second recurrence)
    log_function("analyze_pfs2", "PFS-2 analysis (freedom from second recurrence)")
    pfs2_analysis <- analyze_pfs2(data, confounders, dataset_name, other_map, output_dirs, prefix)
    log_enhanced("PFS-2 analysis completed", level = "INFO", indent = 1)

    log_section_complete("STEP 3: REPEAT RADIATION EFFICACY", step3_start_time)

    return(list(
        pfs2_analysis = pfs2_analysis
    ))
}