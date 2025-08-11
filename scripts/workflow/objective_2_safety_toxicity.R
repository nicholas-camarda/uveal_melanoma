#' Run Objective 2: Safety/Toxicity Analysis
#'
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @param other_map List containing treatment group mappings and categorical variable level mappings for consistent analysis
#' @param confounders Character vector of confounder variables to use for statistical adjustment
#' @return List of analysis results
run_objective_2 <- function(data, dataset_name, output_dirs, prefix, other_map = list(), confounders = NULL) {
    step2_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_phase(paste("STEP 2: SAFETY/TOXICITY ANALYSIS", display_name, sep = " - "))

    # Use provided confounders or fall back to global confounders
    if (is.null(confounders)) {
        confounders <- get("confounders", envir = .GlobalEnv)
    }

    # 2a. Vision changes
    logger::log_info(formatted("Executing analyze_visual_acuity_changes: Vision changes analysis", indent = 1))
    vision_changes <- analyze_visual_acuity_changes(data, output_dirs, prefix, other_map)
    logger::log_info(formatted("Vision changes analysis completed", indent = 1))

    # 2b. Radiation complications
    logger::log_info(formatted("Executing analyze_radiation_complications: Radiation complications analysis", indent = 1))

    # Retinopathy
    retinopathy_analysis <- analyze_radiation_complications(data, sequela_type = "retinopathy", confounders, dataset_name, other_map, output_dirs, prefix)
    logger::log_info(formatted("Retinopathy analysis completed", indent = 1))

    # Neovascular glaucoma
    nvg_analysis <- analyze_radiation_complications(data, sequela_type = "nvg", confounders, dataset_name, other_map, output_dirs, prefix)
    logger::log_info(formatted("Neovascular glaucoma analysis completed", indent = 1))

    # Serous retinal detachment
    srd_analysis <- analyze_radiation_complications(data, sequela_type = "srd", confounders, dataset_name, other_map, output_dirs, prefix)
    logger::log_info(formatted("Serous retinal detachment analysis completed", indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 2: SAFETY/TOXICITY ANALYSIS",
        as.numeric(difftime(Sys.time(), step2_start_time, units = "secs"))
    ))

    return(list(
        vision_changes = vision_changes,
        retinopathy_analysis = retinopathy_analysis,
        nvg_analysis = nvg_analysis,
        srd_analysis = srd_analysis
    ))
}
