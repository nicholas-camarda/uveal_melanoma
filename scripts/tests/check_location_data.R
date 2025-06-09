# Check Location Data in Subgroup Results
# Simple script to examine why location data isn't showing in forest plots

cat("=== CHECKING LOCATION DATA IN SUBGROUP RESULTS ===\n")

# Load and examine the structure of the subgroup results
full_results_file <- "final_data/Analysis/uveal_full/tables/primary_outcomes/subgroup_analysis/uveal_full_primary_outcomes_subgroup_results.rds"

if (file.exists(full_results_file)) {
    cat("✓ Results file found\n")
    full_results <- readRDS(full_results_file)
    cat("Available outcomes:", paste(names(full_results), collapse = ", "), "\n")
    
    # Check each outcome for location data
    for (outcome in names(full_results)) {
        cat("\n=== ", toupper(outcome), " ===\n")
        outcome_data <- full_results[[outcome]]
        cat("Variables in", outcome, ":", paste(names(outcome_data), collapse = ", "), "\n")
        
        if ("location" %in% names(outcome_data)) {
            location_data <- outcome_data[["location"]]
            cat("✓ Location data found!\n")
            
            # Check interaction p-value
            if (!is.null(location_data$interaction_p)) {
                cat("- Interaction p-value:", location_data$interaction_p, "\n")
            } else {
                cat("- Interaction p-value: NULL\n")
            }
            
            # Check subgroup effects
            if (!is.null(location_data$subgroup_effects)) {
                cat("- Subgroup effects rows:", nrow(location_data$subgroup_effects), "\n")
                if (nrow(location_data$subgroup_effects) > 0) {
                    cat("- Subgroup effects columns:", paste(names(location_data$subgroup_effects), collapse = ", "), "\n")
                    cat("- First few subgroup levels:\n")
                    print(head(location_data$subgroup_effects[, c("subgroup_level", "effect_estimate")], 3))
                }
            } else {
                cat("- Subgroup effects: NULL\n")
            }
            
            # Check for error field
            if (!is.null(location_data$error)) {
                cat("❌ Error found:", location_data$error, "\n")
            }
            
        } else {
            cat("❌ NO location data found in", outcome, "\n")
        }
    }
} else {
    cat("❌ Results file not found:", full_results_file, "\n")
    
    # Check what files do exist
    cat("\nChecking what files exist in the directory...\n")
    subgroup_dir <- "final_data/Analysis/uveal_full/tables/primary_outcomes/subgroup_analysis/"
    if (dir.exists(subgroup_dir)) {
        files <- list.files(subgroup_dir, pattern = "\\.rds$")
        cat("Available RDS files:\n")
        for (file in files) {
            cat("-", file, "\n")
        }
    } else {
        cat("Subgroup analysis directory not found:", subgroup_dir, "\n")
    }
}

cat("\n=== DONE ===\n") 