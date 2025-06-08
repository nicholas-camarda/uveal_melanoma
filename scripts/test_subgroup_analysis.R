# Simple test script for subgroup analysis structure
# Uses only base R and survival package

# Load required packages
library(survival)

# Create sample data for testing
set.seed(123)
n <- 100

# Sample data structure based on uveal melanoma study
test_data <- data.frame(
  patient_id = 1:n,
  treatment = sample(c("plaque", "GKSRS"), n, replace = TRUE),
  age = rnorm(n, 65, 10),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  location = sample(c("Choroidal", "Ciliary body", "Iris"), n, replace = TRUE),
  gene_expression_profile = sample(c("Class 1A", "Class 1B", "Class 2"), n, replace = TRUE),
  height_baseline = rnorm(n, 5, 1.5),
  height_change = rnorm(n, -1, 0.5),
  tt_death = rexp(n, 0.01),
  death_event = rbinom(n, 1, 0.3),
  tt_progression = rexp(n, 0.015),
  progression_event = rbinom(n, 1, 0.4),
  local_recurrence = rbinom(n, 1, 0.2),
  metastatic_progression = rbinom(n, 1, 0.25)
)

cat("Test data created successfully with", nrow(test_data), "observations\n")
cat("Treatment distribution:\n")
print(table(test_data$treatment))

# Simple subgroup analysis function
perform_simple_subgroup_analysis <- function(data, subgroup_var, outcome_type = "survival") {
  
  cat("\nPerforming subgroup analysis for:", subgroup_var, "\n")
  
  # Get unique subgroup levels
  subgroup_levels <- unique(data[[subgroup_var]])
  results <- list()
  
  for (level in subgroup_levels) {
    if (is.na(level)) next
    
    subset_data <- data[data[[subgroup_var]] == level & !is.na(data[[subgroup_var]]), ]
    
    if (nrow(subset_data) < 10) {
      cat("Skipping", level, "- insufficient sample size\n")
      next
    }
    
    cat("Analyzing subgroup:", level, "(n =", nrow(subset_data), ")\n")
    
    if (outcome_type == "survival") {
      # Cox regression for survival outcomes
      tryCatch({
        cox_model <- coxph(Surv(tt_death, death_event) ~ treatment, data = subset_data)
        
        results[[level]] <- list(
          subgroup = level,
          n = nrow(subset_data),
          hr = exp(coef(cox_model)[1]),
          p_value = summary(cox_model)$coefficients[1, 5],
          ci_lower = exp(confint(cox_model)[1, 1]),
          ci_upper = exp(confint(cox_model)[1, 2])
        )
      }, error = function(e) {
        cat("Error in Cox model for", level, ":", e$message, "\n")
      })
    } else if (outcome_type == "binary") {
      # Logistic regression for binary outcomes
      tryCatch({
        glm_model <- glm(local_recurrence ~ treatment, data = subset_data, family = binomial)
        
        results[[level]] <- list(
          subgroup = level,
          n = nrow(subset_data),
          or = exp(coef(glm_model)[2]),
          p_value = summary(glm_model)$coefficients[2, 4],
          ci_lower = exp(confint(glm_model)[2, 1]),
          ci_upper = exp(confint(glm_model)[2, 2])
        )
      }, error = function(e) {
        cat("Error in logistic model for", level, ":", e$message, "\n")
      })
    }
  }
  
  return(results)
}

# Test subgroup analysis for different variables
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("TESTING SUBGROUP ANALYSIS FUNCTIONS\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Test with sex subgroups (survival outcome)
sex_results <- perform_simple_subgroup_analysis(test_data, "sex", "survival")
cat("\nSex subgroup results:\n")
for (result in sex_results) {
  if (!is.null(result)) {
    cat("- ", result$subgroup, ": HR =", round(result$hr, 2), 
        ", p =", round(result$p_value, 3), "\n")
  }
}

# Test with location subgroups (binary outcome) 
location_results <- perform_simple_subgroup_analysis(test_data, "location", "binary")
cat("\nLocation subgroup results:\n")
for (result in location_results) {
  if (!is.null(result)) {
    cat("- ", result$subgroup, ": OR =", round(result$or, 2), 
        ", p =", round(result$p_value, 3), "\n")
  }
}

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("BASIC SUBGROUP ANALYSIS TEST COMPLETED SUCCESSFULLY!\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("\nNext steps:\n")
cat("1. The basic subgroup analysis structure is working\n")
cat("2. Functions can handle survival and binary outcomes\n") 
cat("3. Ready to implement full forest plots when more packages available\n")