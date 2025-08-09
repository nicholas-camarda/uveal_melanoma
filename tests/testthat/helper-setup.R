# testthat helper: central bootstrap per standards
# 1) Set working directory to project root (robust)
find_project_root <- function(start = normalizePath(".")) {
  here <- start
  for (i in 1:10) {
    if (file.exists(file.path(here, "scripts", "utils", "all_helper_functions.R")) &&
        dir.exists(file.path(here, "final_data"))) {
      return(here)
    }
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  stop("Project root not found from ", start)
}

.project_root <- find_project_root()
setwd(.project_root)

# 2) Load project helpers and packages
source("scripts/utils/all_helper_functions.R")

# 2a) Helper to suppress stdout and messages (keeps warnings/errors visible)
quiet_eval <- function(expr) {
  env <- environment()
  invisible(capture.output(
    {
      env$val <- withCallingHandlers(expr, message = function(m) invokeRestart("muffleMessage"))
    },
    type = "output"
  ))
  env$val
}

# 3) Prepare parity environments for table_generation split
orig_env <- new.env(parent = globalenv())
new_env  <- new.env(parent = globalenv())

# Source original monolithic file (baseline)
if (file.exists("scripts/utils/table_generation.R")) {
  source("scripts/utils/table_generation.R", local = orig_env)
}

# Source new modularized files
mod_files <- c(
  "scripts/tables/table_generation_core.R",
  "scripts/tables/table_model_fitting.R",
  "scripts/tables/table_formatting.R",
  "scripts/tables/table_diagnostics.R",
  "scripts/tables/table_io.R"
)
for (f in mod_files) {
  if (file.exists(f)) source(f, local = new_env)
}

# 3b) Prepare parity environments for data_processing split
dp_orig_env <- new.env(parent = globalenv())
dp_new_env  <- new.env(parent = globalenv())

# Source original data processing monolith
if (file.exists("scripts/data_helper/data_processing.R")) {
  source("scripts/data_helper/data_processing.R", local = dp_orig_env)
}

# Source new data processing modules
dp_mod_files <- c(
  "scripts/data_helper/data_loading.R",
  "scripts/data_helper/data_derivation.R",
  "scripts/data_helper/cohort_creation.R",
  "scripts/data_helper/data_summaries.R",
  "scripts/data_helper/cohort_orchestration.R"
)
for (f in dp_mod_files) {
  if (file.exists(f)) source(f, local = dp_new_env)
}

# Prepare parity environments for subgroup_analysis split
sg_orig_env <- new.env(parent = globalenv())
sg_new_env  <- new.env(parent = globalenv())
if (file.exists("scripts/analysis/subgroup_analysis.R")) {
  source("scripts/analysis/subgroup_analysis.R", local = sg_orig_env)
}
for (f in c("scripts/subgroup/subgroup_data_prep.R",
            "scripts/subgroup/subgroup_survival.R",
            "scripts/subgroup/subgroup_binary.R",
            "scripts/subgroup/subgroup_height.R",
            "scripts/subgroup/subgroup_formatting.R")) {
  if (file.exists(f)) source(f, local = sg_new_env)
}

# Prepare parity environments for statistical_analysis split
sa_orig_env <- new.env(parent = globalenv())
sa_new_env  <- new.env(parent = globalenv())

# Source original statistical analysis monolith
if (file.exists("scripts/analysis/statistical_analysis.R")) {
  source("scripts/analysis/statistical_analysis.R", local = sa_orig_env)
}

# Source new statistical analysis modules
sa_mod_files <- c(
  "scripts/analysis/binary_outcomes.R",
  "scripts/analysis/survival_outcomes.R",
  "scripts/analysis/rmst_visualization.R"
)
for (f in sa_mod_files) {
  if (file.exists(f)) source(f, local = sa_new_env)
}

# Prepare parity environments for forest_plot split
fp_orig_env <- new.env(parent = globalenv())
fp_new_env  <- new.env(parent = globalenv())

# Source original forest plot monolith
if (file.exists("scripts/visualization/forest_plot.R")) {
  source("scripts/visualization/forest_plot.R", local = fp_orig_env)
}

# Source new forest plot modules
fp_mod_files <- c(
  "scripts/visualization/forest_plot_data.R",
  "scripts/visualization/forest_plot_draw.R",
  "scripts/visualization/forest_plot_formatting.R"
)
for (f in fp_mod_files) {
  if (file.exists(f)) source(f, local = fp_new_env)
}

# Override processed data directory for both original and modular environments to keep writes in test_output
.test_processed_dir <- file.path("test_output", "processed_data_parity")
if (!dir.exists(.test_processed_dir)) dir.create(.test_processed_dir, recursive = TRUE)
assign("PROCESSED_DATA_DIR", .test_processed_dir, envir = dp_orig_env)
assign("PROCESSED_DATA_DIR", .test_processed_dir, envir = dp_new_env)
assign("PROCESSED_DATA_DIR", .test_processed_dir, envir = .GlobalEnv)

# Also force RAW_DATA_DIR to absolute path so tests are robust to wd changes
.abs_raw_dir <- file.path(.project_root, "final_data", "Original Files")
assign("RAW_DATA_DIR", .abs_raw_dir, envir = dp_orig_env)
assign("RAW_DATA_DIR", .abs_raw_dir, envir = dp_new_env)
assign("RAW_DATA_DIR", .abs_raw_dir, envir = .GlobalEnv)

# 4) Ensure test_output directory exists
if (!dir.exists("test_output")) dir.create("test_output") 

# 5) Prepare end-to-end cleaned input for tests (guarantee consort_group) when raw Excel is present
if (exists("INPUT_FILENAME") && is.function(dp_new_env$load_and_clean_data)) {
  raw_path <- file.path("final_data/Original Files", INPUT_FILENAME)
  if (file.exists(raw_path)) {
    df_clean <- quiet_eval(dp_new_env$load_and_clean_data(INPUT_FILENAME))
    if (nrow(df_clean) > 500) df_clean <- df_clean[sample(seq_len(nrow(df_clean)), 500), , drop = FALSE]
    assign("df_clean", df_clean, envir = .GlobalEnv)
  }
} 

# Ensure fallback analysis directories exist under test_output for any incidental writes
.infer_dirs <- c(
  file.path("test_output", "Analysis", "General", "treatment_duration"),
  file.path("test_output", "Analysis", "General", "baseline_characteristics")
)
invisible(lapply(.infer_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# 7) Helper to load test data
# Prefer real analytic dataset; fallback to synthetic if unavailable
load_test_df <- function() {
  if (exists("df_clean", inherits = TRUE)) {
    return(get("df_clean", inherits = TRUE))
  }
  rds_candidates <- c(
    "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds",
    "final_data/Analytic Dataset/uveal_melanoma_restricted_cohort.rds",
    "final_data/Analytic Dataset/uveal_melanoma_gksrs_only_cohort.rds"
  )
  rds_path <- NULL
  for (p in rds_candidates) {
    if (file.exists(p)) {
      rds_path <- p
      break
    }
  }
  if (!is.null(rds_path)) {
    df <- readRDS(rds_path)
    # Take a small deterministic subset of rows
    if (nrow(df) > 50) df <- df[sample(seq_len(nrow(df)), 50), , drop = FALSE]
    return(df)
  }
  # Fallback synthetic minimal dataset (only used if RDS missing)
  data.frame(
    id = 1:6,
    initial_gk = c("Y", "N", "N", "Y", "N", "N"),
    initial_gk_date = as.Date(c("2020-01-01", NA, NA, "2020-02-01", NA, NA)),
    initial_plaque = c("N", "Y", "N", "N", "Y", "N"),
    initial_plaque_date = as.Date(c(NA, "2020-01-15", NA, NA, "2020-02-10", NA)),
    recurrence1 = c("N", "N", "Y", "N", "Y", "N"),
    recurrence1_date = as.Date(c(NA, NA, "2020-05-01", NA, "2020-04-15", NA)),
    recurrence1_treatment_date = as.Date(c(NA, NA, "2020-05-10", NA, "2020-04-20", NA)),
    recurrence1_treatment = c(NA, NA, "GKSRS", NA, "Enucleation", NA),
    recurrence2 = c("N", "N", "N", "N", "N", "N"),
    recurrence2_date = as.Date(NA),
    recurrence3 = c("N", "N", "N", "N", "N", "N"),
    recurrence3_date = as.Date(NA),
    mets_progression = c("N", "N", "N", "N", "N", "N"),
    mets_progression_date = as.Date(NA),
    enucleation = c("N", "N", "N", "N", "N", "N"),
    enucleation_date = as.Date(NA),
    date_diagnosis = as.Date("2019-01-01") + 0:5,
    dob = as.Date("1970-01-01") + 365 * 0:5,
    last_known_alive_date = as.Date("2021-01-01") + 0:5,
    dod = as.Date(c(NA, NA, NA, NA, NA, NA)),
    initial_tumor_height = c(5, 12, 8, 4, 10, 6),
    initial_tumor_diameter = c(18, 25, 16, 12, 22, 20),
    last_height = c(4, 11, 8, 3, 9, 5),
    recurrence1_pretreatment_height = c(NA, NA, 6, NA, 7, NA),
    initial_overall_stage = factor(c("1", "2A", "2B", "3A", "3B", "4")),
    optic_nerve = c("N", "N", "N", "N", "Y", "N"),
    initial_vision = c(20, 30, 25, 35, 28, 22),
    sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
    location = c("Choroidal", "Ciliary_Body", "Cilio_choroidal", "Conjunctival", "Irido_Ciliary", "Iris"),
    internal_reflectivity = c("Low", "Medium", "High", "Unknown", "Low_Medium", "Medium_High"),
    srf = c("N", "Y", "N", "N", "Y", "N"),
    op = c("N", "N", "Y", "N", "N", "Y"),
    symptoms = c("N", "Y", "N", "Y", "N", "N"),
    vision_loss_blurred_vision = c("N", "N", "Y", "N", "N", "N"),
    visual_field_defect = c("N", "N", "N", "Y", "N", "N"),
    flashes_photopsia = c("N", "Y", "N", "N", "N", "N"),
    floaters = c("N", "N", "Y", "N", "N", "N"),
    pain = c("N", "N", "N", "Y", "N", "N"),
    biopsy1_gep = c("Class_1A_PRAME_negative", "Class_1B_PRAME_positive", "Class_2_PRAME_negative", "Unknown", "Failed", "PRAME_not_reported"),
    biopsy1_gep_mfs = c(0.9, 0.8, 0.6, NA, NA, NA),
    biopsy1_gep_mss = c(0.95, 0.85, 0.7, NA, NA, NA),
    stringsAsFactors = FALSE
  )
}
