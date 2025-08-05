# Function Dependencies and Global Variables Documentation
**Created**: 2025-08-05 14:13:55
**Source**: scripts/main.R (908 lines)
**Purpose**: Document all dependencies for workflow reorganization

## GLOBAL VARIABLES USED IN MAIN.R

### Configuration Variables (from all_helper_functions.R)
- `USE_LOGS` - Boolean flag to enable/disable logging
- `LOGS_DIR` - Directory path for log files
- `RECREATE_ANALYTIC_DATASETS` - Boolean flag to recreate analytic datasets
- `OUTPUT_DIR` - Base directory for all output files

### Local Variables in main.R
- `temp_output_dirs_by_cohort` - List of output directories organized by cohort
- `expected_cohorts` - Vector of expected cohort names
- `cohort_dir_name` - Directory name for each cohort
- `cohort_base_dir` - Base directory for each cohort
- `simplified_cohort_name` - Simplified name for cohort keys
- `analytic_result` - Result from create_analytic_dataset function
- `final_analytic_datasets` - Final analytic datasets
- `other_map` - Mapping of other variables
- `summary_tables` - Summary tables from data processing

## FUNCTIONS DEFINED IN MAIN.R

### 1. run_objective_1 (Lines 106-600)
**Signature**: `run_objective_1(data, dataset_name, output_dirs, prefix, other_map = list())`
**Purpose**: Primary outcomes analysis
**Parameters**:
- `data` - Dataset for analysis
- `dataset_name` - Name of the dataset
- `output_dirs` - List of output directories organized by analysis type
- `prefix` - Character string prefix for output files (cohort identification)
- `other_map` - List of other variables (default: empty list)

### 2. run_objective_2 (Lines 659-700)
**Signature**: `run_objective_2(data, dataset_name, output_dirs, prefix, other_map = list())`
**Purpose**: Safety/toxicity analysis
**Parameters**: Same as run_objective_1

### 3. run_objective_3 (Lines 705-730)
**Signature**: `run_objective_3(data, dataset_name, output_dirs, prefix, other_map = list())`
**Purpose**: Repeat radiation efficacy analysis
**Parameters**: Same as run_objective_1

### 4. run_objective_4 (Lines 735-760)
**Signature**: `run_objective_4(data, dataset_name, output_dirs, prefix, other_map = list())`
**Purpose**: GEP validation analysis
**Parameters**: Same as run_objective_1

### 5. run_my_analysis (Lines 765-850)
**Signature**: `run_my_analysis(dataset_name, objectives_to_run = c(1, 2, 3, 4))`
**Purpose**: Orchestrate analysis for specific dataset and objectives
**Parameters**:
- `dataset_name` - Name of the dataset to analyze
- `objectives_to_run` - Vector of objective numbers to run (default: all objectives)

### 6. main_execution (Lines 855-908)
**Signature**: `main_execution()`
**Purpose**: Main workflow execution for all datasets
**Parameters**: None

### 7. run_specific_objective (Lines 908+)
**Signature**: `run_specific_objective(dataset_name, objective_number)`
**Purpose**: Run specific objective for specific dataset
**Parameters**:
- `dataset_name` - Name of the dataset
- `objective_number` - Number of objective to run (1-4)

## FUNCTIONS CALLED FROM HELPER FUNCTIONS (all_helper_functions.R)

### Logging Functions
- `log_enhanced()` - Enhanced logging with levels and indentation
- `log_section_start()` - Start logging section
- `log_section_complete()` - Complete logging section
- `log_function()` - Log function execution

### Data Processing Functions
- `create_analytic_dataset()` - Create analytic datasets with full processing pipeline
- `create_output_structure()` - Create output directory structure
- `setup_cohort_outputs()` - Set up cohort-specific output directories

### Analysis Functions
- `analyze_binary_outcome_rates()` - Analyze binary outcome rates
- `analyze_time_to_event_outcomes()` - Analyze time-to-event outcomes
- `analyze_tumor_height_changes()` - Analyze tumor height changes
- `analyze_treatment_effect_subgroups_height()` - Subgroup analysis for tumor height
- `format_subgroup_analysis_tables()` - Format subgroup analysis tables
- `create_forest_plots_height()` - Create forest plots for height analyses
- `primary_outcomes_subgroup_analysis()` - Subgroup analysis for primary outcomes
- `analyze_visual_acuity_changes()` - Analyze visual acuity changes
- `analyze_radiation_complications()` - Analyze radiation complications
- `analyze_pfs2()` - Analyze PFS-2 (freedom from second recurrence)
- `analyze_gep_mfs_validation()` - Analyze MFS GEP validation
- `analyze_gep_mss_validation()` - Analyze MSS GEP validation
- `simple_gep_validation()` - Simple GEP validation

### Utility Functions
- `readRDS()` - Read RDS files
- `file.path()` - Create file paths
- `dir.exists()` - Check if directory exists
- `dir.create()` - Create directories
- `format()` - Format timestamps
- `Sys.time()` - Get current time
- `sprintf()` - String formatting

## DATA STRUCTURES

### Output Directory Structure
The `output_dirs` parameter contains a list with the following structure:
- `obj1_subgroup_primary` - Primary subgroup analysis directory
- `obj1_subgroup_sensitivity` - Sensitivity subgroup analysis directory
- `obj1_forest_plots` - Forest plots directory
- `obj1_subgroup_clinical` - Clinical subgroup analysis directory
- `obj2_*` - Objective 2 directories
- `obj3_*` - Objective 3 directories
- `obj4_*` - Objective 4 directories

### Expected Cohorts
- `uveal_melanoma_full_cohort` - Full cohort (all patients)
- `uveal_melanoma_restricted_cohort` - Restricted cohort (eligible for both treatments)
- `uveal_melanoma_gksrs_only_cohort` - GKSRS-only cohort (ineligible for plaque)

## DEPENDENCIES SUMMARY

### Critical Dependencies
1. **all_helper_functions.R** - Contains all helper functions and global variables
2. **Data files** - Analytic datasets in `final_data/Analytic Dataset/`
3. **Output directories** - Created dynamically based on cohort structure
4. **Logging system** - Optional but integrated throughout

### Migration Requirements
- All functions must be moved EXACTLY as they are
- All global variables must be available from all_helper_functions.R
- All function signatures must remain unchanged
- All parameter documentation must be preserved exactly
- All dependencies must be maintained

## VALIDATION CHECKLIST

### Pre-Migration
- [ ] All global variables documented
- [ ] All function signatures documented
- [ ] All dependencies identified
- [ ] All parameter documentation verified
- [ ] All return values documented

### Post-Migration
- [ ] All functions moved exactly as they are
- [ ] All function signatures unchanged
- [ ] All dependencies maintained
- [ ] All global variables accessible
- [ ] All functionality preserved 