# Detailed Function Inventory

## 📁 File-by-File Function Analysis

### 🔧 Core Files

#### `scripts/main.R` (884 lines)
**Functions:**
- `run_objective_1()` - Primary outcomes analysis
- `run_objective_2()` - Secondary outcomes analysis  
- `run_objective_3()` - PFS-2 analysis
- `run_objective_4()` - GEP validation analysis
- `run_my_analysis()` - Main analysis orchestrator
- `main_execution()` - Entry point
- `run_specific_objective()` - Single objective runner

**Status:** ✅ Well organized, clear entry points

#### `scripts/utils/all_helper_functions.R` (288 lines)
**Functions:**
- `use()` - Package loading utility
- `setup_cohort_outputs()` - Output directory setup
- `validate_naming_consistency()` - Naming validation
- `make_filename_safe()` - Filename sanitization

**Status:** ✅ Clean, focused utility functions

#### `scripts/utils/config_constants.R` (337 lines) - REORGANIZED
**Constants:** (Organized by logical sections)
- **Core Data Paths:** DATA_DIR, RAW_DATA_DIR, PROCESSED_DATA_DIR, OUTPUT_DIR
- **Data Processing:** THRESHOLD_RARITY, EXTREME_ESTIMATE_THRESHOLD, etc.
- **Treatment Configuration:** TREATMENT_FACTOR_LEVELS, TREATMENT_LABELS, etc.
- **Analysis Variables:** confounders, subgroup_vars, continuous_subgroup_vars
- **Data Validation:** CRITICAL_VARIABLES, DERIVED_VARIABLES, CRITICAL_FACTORS
- **Variable Labeling:** STANDARD_TABLE_LABELS, BASELINE_VARIABLES_TO_SUMMARIZE
- **Plot Settings:** FOREST_PLOT_WIDTH, SURVIVAL_PLOT_WIDTH, etc.
- **GEP Validation:** GEP_VALIDATION_TIMEPOINTS, GEP_BOOTSTRAP_ITERATIONS, etc.

**Status:** ✅ Single source of truth, well organized

### 📊 Analysis Files

#### `scripts/analysis/statistical_analysis.R` (1097 lines)
**Functions:**
- `analyze_binary_outcome_rates()` - Binary outcome analysis
- `analyze_time_to_event_outcomes()` - Survival analysis
- `plot_rmst_pvalue_progression()` - RMST plotting
- `analyze_pfs2()` - PFS-2 analysis
- `test_proportional_hazards_assumption()` - PH assumption testing

**Status:** ✅ Well organized, core statistical functions

#### `scripts/analysis/subgroup_analysis.R` (1529 lines)
**Functions:**
- `get_cutoff_value()` - Cutoff value retrieval
- `create_clinical_bins()` - Clinical bin creation
- `get_subgroup_levels()` - Subgroup level retrieval
- `analyze_treatment_effect_subgroups_survival()` - Survival subgroup analysis
- `analyze_treatment_effect_subgroups_binary()` - Binary subgroup analysis
- `analyze_treatment_effect_subgroups_height()` - Height subgroup analysis
- `process_subgroup_data()` - Data processing for subgroups
- `fit_subgroup_model()` - Model fitting for subgroups
- `calculate_subgroup_effects()` - Effect calculation
- `format_p_value()` - P-value formatting
- `format_subgroup_analysis_tables()` - Table formatting
- `format_subgroup_analysis_results()` - Results formatting

**Status:** ✅ Comprehensive subgroup analysis with configuration functions

#### `scripts/analysis/gep_validation_analysis.R` (3031 lines) ⚠️ TOO LARGE
**Functions:** (Partial list - file is very large)
- Multiple GEP validation functions
- Plotting functions
- Statistical analysis functions

**Issues:** 
- File is too large (3031 lines)
- Should be split into multiple files
- Hard to maintain and navigate

#### `scripts/analysis/gep_validation_helpers.R` (297 lines)
**Functions:**
- Helper functions for GEP validation

**Status:** Could be merged into main GEP file or kept separate if main file is split

#### `scripts/analysis/tumor_height_analysis.R` (118 lines)
**Functions:**
- `analyze_tumor_height_changes()` - Tumor height analysis

**Status:** ✅ Small, focused file

#### `scripts/analysis/vision_safety_analysis.R` (284 lines)
**Functions:**
- `analyze_visual_acuity_changes()` - Visual acuity analysis
- `analyze_radiation_complications()` - Radiation complications analysis

**Status:** ✅ Well organized, focused functions

### 🗃️ Data Processing Files

#### `scripts/data_helper/data_processing.R` (1249 lines) - UPDATED
**Functions:**
- `get_cohort_info()` - Cohort information retrieval
- `fix_event_date_consistency()` - Date consistency fixing
- `load_and_clean_data()` - Data loading and cleaning
- `create_derived_variables()` - Variable creation
- `apply_criteria()` - Inclusion/exclusion criteria
- `prepare_factor_levels()` - Factor level preparation (UPDATED)
- `create_binned_continuous_variables()` - Binned continuous variable creation for subgroup analysis
- `calculate_treatment_duration_metrics()` - Duration metrics
- `create_analytic_dataset()` - Complete data processing pipeline (NEW)
- `generate_validation_report()` - Validation report generation

**Status:** ✅ Updated with proper rare category handling

#### `scripts/data_helper/data_utilities.R` (525 lines)
**Functions:**
- `handle_rare_categories()` - Rare category collapsing
- `list_available_datasets()` - Dataset listing
- `generate_valid_confounders()` - Confounder generation

**Status:** ✅ Data utility functions

### 🛠️ Utility Files

#### `scripts/utils/logging_utilities.R`
**Functions:**
- `log_enhanced()` - Enhanced logging
- `log_progress()` - Progress logging
- `log_section_start()` - Section start logging
- `log_section_complete()` - Section completion logging
- `log_function()` - Function execution logging

**Status:** ✅ Dedicated logging functions

#### `scripts/utils/validation_utilities.R`
**Functions:**
- `validate_cohort_integrity()` - Cohort validation
- `validate_factor_level_consistency()` - Factor level validation
- `validate_single_cohort_comprehensive()` - Single cohort validation
- `validate_cross_cohort_consistency()` - Cross-cohort validation
- `validate_processed_files_exist()` - File existence validation

**Status:** ✅ Dedicated validation functions

#### `scripts/utils/model_utilities.R`
**Functions:**
- `enforce_unordered_factors()` - Factor enforcement
- `get_variable_labels()` - Variable label retrieval
- `get_treatment_coefficient_name()` - Treatment coefficient naming
- `get_interaction_coefficient_name()` - Interaction coefficient naming

**Status:** ✅ Dedicated model utility functions

#### `scripts/utils/gep_validation_utilities.R`
**Functions:**
- `validate_gep_variables_with_report()` - GEP validation with report
- `validate_gep_variables()` - GEP variable validation

**Status:** ✅ Dedicated GEP validation functions

#### `scripts/utils/table_generation.R` (1746 lines) - UPDATED
**Functions:**
- `get_cohort_specific_other_map()` - Cohort-specific other_map retrieval (UPDATED)
- `create_gtsummary_table()` - gtsummary table creation
- `add_other_level_details()` - Other level details addition
- `add_factor_label_pvalues_to_table()` - Factor label p-values
- `remove_orphaned_variables()` - Orphaned variable removal
- `generate_regression_table()` - Regression table generation
- `create_diagnostics_file()` - Diagnostics file creation

**Status:** ✅ Updated to work with new other_map structure

#### `scripts/utils/output_utilities.R` (404 lines)
**Functions:**
- `create_output_structure()` - Output directory creation
- `list_test_directories()` - Test directory listing
- `create_all_combined_forest_plots()` - Combined forest plot creation

**Status:** ✅ Output utility functions

#### `scripts/utils/extreme_estimate_handling.R` (265 lines)
**Functions:**
- `filter_extreme_estimates()` - Extreme estimate filtering
- `identify_extreme_estimates()` - Extreme estimate identification
- `generate_extreme_estimate_report()` - Extreme estimate reporting

**Status:** ✅ Extreme estimate handling functions

### 🎨 Visualization Files

#### `scripts/visualization/forest_plot.R` (972 lines)
**Functions:**
- `create_single_cohort_forest_plot()` - Single cohort forest plot
- `create_forest_plot_data()` - Forest plot data creation
- `format_forest_plot_data()` - Forest plot data formatting

**Status:** ✅ Forest plot generation functions

### 🛠️ Standalone Tool Scripts

#### `scripts/tools/confounder_analysis.R` (177 lines)
**Functions:**
- Standalone confounder analysis tool (not part of main pipeline)

**Status:** ✅ Properly separated from main pipeline

#### `scripts/tools/derived_variables_documentation.R` (661 lines)
**Functions:**
- Standalone documentation generation tool (not part of main pipeline)

**Status:** ✅ Properly separated from main pipeline

## ✅ IMPROVEMENTS ACHIEVED

### 1. **Eliminated Duplicates**
- ✅ Removed `scripts/utils/analysis_config.R` completely
- ✅ No more duplicate functions across files
- ✅ Single source of truth for configuration

### 2. **Proper Function Distribution**
- ✅ Logging functions in `logging_utilities.R`
- ✅ Validation functions in `validation_utilities.R`
- ✅ Model utilities in `model_utilities.R`
- ✅ GEP validation in `gep_validation_utilities.R`
- ✅ Configuration constants in `config_constants.R`

### 3. **Updated Data Processing**
- ✅ Fixed rare category collapse timing
- ✅ Proper `other_map.rds` structure
- ✅ Correct cohort-specific data extraction

### 4. **Improved Configuration**
- ✅ Reorganized `config_constants.R` with logical sections
- ✅ Consistent treatment group configuration
- ✅ Complete baseline characteristics variables

## 📊 FUNCTION COUNT SUMMARY

### Core Functions: ~25
- Main execution: 7 functions
- Data processing: 10 functions
- Statistical analysis: 5 functions
- Subgroup analysis: 12 functions

### Utility Functions: ~40
- Logging: 5 functions
- Validation: 5 functions
- Model utilities: 4 functions
- Table generation: 7 functions
- Output utilities: 3 functions
- Extreme estimate handling: 3 functions
- GEP validation: 2 functions
- Visualization: 3 functions

### Tool Functions: ~15
- Standalone tools: 15 functions (not part of main pipeline)

### Total Functions: ~80 functions

## 🎯 REMAINING CONSIDERATIONS

### 1. **File Size Issues**
- `gep_validation_analysis.R` (3031 lines) - Needs splitting
- `table_generation.R` (1746 lines) - Consider splitting

### 2. **Documentation**
- Some functions need better roxygen2 documentation
- Consider adding function documentation as needed

### 3. **Testing**
- Ensure all critical functions have tests
- Add tests for new/changed functions

## ✅ OVERALL ASSESSMENT

**Status:** ✅ **SIGNIFICANTLY IMPROVED**

The function inventory is now clean and well-organized:
- ✅ No duplicate functions
- ✅ Proper function distribution
- ✅ Clear separation of concerns
- ✅ Dedicated utility files
- ✅ Updated data processing
- ✅ Consistent configuration

The codebase is now much more maintainable and easier to navigate. 