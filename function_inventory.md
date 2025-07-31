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

**Issues:** 
- `setup_cohort_outputs()` should be in `analysis_config.R`
- `validate_naming_consistency()` is duplicated in `analysis_config.R`

### 📊 Analysis Files

#### `scripts/analysis/statistical_analysis.R` (1097 lines)
**Functions:**
- `analyze_binary_outcome_rates()` - Binary outcome analysis
- `analyze_time_to_event_outcomes()` - Survival analysis
- `plot_rmst_pvalue_progression()` - RMST plotting
- `analyze_pfs2()` - PFS-2 analysis
- `test_proportional_hazards_assumption()` - PH assumption testing

**Status:** ✅ Well organized, core statistical functions

#### `scripts/analysis/subgroup_analysis.R` (1440 lines)
**Functions:**
- `analyze_treatment_effect_subgroups_survival()` - Survival subgroup analysis
- `analyze_treatment_effect_subgroups_binary()` - Binary subgroup analysis
- `analyze_treatment_effect_subgroups_height()` - Height subgroup analysis
- `process_subgroup_data()` - Data processing for subgroups
- `fit_subgroup_model()` - Model fitting for subgroups
- `calculate_subgroup_effects()` - Effect calculation
- `format_p_value()` - P-value formatting
- `format_subgroup_analysis_tables()` - Table formatting
- `format_subgroup_analysis_results()` - Results formatting

**Status:** ✅ Comprehensive subgroup analysis

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

#### `scripts/data_helper/data_processing.R` (1246 lines)
**Functions:**
- `get_cohort_info()` - Cohort information retrieval
- `fix_event_date_consistency()` - Date consistency fixing
- `load_and_clean_data()` - Data loading and cleaning
- `create_derived_variables()` - Variable creation
- `apply_criteria()` - Inclusion/exclusion criteria
- `prepare_factor_levels()` - Factor level preparation
- `create_all_subgroup_variables()` - Subgroup variable creation
- `calculate_treatment_duration_metrics()` - Duration metrics
- `create_summary_tables()` - Summary table creation
- `save_cohorts()` - Cohort saving
- `create_analytic_dataset()` - Dataset creation

**Status:** ✅ Core data processing functions

#### `scripts/data_helper/data_utilities.R` (525 lines)
**Functions:**
- `list_available_datasets()` - Dataset listing
- `handle_rare_categories()` - Rare category handling
- `generate_valid_confounders()` - Confounder generation
- `bin_continuous()` - Continuous variable binning
- `summarize_data()` - Data summarization
- `calculate_variable_interaction_pvalue()` - Interaction p-values
- `calculate_variable_overall_significance()` - Overall significance

**Status:** ⚠️ Some functions could be moved to `data_processing.R`

#### `scripts/data_helper/confounder_analysis.R` (177 lines)
**Functions:** None (appears to be a standalone script)

**Status:** ❌ Should be moved to `scripts/analysis/` or `scripts/old/`

### 🛠️ Utility Files

#### `scripts/utils/analysis_config.R` (1542 lines) ⚠️ VERY LARGE
**Functions:**
- `enforce_unordered_factors()` - Factor enforcement
- `log_enhanced()` - Enhanced logging
- `log_progress()` - Progress logging
- `log_section_start()` - Section start logging
- `log_section_complete()` - Section completion logging
- `log_function()` - Function logging
- `validate_cohort_integrity()` - Cohort validation
- `validate_factor_level_consistency()` - Factor level validation
- `validate_naming_consistency()` - Naming validation (duplicate)
- `generate_validation_report()` - Validation report generation
- `ensure_consistent_contrasts()` - Contrast consistency
- `get_treatment_coefficient_name()` - Treatment coefficient names
- `get_interaction_coefficient_name()` - Interaction coefficient names
- `get_variable_labels()` - Variable labels
- `validate_gep_variables_with_report()` - GEP validation with report
- `validate_gep_variables()` - GEP validation

**Issues:**
- File is very large (1542 lines)
- Contains mixed functionality (logging, validation, configuration)
- Should be split into logical sections

#### `scripts/utils/table_generation.R` (1502 lines) ⚠️ LARGE
**Functions:**
- `build_model_formula()` - Formula building
- `fit_regression_model()` - Model fitting
- `create_comprehensive_diagnostics()` - Diagnostic creation
- `get_filtered_variables_from_table()` - Variable filtering
- `get_model_type()` - Model type detection
- `generate_regression_table()` - Regression table generation
- `create_gtsummary_table()` - gtsummary table creation
- `add_factor_label_pvalues_to_table()` - P-value addition
- `remove_orphaned_variables()` - Variable removal
- `add_other_level_details()` - Level detail addition
- `save_table_outputs()` - Table output saving
- `modify_gt_table_pvalues()` - P-value modification
- `format_confidence_intervals_post()` - CI formatting
- `detect_model_type()` - Model type detection
- `calculate_ftest_pvalue()` - F-test p-values
- `calculate_wald_pvalue()` - Wald p-values
- `calculate_factor_label_pvalue()` - Factor label p-values

**Status:** ⚠️ Large file, could be split by functionality

#### `scripts/utils/output_utilities.R` (404 lines)
**Functions:**
- `create_output_structure()` - Output structure creation
- `merge_cohort_tables()` - Table merging
- `create_all_combined_forest_plots()` - Forest plot creation
- `save_gt_html()` - HTML saving
- `clean_table_headers()` - Header cleaning
- `apply_publication_styling()` - Publication styling
- `write_analysis_diagnostics_excel()` - Excel diagnostics

**Status:** ✅ Well organized output utilities

#### `scripts/utils/extreme_estimate_handling.R` (265 lines)
**Functions:**
- `detect_extreme_regression_estimates()` - Extreme estimate detection
- `filter_extreme_estimates_from_table()` - Extreme estimate filtering
- `apply_extreme_estimate_filtering()` - Extreme estimate application

**Status:** ✅ Focused, well-organized

#### `scripts/utils/subgroup_config.R` (88 lines)
**Functions:**
- `get_cutoff_value()` - Cutoff value retrieval
- `create_clinical_bins()` - Clinical bin creation
- `get_subgroup_levels()` - Subgroup level retrieval

**Status:** ⚠️ Very small file, could be merged into `subgroup_analysis.R`

### 🎨 Visualization Files

#### `scripts/visualization/forest_plot.R` (972 lines)
**Functions:**
- Multiple forest plot generation functions

**Status:** ✅ Dedicated visualization file

### 🗑️ Deprecated Files

#### `scripts/old/uveal_melanoma_analysis_DEPRECATED.R`
**Functions:**
- `ensure_consistent_contrasts()` (duplicate)
- `get_treatment_coefficient_name()` (duplicate)
- `get_interaction_coefficient_name()` (duplicate)
- `get_variable_labels()` (duplicate)
- `list_available_datasets()` (duplicate)
- `handle_rare_categories()` (duplicate)
- `generate_valid_confounders()` (duplicate)
- `calculate_rates()` (unused)
- `analyze_survival()` (unused)

**Status:** ❌ Should be deleted - all functions are duplicates or unused

## 🔍 Redundancy Analysis

### Duplicate Functions Found:
1. `validate_naming_consistency()` - 2 locations
2. `ensure_consistent_contrasts()` - 2 locations
3. `get_treatment_coefficient_name()` - 2 locations
4. `get_variable_labels()` - 2 locations
5. `list_available_datasets()` - 2 locations
6. `handle_rare_categories()` - 2 locations
7. `generate_valid_confounders()` - 2 locations

### Similar Function Groups:
1. **Logging functions** in `analysis_config.R` - could be moved to separate file
2. **Validation functions** in `analysis_config.R` - could be moved to separate file
3. **Table generation functions** in `table_generation.R` - could be split by type

## 📊 Summary Statistics

### File Sizes:
- **Largest:** `gep_validation_analysis.R` (3031 lines) ⚠️
- **Second Largest:** `analysis_config.R` (1542 lines) ⚠️
- **Third Largest:** `table_generation.R` (1502 lines) ⚠️
- **Smallest:** `subgroup_config.R` (88 lines) ⚠️

### Function Counts by Category:
- **Analysis Functions:** ~25 functions
- **Data Processing Functions:** ~15 functions
- **Utility Functions:** ~35 functions
- **Configuration Functions:** ~15 functions
- **Visualization Functions:** ~5 functions

### Issues by Priority:
- **High Priority:** 7 duplicate functions, 1 deprecated file
- **Medium Priority:** 3 oversized files, 1 misplaced file
- **Low Priority:** 1 undersized file, function organization

This inventory provides a complete picture of your function landscape and identifies specific areas for cleanup and optimization. 