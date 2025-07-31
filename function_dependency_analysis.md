# Function Dependency Analysis & Codebase Organization Assessment

## 📊 Executive Summary

**Total Functions Identified:** ~80+ functions across 15+ R files
**Main Categories:** Analysis, Data Processing, Utilities, Visualization, Configuration
**Critical Issues Found:** 
- Duplicate functions between files
- Functions in potentially wrong locations
- Some unused functions
- Complex dependency chains

## 🗂️ Current File Organization

### Core Files (High Priority)
- `scripts/main.R` (884 lines) - Main execution script
- `scripts/utils/all_helper_functions.R` (288 lines) - Core utilities and library loading
- `scripts/utils/analysis_config.R` (1542 lines) - Configuration and validation functions

### Analysis Files
- `scripts/analysis/statistical_analysis.R` (1097 lines) - Core statistical analysis
- `scripts/analysis/subgroup_analysis.R` (1440 lines) - Subgroup analysis functions
- `scripts/analysis/gep_validation_analysis.R` (3031 lines) - GEP validation (largest file)
- `scripts/analysis/gep_validation_helpers.R` (297 lines) - GEP helper functions
- `scripts/analysis/tumor_height_analysis.R` (118 lines) - Tumor height analysis
- `scripts/analysis/vision_safety_analysis.R` (284 lines) - Vision safety analysis

### Data Processing Files
- `scripts/data_helper/data_processing.R` (1246 lines) - Main data processing
- `scripts/data_helper/data_utilities.R` (525 lines) - Data utilities
- `scripts/data_helper/confounder_analysis.R` (177 lines) - Confounder analysis

### Utility Files
- `scripts/utils/table_generation.R` (1502 lines) - Table generation utilities
- `scripts/utils/output_utilities.R` (404 lines) - Output utilities
- `scripts/utils/extreme_estimate_handling.R` (265 lines) - Extreme estimate handling
- `scripts/utils/subgroup_config.R` (88 lines) - Subgroup configuration

### Visualization Files
- `scripts/visualization/forest_plot.R` (972 lines) - Forest plot generation

## 🔍 Critical Issues Identified

### 1. **DUPLICATE FUNCTIONS** (High Priority)

#### Duplicate `validate_naming_consistency`:
- **Location 1:** `scripts/utils/all_helper_functions.R` (line 202)
- **Location 2:** `scripts/utils/analysis_config.R` (line 765)
- **Action:** Remove duplicate from `all_helper_functions.R`

#### Duplicate `ensure_consistent_contrasts`:
- **Location 1:** `scripts/utils/analysis_config.R` (line 1288)
- **Location 2:** `scripts/old/uveal_melanoma_analysis_DEPRECATED.R` (line 34)
- **Action:** Remove from deprecated file

#### Duplicate `get_treatment_coefficient_name`:
- **Location 1:** `scripts/utils/analysis_config.R` (line 1313)
- **Location 2:** `scripts/old/uveal_melanoma_analysis_DEPRECATED.R` (line 52)
- **Action:** Remove from deprecated file

#### Duplicate `get_variable_labels`:
- **Location 1:** `scripts/utils/analysis_config.R` (line 1422)
- **Location 2:** `scripts/old/uveal_melanoma_analysis_DEPRECATED.R` (line 141)
- **Action:** Remove from deprecated file

### 2. **FUNCTIONS IN WRONG LOCATIONS** (Medium Priority)

#### Functions that should be moved:

**From `all_helper_functions.R` to `analysis_config.R`:**
- `setup_cohort_outputs` - Configuration function
- `validate_naming_consistency` - Validation function

**From `data_utilities.R` to `data_processing.R`:**
- `list_available_datasets` - Data processing utility
- `handle_rare_categories` - Data processing utility
- `generate_valid_confounders` - Data processing utility

### 3. **POTENTIALLY UNUSED FUNCTIONS** (Need Verification)

#### In `scripts/old/uveal_melanoma_analysis_DEPRECATED.R`:
- All functions in this file are likely unused (deprecated)
- **Action:** Verify and remove if confirmed unused

#### In `scripts/data_helper/confounder_analysis.R`:
- This appears to be a standalone analysis script, not a function library
- **Action:** Move to `scripts/analysis/` or `scripts/old/` if not needed

### 4. **ORGANIZATIONAL ISSUES** (Medium Priority)

#### File Size Issues:
- `gep_validation_analysis.R` (3031 lines) - Too large, should be split
- `analysis_config.R` (1542 lines) - Very large configuration file
- `table_generation.R` (1502 lines) - Large utility file

#### Logical Grouping Issues:
- `subgroup_config.R` (88 lines) - Very small, could be merged into `subgroup_analysis.R`
- `gep_validation_helpers.R` (297 lines) - Could be merged into main GEP file

## 🔗 Dependency Analysis

### Core Dependencies (Main Entry Points):
```
main.R
├── all_helper_functions.R
│   ├── analysis_config.R
│   ├── extreme_estimate_handling.R
│   ├── table_generation.R
│   ├── data_processing.R
│   ├── data_utilities.R
│   ├── output_utilities.R
│   ├── statistical_analysis.R
│   ├── tumor_height_analysis.R
│   ├── vision_safety_analysis.R
│   ├── subgroup_analysis.R
│   ├── gep_validation_analysis.R
│   └── forest_plot.R
```

### Critical Function Dependencies:
```
run_my_analysis() [main.R]
├── run_objective_1() [main.R]
│   ├── analyze_binary_outcome_rates() [statistical_analysis.R]
│   ├── analyze_time_to_event_outcomes() [statistical_analysis.R]
│   ├── analyze_tumor_height_changes() [tumor_height_analysis.R]
│   └── analyze_treatment_effect_subgroups_height() [subgroup_analysis.R]
├── run_objective_2() [main.R]
│   └── analyze_visual_acuity_changes() [vision_safety_analysis.R]
├── run_objective_3() [main.R]
│   └── analyze_pfs2() [statistical_analysis.R]
└── run_objective_4() [main.R]
    └── [GEP validation functions from gep_validation_analysis.R]
```

## 📋 Recommended Actions

### Phase 1: Immediate Cleanup (High Impact, Low Risk)
1. **Remove duplicate functions** from `all_helper_functions.R`
2. **Delete or move** `scripts/old/uveal_melanoma_analysis_DEPRECATED.R`
3. **Move** `confounder_analysis.R` to appropriate location
4. **Consolidate** small files (`subgroup_config.R` into `subgroup_analysis.R`)

### Phase 2: Reorganization (Medium Impact, Medium Risk)
1. **Split** `gep_validation_analysis.R` into smaller modules
2. **Move** configuration functions to appropriate files
3. **Consolidate** data utilities into data processing
4. **Reorganize** `analysis_config.R` into logical sections

### Phase 3: Optimization (Low Impact, Low Risk)
1. **Review** and remove truly unused functions
2. **Optimize** function dependencies
3. **Standardize** function naming conventions
4. **Add** comprehensive documentation

## 🎯 Specific Recommendations

### 1. **Immediate Actions:**
```bash
# Remove duplicate functions
# Move confounder_analysis.R to scripts/analysis/
# Delete deprecated file
# Consolidate subgroup_config.R
```

### 2. **File Consolidation:**
- Merge `subgroup_config.R` → `subgroup_analysis.R`
- Merge `gep_validation_helpers.R` → `gep_validation_analysis.R` (or split main file)
- Move data utilities from `data_utilities.R` to `data_processing.R`

### 3. **Function Relocation:**
- Move `setup_cohort_outputs` from `all_helper_functions.R` to `analysis_config.R`
- Move `validate_naming_consistency` from `all_helper_functions.R` to `analysis_config.R`

### 4. **File Splitting:**
- Split `gep_validation_analysis.R` into:
  - `gep_validation_core.R`
  - `gep_validation_helpers.R`
  - `gep_validation_plots.R`

## 📊 Impact Assessment

### Before Cleanup:
- **Total Functions:** ~80+
- **Duplicate Functions:** 4+ identified
- **Files:** 15+ R files
- **Largest File:** 3031 lines
- **Deprecated Code:** 1 entire file

### After Cleanup:
- **Total Functions:** ~70-75 (removing duplicates and unused)
- **Duplicate Functions:** 0
- **Files:** 12-13 R files (consolidated)
- **Largest File:** ~1500 lines (split large files)
- **Deprecated Code:** 0

## 🚀 Implementation Status

### **Current Status:** ✅ READY TO IMPLEMENT
- **Branch:** `codebase-reorganization` (created from master)
- **Starting Point:** All current changes committed
- **Testing Infrastructure:** Minimal package setup complete
- **Documentation:** Function inventory and dependency analysis complete

### **Implementation Plan:**

#### **PHASE 1: Immediate Cleanup (High Impact, Low Risk)**
- [x] **Step 1.1:** Delete deprecated file (`scripts/old/uveal_melanoma_analysis_DEPRECATED.R`) ✅ COMPLETE
- [x] **Step 1.2:** Split `analysis_config.R` into focused files ✅ COMPLETE
- [ ] **Step 1.3:** Move `confounder_analysis.R` to appropriate location

#### **PHASE 2: File Consolidation (Medium Impact, Medium Risk)**
- [ ] **Step 2.1:** Merge `subgroup_config.R` into `subgroup_analysis.R`
- [ ] **Step 2.2:** Split `gep_validation_analysis.R` into smaller files

#### **PHASE 3: Conservative Optimization (Low Impact, Low Risk)**
- [ ] **Step 3.1:** Review and standardize function organization
- [ ] **Step 3.2:** Final testing and validation

### **Testing Strategy:**
- **Before each change:** Run baseline test
- **After each change:** Verify functionality preserved
- **Rollback plan:** Git commits for each step
- **Documentation:** Update tracking documents after each step

### **Success Criteria:**
- All existing analyses still run without errors
- Same output files are generated
- No functionality is lost
- Codebase is cleaner and more organized
- All files under 500 lines (manageable size)

This analysis provides a roadmap for cleaning up and optimizing your codebase while maintaining functionality. 