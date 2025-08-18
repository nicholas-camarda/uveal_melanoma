library(testthat)

# Set working directory to project root for consistent path handling
setwd(dirname(normalizePath(".")))

# Set test environment variables to prevent interference with main project

Sys.setenv(TEST_OUTPUT_DIR = "test_output")

# CRITICAL: Override project constants to prevent test interference
# These must be set BEFORE sourcing load_all.R to prevent directory creation in wrong places
# Use absolute paths from project root to ensure correct location
project_root <- getwd()
Sys.setenv(DATA_DIR = file.path(project_root, "test_output"))

Sys.setenv(OUTPUT_DIR = file.path(project_root, "test_output", "analysis"))
Sys.setenv(RAW_DATA_DIR = file.path(project_root, "test_output", "raw"))
Sys.setenv(TOOLS_OUTPUT_DIR = file.path(project_root, "test_output", "tools"))
Sys.setenv(MERGED_TABLES_DIR = file.path(project_root, "test_output", "merged_tables"))
Sys.setenv(LOGS_DIR = file.path(project_root, "test_output", "logs"))

# Load the project environment with ALL of the variables and functions
# You do not need to load libraries separately
source(here("scripts", "load_all.R"))

# Source the helper file for test data creation
source(here("tests", "testthat", "test_helper_data.R"))

# Run the tests
test_check("ocular-melanoma-analysis")
