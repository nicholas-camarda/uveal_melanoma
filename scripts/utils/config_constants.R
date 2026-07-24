# Configuration Constants - Public Entry Point
#
# `scripts/load_all.R` sources this file, and downstream code should continue to
# rely on the objects exposed here rather than sourcing private config modules
# under `scripts/config/`.
# Module order is deterministic because later modules depend on path, factor,
# modeling, and GEP policy objects defined earlier.

# Keep model coefficient names stable across all model families.
options(contrasts = c("contr.treatment", "contr.poly"))

CONFIG_MODULES <- c(
    "project_paths.R",
    "data_processing_policy.R",
    "modeling_policy.R",
    "gep_policy.R",
    "objective0_contracts.R",
    "objective1_contracts.R",
    "labels_display.R"
)

for (config_module in CONFIG_MODULES) {
    source(here::here("scripts", "config", config_module))
}
