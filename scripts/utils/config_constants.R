# Configuration Constants - Public Entry Point
#
# `scripts/load_all.R` sources this file, and downstream code should continue to
# rely on the objects exposed here rather than sourcing private config modules
# under `scripts/config/`.
# Module order is deterministic because later modules depend on path, shared
# policy, and objective-specific objects defined earlier. Objective-specific
# contracts and policy are kept in modules named for their owning objective.

# Keep model coefficient names stable across all model families.
options(contrasts = c("contr.treatment", "contr.poly"))

CONFIG_MODULES <- c(
    "project_paths.R",
    "data_processing_policy.R",
    "modeling_policy.R",
    "objective0_contracts.R",
    "objective1_contracts.R",
    "objective2_contracts.R",
    "objective3_contracts.R",
    "objective4_policy.R",
    "objective4_contracts.R",
    "labels_display.R"
)

for (config_module in CONFIG_MODULES) {
    source(here::here("scripts", "config", config_module))
}
