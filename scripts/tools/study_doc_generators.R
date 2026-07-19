# Generated study documentation helpers

extract_load_all_sources <- function(load_all_path = here::here("scripts", "load_all.R")) {
    load_all_lines <- readLines(load_all_path, warn = FALSE)
    source_lines <- grep("^source\\(", trimws(load_all_lines), value = TRUE)

    extracted_paths <- lapply(source_lines, function(line) {
        match <- regexec('source\\((?:here|here::here)\\((.*)\\)\\)', line)
        pieces <- regmatches(line, match)[[1]]
        if (length(pieces) < 2) {
            return(character())
        }

        args <- strsplit(pieces[[2]], ",")[[1]]
        args <- trimws(args)
        args <- gsub('^"|"$', "", args)
        args <- gsub("^'|'$", "", args)
        file.path(args)
    })
    extracted_paths <- unlist(extracted_paths, use.names = FALSE)

    unique(extracted_paths[!is.na(extracted_paths)])
}

categorize_sourced_file <- function(path) {
    dplyr::case_when(
        grepl("^scripts/utils/", path) ~ "Utils",
        grepl("^scripts/tools/", path) ~ "Tools",
        grepl("^scripts/data_helper/", path) ~ "Data Helper",
        grepl("^scripts/tables/", path) ~ "Tables",
        grepl("^scripts/analysis/", path) ~ "Analysis",
        grepl("^scripts/subgroup/", path) ~ "Subgroup",
        grepl("^scripts/visualization/", path) ~ "Visualization",
        grepl("^scripts/gep/", path) ~ "GEP",
        grepl("^scripts/workflow/", path) ~ "Workflow",
        TRUE ~ "Other"
    )
}

render_loader_inventory <- function(sourced_files) {
    inventory <- tibble::tibble(
        path = sourced_files,
        subsystem = vapply(sourced_files, categorize_sourced_file, character(1))
    ) %>%
        dplyr::arrange(.data$subsystem, .data$path)

    rendered_groups <- inventory %>%
        dplyr::group_by(.data$subsystem) %>%
        dplyr::summarise(
            lines = list(sprintf("- `%s`", .data$path)),
            .groups = "drop"
        )

    unlist(purrr::map2(rendered_groups$subsystem, rendered_groups$lines, function(subsystem, lines) {
        c(sprintf("### %s", subsystem), "", lines, "")
    }), use.names = FALSE)
}

render_dependency_diagram_markdown <- function(load_all_path = here::here("scripts", "load_all.R")) {
    sourced_files <- extract_load_all_sources(load_all_path = load_all_path)

    c(
        "# High-Level Dependency Diagram",
        "",
        "This file is generated from the current loader plus curated workflow/output rules. The Mermaid diagram stays high-level; the appendix captures the full sourced inventory so the doc does not drift from `scripts/load_all.R`.",
        "",
        "```mermaid",
        "flowchart LR",
        "    subgraph CFG[Config]",
        "        CONFIG[utils/config_constants.R]",
        "    end",
        "",
        "    subgraph LDR[Loader]",
        "        LOADALL[scripts/load_all.R]",
        "    end",
        "",
        "    subgraph UTL[Shared Helpers]",
        "        UHELP[utils + tool helpers]",
        "        TDOC[tools/study_doc_generators.R]",
        "    end",
        "",
        "    subgraph DATA[Data Pipeline]",
        "        DLOAD[data_helper/data_loading.R]",
        "        DDER[data_helper/data_derivation.R]",
        "        DCOH[data_helper/cohort_creation.R]",
        "        DORCH[data_helper/cohort_orchestration.R]",
        "        DGEP[data_helper/gep_missing_data_analysis.R]",
        "    end",
        "",
        "    subgraph TBL[Tables and Diagnostics]",
        "        TBLS[tables/*.R]",
        "    end",
        "",
        "    subgraph ANA[Analysis Modules]",
        "        ANAALL[analysis/*.R]",
        "        SUBALL[subgroup/*.R]",
        "        VISALL[visualization/*.R]",
        "    end",
        "",
        "    subgraph GEP[GEP Validation]",
        "        GEPALL[gep/**/*.R]",
        "    end",
        "",
        "    subgraph WFL[Workflow]",
        "        AORCH[workflow/analysis_orchestration.R]",
        "        OBJ0[workflow/objective_0_data_processing.R]",
        "        OBJ1[workflow/objective_1_primary_outcomes.R]",
        "        OBJ2[workflow/objective_2_safety_toxicity.R]",
        "        OBJ3[workflow/objective_3_repeat_radiation.R]",
        "        OBJ4[workflow/objective_4_gep_analysis.R]",
        "        PUB[workflow/publish_outputs.R]",
        "    end",
        "",
        "    RAW[(Export-backed raw inputs)]",
        "    PDATA[(Runtime analytic datasets)]",
        "    OUT[(Runtime cohort outputs)]",
        "    MERGED[(Runtime merged tables)]",
        "    LOGS[(Runtime logs)]",
        "    SNAP[(Synced publish snapshots)]",
        "",
        "    LOADALL --> CONFIG",
        "    LOADALL --> UHELP",
        "    LOADALL --> TDOC",
        "    LOADALL --> DATA",
        "    LOADALL --> TBLS",
        "    LOADALL --> ANAALL",
        "    LOADALL --> SUBALL",
        "    LOADALL --> VISALL",
        "    LOADALL --> GEPALL",
        "    LOADALL --> AORCH",
        "",
        "    OBJ0 --> DORCH",
        "    DLOAD --> RAW",
        "    DORCH --> DLOAD",
        "    DORCH --> DDER",
        "    DORCH --> DCOH",
        "    DORCH --> DGEP",
        "    DORCH --> PDATA",
        "    DORCH --> OUT",
        "",
        "    AORCH --> OBJ0",
        "    AORCH --> OBJ1",
        "    AORCH --> OBJ2",
        "    AORCH --> OBJ3",
        "    AORCH --> OBJ4",
        "    AORCH --> MERGED",
        "    AORCH --> LOGS",
        "",
        "    OBJ1 --> ANAALL",
        "    OBJ1 --> TBLS",
        "    OBJ1 --> OUT",
        "    OBJ2 --> ANAALL",
        "    OBJ2 --> TBLS",
        "    OBJ2 --> OUT",
        "    OBJ3 --> ANAALL",
        "    OBJ3 --> TBLS",
        "    OBJ3 --> OUT",
        "    OBJ4 --> GEPALL",
        "    OBJ4 --> TBLS",
        "    OBJ4 --> OUT",
        "",
        "    PUB --> OUT",
        "    PUB --> MERGED",
        "    PUB --> SNAP",
        "```",
        "",
        "## Notes",
        "",
        "- The main loader inventory is taken from `scripts/load_all.R` rather than maintained by hand.",
        "- Objective 0 now owns the runtime audit trail that feeds cohort `00_General` artifacts.",
        "- Published synced snapshots are created by `workflow/publish_outputs.R`, so the output model now includes both runtime and synced layers.",
        "",
        "## Loader Inventory",
        "",
        render_loader_inventory(sourced_files)
    )
}

generate_dependency_diagram_doc <- function(output_file = here::here("docs", "dependency_diagram.md")) {
    markdown_lines <- render_dependency_diagram_markdown()
    writeLines(markdown_lines, output_file)

    list(
        status = "success",
        output_file = output_file
    )
}

render_figure_counts_table <- function(cohort_summary, cohort_label) {
    treatment_rows <- tibble::tribble(
        ~Treatment, ~N, ~Local_Recurrence, ~Metastasis, ~Alive, ~Lost_to_Followup, ~Dead,
        "PBT",
        cohort_summary$treatments$PBT$n,
        cohort_summary$treatments$PBT$local_recurrence,
        cohort_summary$treatments$PBT$metastasis,
        cohort_summary$treatments$PBT$alive,
        cohort_summary$treatments$PBT$lost_to_followup,
        cohort_summary$treatments$PBT$dead,
        "GKSRS",
        cohort_summary$treatments$GKSRS$n,
        cohort_summary$treatments$GKSRS$local_recurrence,
        cohort_summary$treatments$GKSRS$metastasis,
        cohort_summary$treatments$GKSRS$alive,
        cohort_summary$treatments$GKSRS$lost_to_followup,
        cohort_summary$treatments$GKSRS$dead
    )

    c(
        sprintf("### %s", cohort_label),
        "",
        sprintf("Overall cohort N = **%d**", cohort_summary$total),
        "",
        "| Treatment | N | Local recurrence | Metastasis | Alive | Lost to follow-up | Dead |",
        "|---|---:|---:|---:|---:|---:|---:|",
        apply(treatment_rows, 1, function(row) {
            sprintf(
                "| %s | %s | %s | %s | %s | %s | %s |",
                row[["Treatment"]],
                row[["N"]],
                row[["Local_Recurrence"]],
                row[["Metastasis"]],
                row[["Alive"]],
                row[["Lost_to_Followup"]],
                row[["Dead"]]
            )
        }),
        ""
    )
}

read_removed_patients_summary_for_doc <- function() {
    summary_path <- file.path(OUTPUT_DIR, "uveal_full", "00_General", "removed_patients_summary.tsv")
    if (!file.exists(summary_path)) {
        return(tibble::tibble())
    }

    tryCatch(
        readr::read_tsv(summary_path, show_col_types = FALSE),
        error = function(e) tibble::tibble()
    )
}

abbreviate_user_home_path <- function(path) {
    home <- path.expand("~")
    home_prefix <- paste0(home, .Platform$file.sep)

    if (identical(path, home)) {
        return("~")
    }
    if (startsWith(path, home_prefix)) {
        return(paste0("~", substring(path, nchar(home) + 1L)))
    }
    path
}

render_figure_counts_audit_markdown <- function(summary_data = NULL) {
    if (is.null(summary_data)) {
        summary_path <- file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json")
        summary_data <- jsonlite::read_json(summary_path, simplifyVector = TRUE)
    }

    removed_patients <- read_removed_patients_summary_for_doc()
    removed_lines <- if (nrow(removed_patients) > 0) {
        apply(removed_patients, 1, function(row) {
            sprintf(
                "| %s | %s | %s | %s | %s |",
                row[["id"]] %||% "",
                row[["removal_reason"]] %||% "",
                row[["removal_step"]] %||% "",
                row[["consort_group"]] %||% "",
                row[["treatment_group"]] %||% ""
            )
        })
    } else {
        "| No recorded removals available |  |  |  |  |"
    }

    c(
        "# Figure Counts Audit",
        "",
        "This file is generated from canonical runtime artifacts and centralized config constants. It is a current-state cohort and figure-count audit; it does **not** validate the rendered flowchart image itself.",
        "",
        "## Canonical sources",
        "",
        sprintf("- `%s`", abbreviate_user_home_path(file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"))),
        sprintf("- `%s`", abbreviate_user_home_path(file.path(OUTPUT_DIR, "uveal_full", "00_General", "removed_patients_summary.tsv"))),
        "- `scripts/utils/cohort_summary_export.R`",
        "- `scripts/utils/config_constants.R`",
        "",
        "## Current exclusion summary",
        "",
        sprintf("- Total exclusions: **%d**", summary_data$exclusions$total_excluded),
        sprintf("- Stage IV exclusions: **%d**", summary_data$exclusions$by_step$stage_iv_exclusion %||% 0),
        sprintf("- Manual exclusions: **%d**", summary_data$exclusions$by_step$manual_exclusion %||% 0),
        "",
        "### Removed patients",
        "",
        "| ID | Reason | Step | Consort group | Treatment |",
        "|---:|---|---|---|---|",
        removed_lines,
        "",
        "## Current cohort counts used for figure-facing summaries",
        "",
        render_figure_counts_table(summary_data$cohorts$full_cohort, "Full Cohort"),
        render_figure_counts_table(summary_data$cohorts$restricted_cohort, "Restricted Cohort"),
        "## Vital-status rule definition",
        "",
        sprintf("- Data cutoff date: `%s`", format(VITAL_STATUS_DATA_CUTOFF_DATE, "%Y-%m-%d")),
        sprintf("- Lost-to-follow-up threshold: `%d` days", LOST_TO_FOLLOWUP_CUTOFF_DAYS),
        "- `dead` if `death_event == 1`",
        "- otherwise `alive` if `days_since_last_contact <= LOST_TO_FOLLOWUP_CUTOFF_DAYS`",
        "- otherwise `lost_to_followup`",
        "",
        "## Interpretation",
        "",
        "- The counts above are the current runtime source of truth for figure-facing cohort summaries.",
        "- If the rendered figure disagrees with this file, the figure should be updated; this file is intentionally artifact-first rather than image-first.",
        "- GKSRS-only cohort counts are still exported elsewhere, but they are not the primary target of the current figure-count audit."
    )
}

generate_figure_counts_audit_doc <- function(output_file = here::here("docs", "FIGURE_COUNTS_AUDIT.md")) {
    markdown_lines <- render_figure_counts_audit_markdown()
    writeLines(markdown_lines, output_file)

    list(
        status = "success",
        output_file = output_file
    )
}

refresh_study_docs <- function() {
    dependency_result <- generate_dependency_diagram_doc()
    figure_result <- generate_figure_counts_audit_doc()

    list(
        status = "success",
        dependency_diagram = dependency_result,
        figure_counts_audit = figure_result
    )
}
