#' Factor-Level Audit Utilities
#'
#' Shared factor-level expectations and source-audit helpers for the analysis
#' pipeline. The goal is to make factor ordering explicit and to fail fast when
#' new implicit releveling patterns appear in model-facing code.

#' Return the canonical factor-level expectations used by validation
#'
#' This list is the shared source of truth for factor level checks in the
#' validation layer. Only include factors with fixed, repository-wide level
#' orderings here.
#'
#' @return Named list of factor configurations.
get_canonical_factor_level_expectations <- function() {
    list(
        treatment_group = list(
            levels = TREATMENT_FACTOR_LEVELS,
            reference = TREATMENT_REFERENCE_LEVEL,
            comparison = TREATMENT_COMPARISON_LEVEL,
            critical = TRUE
        ),
        recurrence1 = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = TRUE
        ),
        mets_progression = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        sex = list(
            levels = SEX_FACTOR_LEVELS,
            reference = SEX_FACTOR_LEVELS[1],
            critical = FALSE
        ),
        location = list(
            levels = c("Choroidal", "Ciliary Body", "Cilio-Choroidal", "Conjunctival", "Irido-Ciliary", "Iris"),
            reference = "Choroidal",
            critical = FALSE
        ),
        optic_nerve = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = TRUE
        ),
        internal_reflectivity = list(
            levels = c("Very Low", "Low", "Low-Medium", "Medium", "Medium-High", "High", "Unknown"),
            reference = "Very Low",
            critical = FALSE
        ),
        srf = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        op = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        symptoms = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        vision_loss_blurred_vision = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        visual_field_defect = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        flashes_photopsia = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        floaters = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        pain = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],
            critical = FALSE
        ),
        initial_overall_stage = list(
            levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"),
            reference = "1",
            critical = FALSE
        ),
        initial_stage_binary = list(
            levels = c("Stage I-III", "Stage IV"),
            reference = "Stage I-III",
            critical = FALSE
        ),
        biopsy1_gep_raw = list(
            levels = c(
                "Class_1A_PRAME_negative", "Class_1A_PRAME_positive", "Class_1A_PRAME_not_reported",
                "Class_1B_PRAME_negative", "Class_1B_PRAME_positive",
                "Class_2_PRAME_negative", "Class_2_PRAME_positive", "Class_2_PRAME_Unknown", "Class_2_PRAME_not_reported",
                "Failed", "Unknown", "Class_1A_PRAME_discordant", "No"
            ),
            reference = "Class_1A_PRAME_negative",
            critical = FALSE
        ),
        biopsy1_gep = list(
            levels = c(
                "Class 1 PRAME Negative",
                "Class 1 PRAME Positive",
                "Class 2 PRAME Negative",
                "Class 2 PRAME Positive",
                "GEP Failed/Indeterminate",
                "GEP Not Tested"
            ),
            reference = "Class 1 PRAME Negative",
            critical = FALSE
        ),
        gep_class_simple = list(
            levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"),
            reference = "Class 1",
            critical = FALSE
        ),
        prame_status = list(
            levels = c("Negative", "Positive", "Unknown", "Not Available"),
            reference = "Negative",
            critical = FALSE
        ),
        gep12_prame_status = list(
            levels = c("Negative", "Positive"),
            reference = "Negative",
            critical = FALSE
        ),
        initial_t_stage_simple = list(
            levels = c("T1", "T2", "T3", "T4"),
            reference = "T1",
            critical = FALSE
        )
    )
}

#' Capture a single factor-related source block from an R file
#'
#' Reads from a line containing a factor-related pattern until the surrounding
#' call is closed or the maximum line count is reached.
#'
#' @param lines Character vector of file lines.
#' @param start_index Integer line index where the match begins.
#' @param max_lines Maximum number of lines to capture.
#'
#' @return Character scalar containing the captured source block.
capture_factor_audit_block <- function(lines, start_index, max_lines = 25L) {
    if (length(lines) == 0 || start_index < 1L || start_index > length(lines)) {
        return("")
    }

    end_index <- min(length(lines), start_index + max_lines - 1L)
    block <- character()
    balance <- 0L
    started <- FALSE
    factor_call_pattern <- "(?<![A-Za-z0-9_.])factor\\s*\\("

    for (i in seq.int(start_index, end_index)) {
        current_line <- lines[[i]]
        block <- c(block, current_line)

        code_line <- sub("#.*$", "", current_line)
        if (grepl(factor_call_pattern, code_line, perl = TRUE) ||
            grepl("as\\.factor\\s*\\(", code_line, perl = TRUE) ||
            grepl("droplevels\\s*\\(", code_line, perl = TRUE) ||
            grepl("levels\\s*\\(", code_line, perl = TRUE)) {
            started <- TRUE
        }

        balance <- balance +
            stringr::str_count(code_line, fixed("(")) -
            stringr::str_count(code_line, fixed(")"))

        if (isTRUE(started) && balance <= 0L) {
            break
        }
    }

    paste(block, collapse = "\n")
}

#' Classify a factor-related source block
#'
#' The classification is intentionally conservative:
#' - explicit level declarations are treated as safe
#' - helper-based preservation is treated as level-preserving normalization
#' - factor reconstruction without explicit levels is treated as risky
#' - droplevels in table/plot/output paths is treated as display-only
#'
#' @param code_block Character block of source code.
#' @param file_path Character path to the source file.
#'
#' @return A list with `classification` and `reason`.
classify_factor_audit_block <- function(code_block, file_path) {
    code_norm <- stringr::str_squish(sub("#.*$", "", code_block))
    file_norm <- normalizePath(file_path, winslash = "/", mustWork = FALSE)

    if (!nzchar(code_norm)) {
        return(NULL)
    }

    factor_call_pattern <- "(?<![A-Za-z0-9_.])factor\\s*\\("
    display_path <- grepl(
        "/(visualization|tables|output_utilities|data_summaries|rmst_visualization|vision_safety_analysis|forest_plot_|tools/|tests/)",
        file_norm
    )

    if (grepl("\\bas\\.factor\\(", code_norm)) {
        return(list(
            classification = "needs explicit level preservation",
            reason = "base factor coercion discards the original level order"
        ))
    }

    if (grepl("\\blevels\\s*\\(\\s*factor\\s*\\(", code_norm)) {
        return(list(
            classification = "needs explicit level preservation",
            reason = "rebuilding a factor from another factor can reset levels implicitly"
        ))
    }

    if (grepl("coerce_to_factor_preserving_levels|get_stable_factor_levels", code_norm)) {
        return(list(
            classification = "level-preserving normalization",
            reason = "uses the shared stable-level helper"
        ))
    }

    if (grepl("\\bdroplevels\\s*\\(", code_norm)) {
        return(list(
            classification = if (isTRUE(display_path)) "display-only, not model-facing" else "level-preserving normalization",
            reason = if (isTRUE(display_path)) {
                "unused levels are dropped for display or table hygiene"
            } else {
                "unused levels are dropped after the factor order has already been fixed"
            }
        ))
    }

    if (grepl(factor_call_pattern, code_norm, perl = TRUE)) {
        has_levels <- grepl("\\blevels\\s*=", code_norm)
        if (isTRUE(has_levels)) {
            return(list(
                classification = if (isTRUE(display_path)) "display-only, not model-facing" else "safe",
                reason = "explicit levels are declared in the factor call"
            ))
        }

        if (grepl("\\bfactor\\s*\\(\\s*\\)", code_norm) ||
            grepl("\\bfactor\\s*\\(\\s*NA_character_\\s*\\)", code_norm)) {
            return(list(
                classification = "display-only, not model-facing",
                reason = "placeholder factor construction used for display or missing-value scaffolding"
            ))
        }

        return(list(
            classification = "needs explicit level preservation",
            reason = "factor() is used without an explicit level declaration"
        ))
    }

    list(
        classification = "safe",
        reason = "no factor-level mutation detected"
    )
}

#' Scan source files for factor-level sites
#'
#' Searches R source files for factor reconstruction, explicit factor level
#' declarations, and level-dropping helpers. The returned table is suitable for
#' generating a lightweight audit report or CI guard.
#'
#' @param root_dir Repository root directory.
#' @param paths Character vector of subdirectories to scan relative to
#'   `root_dir`.
#'
#' @return A tibble with one row per factor-related source site.
scan_factor_level_sites <- function(root_dir = here::here(), paths = c("scripts")) {
    source_files <- unlist(lapply(paths, function(path) {
        full_path <- file.path(root_dir, path)
        if (!dir.exists(full_path)) {
            return(character())
        }
        list.files(full_path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
    }), use.names = FALSE)

    source_files <- unique(sort(source_files[file.exists(source_files)]))
    if (length(source_files) == 0) {
        return(tibble::tibble())
    }

    rows <- list()
    match_patterns <- c(
        "as\\.factor\\s*\\(",
        "droplevels\\s*\\(",
        "levels\\s*\\(\\s*factor\\s*\\(",
        "(?<![A-Za-z0-9_.])factor\\s*\\("
    )

    for (file_path in source_files) {
        lines <- readLines(file_path, warn = FALSE)
        if (length(lines) == 0) {
            next
        }

        match_index <- unique(sort(unlist(lapply(match_patterns, function(pattern) {
            grep(pattern, lines, perl = TRUE)
        }), use.names = FALSE)))

        if (length(match_index) == 0) {
            next
        }

        for (line_index in match_index) {
            line_text <- lines[[line_index]]
            if (grepl("^\\s*#", line_text)) {
                next
            }

            code_block <- capture_factor_audit_block(lines, line_index)
            site_classification <- classify_factor_audit_block(code_block, file_path)
            if (is.null(site_classification)) {
                next
            }

            rows[[length(rows) + 1L]] <- tibble::tibble(
                file = normalizePath(file_path, winslash = "/", mustWork = FALSE),
                line = line_index,
                code = stringr::str_squish(sub("#.*$", "", code_block)),
                classification = site_classification$classification,
                reason = site_classification$reason
            )
        }
    }

    if (length(rows) == 0) {
        return(tibble::tibble())
    }

    dplyr::bind_rows(rows) %>%
        dplyr::distinct(.data$file, .data$line, .data$code, .keep_all = TRUE) %>%
        dplyr::arrange(.data$file, .data$line)
}

#' Summarize factor-level audit findings
#'
#' @param audit_table Tibble returned by `scan_factor_level_sites()`.
#'
#' @return A list with counts and the subset of risky findings.
summarize_factor_level_audit <- function(audit_table) {
    if (is.null(audit_table) || nrow(audit_table) == 0) {
        return(list(
            counts = tibble::tibble(classification = character(), n = integer()),
            risky_sites = tibble::tibble()
        ))
    }

    counts <- audit_table %>%
        dplyr::count(.data$classification, name = "n") %>%
        dplyr::arrange(dplyr::desc(.data$n), .data$classification)

    risky_sites <- audit_table %>%
        dplyr::filter(.data$classification == "needs explicit level preservation")

    list(counts = counts, risky_sites = risky_sites)
}

#' Write a short markdown factor-level audit report
#'
#' @param output_file Path to the markdown file to write.
#' @param root_dir Repository root directory.
#' @param paths Character vector of subdirectories to scan.
#'
#' @return Invisibly returns the audit table used to generate the report.
write_factor_level_audit_report <- function(
    output_file = here::here("docs", "FACTOR_LEVEL_AUDIT.md"),
    root_dir = here::here(),
    paths = c("scripts")
) {
    audit_table <- scan_factor_level_sites(root_dir = root_dir, paths = paths)
    summary_info <- summarize_factor_level_audit(audit_table)
    canonical_factors <- names(get_canonical_factor_level_expectations())

    dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

    lines <- c(
        "# Factor-Level Audit",
        "",
        sprintf("- Report generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        sprintf("- Scan scope: %s", paste(paths, collapse = ", ")),
        sprintf("- Canonical fixed-level factors: %s", paste(canonical_factors, collapse = ", ")),
        "",
        "## Summary",
        sprintf("- Total factor-related source sites: %d", nrow(audit_table)),
        sprintf("- Safe or intentional sites: %d", sum(audit_table$classification != "needs explicit level preservation", na.rm = TRUE)),
        sprintf("- Risky sites requiring follow-up: %d", nrow(summary_info$risky_sites)),
        "",
        "## Interpretation",
        "- `safe` means the factor call declares levels explicitly or otherwise preserves the existing order.",
        "- `level-preserving normalization` means the shared stable-level helper is used.",
        "- `display-only, not model-facing` means the factor manipulation is for tables, plots, or placeholder display scaffolding.",
        "- `needs explicit level preservation` means a code path still reconstructs a factor without an explicit level contract.",
        ""
    )

    if (nrow(summary_info$risky_sites) > 0) {
        lines <- c(
            lines,
            "## Risky Sites",
            "",
            "| File | Line | Classification | Reason |",
            "| --- | ---: | --- | --- |"
        )

        for (i in seq_len(nrow(summary_info$risky_sites))) {
            row <- summary_info$risky_sites[i, ]
            lines <- c(
                lines,
                sprintf(
                    "| `%s` | %d | %s | %s |",
                    row$file,
                    row$line,
                    row$classification,
                    row$reason
                )
            )
        }
    } else {
        lines <- c(lines, "## Risky Sites", "", "No risky implicit releveling sites were found in the scanned paths.")
    }

    writeLines(lines, con = output_file)
    invisible(audit_table)
}
