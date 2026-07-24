# Periodic refresh runner for documentation-oriented tools

if (!exists("TOOLS_OUTPUT_DIR", inherits = TRUE)) {
    source(here::here("scripts", "load_all.R"))
}

source(here::here("scripts", "tools", "derived_variables_documentation.R"))
source(here::here("scripts", "tools", "comprehensive_variable_census.R"))
source(here::here("scripts", "tools", "study_doc_generators.R"))

run_tool_refresh_suite <- function(
    dataset_name = "uveal_melanoma_full_cohort",
    output_dir = TOOLS_OUTPUT_DIR,
    include_timestamp = FALSE
) {
    output_dir <- ensure_tool_output_dir(output_dir)
    suite_run_id <- tool_run_timestamp()

    tool_rows <- list()

    run_one_tool <- function(tool_name, expression) {
        started_at <- Sys.time()
        result <- tryCatch(
            expression,
            error = function(e) {
                list(
                    status = "error",
                    error_message = conditionMessage(e)
                )
            }
        )
        completed_at <- Sys.time()

        if (is.null(result$status)) {
            result$status <- "success"
        }

        tool_rows[[length(tool_rows) + 1L]] <<- data.frame(
            suite_run_id = suite_run_id,
            tool_name = tool_name,
            status = result$status,
            started_at = format(started_at, "%Y-%m-%d %H:%M:%S"),
            completed_at = format(completed_at, "%Y-%m-%d %H:%M:%S"),
            output_file = if (!is.null(result$output_file)) result$output_file else NA_character_,
            validation_file = if (!is.null(result$validation_file)) result$validation_file else NA_character_,
            error_message = if (!is.null(result$error_message)) result$error_message else NA_character_,
            stringsAsFactors = FALSE
        )

        result
    }

    derived_result <- run_one_tool(
        "derived_variables_documentation",
        generate_derived_variables_documentation(
            dataset_name = dataset_name,
            output_dir = output_dir,
            include_timestamp = include_timestamp
        )
    )

    census_result <- run_one_tool(
        "comprehensive_variable_census",
        create_comprehensive_variable_census(
            dataset_name = dataset_name,
            output_dir = output_dir
        )
    )

    dependency_doc_result <- run_one_tool(
        "dependency_diagram_doc",
        generate_dependency_diagram_doc()
    )

    figure_counts_result <- run_one_tool(
        "figure_counts_audit_doc",
        generate_figure_counts_audit_doc()
    )

    suite_manifest <- do.call(rbind, tool_rows)
    suite_csv <- file.path(output_dir, paste0("tool_refresh_suite_run_", suite_run_id, ".csv"))
    suite_txt <- file.path(output_dir, paste0("tool_refresh_suite_run_", suite_run_id, ".txt"))

    write.csv(suite_manifest, suite_csv, row.names = FALSE)

    writeLines(
        c(
            sprintf("suite_run_id: %s", suite_run_id),
            sprintf("dataset_name: %s", dataset_name),
            sprintf("output_dir: %s", output_dir),
            sprintf("derived_variables_documentation_status: %s", if (!is.null(derived_result$status)) derived_result$status else "unknown"),
            sprintf("comprehensive_variable_census_status: %s", if (!is.null(census_result$status)) census_result$status else "unknown"),
            sprintf("dependency_diagram_doc_status: %s", if (!is.null(dependency_doc_result$status)) dependency_doc_result$status else "unknown"),
            sprintf("figure_counts_audit_doc_status: %s", if (!is.null(figure_counts_result$status)) figure_counts_result$status else "unknown"),
            "tool outputs:"
        ),
        suite_txt
    )

    output_lines <- apply(suite_manifest, 1, function(row) {
        output_bits <- character(0)
        if (!is.na(row[["output_file"]]) && nzchar(row[["output_file"]])) {
            output_bits <- c(output_bits, paste0("output_file=", row[["output_file"]]))
        }
        if (!is.na(row[["validation_file"]]) && nzchar(row[["validation_file"]])) {
            output_bits <- c(output_bits, paste0("validation_file=", row[["validation_file"]]))
        }
        if (!is.na(row[["error_message"]]) && nzchar(row[["error_message"]])) {
            output_bits <- c(output_bits, paste0("error=", row[["error_message"]]))
        }
        sprintf("- %s: %s", row[["tool_name"]], paste(output_bits, collapse = "; "))
    })

    write(output_lines, suite_txt, append = TRUE)

    invisible(list(
        suite_run_id = suite_run_id,
        manifest = suite_manifest,
        csv_path = suite_csv,
        txt_path = suite_txt,
        derived_result = derived_result,
        census_result = census_result,
        dependency_doc_result = dependency_doc_result,
        figure_counts_result = figure_counts_result
    ))
}

main <- function() {
    cat("=== TOOL REFRESH SUITE ===\n")
    result <- run_tool_refresh_suite()
    cat(sprintf("Suite manifest written to: %s\n", result$csv_path))
    cat(sprintf("Suite summary written to: %s\n", result$txt_path))
    cat("=== TOOL REFRESH COMPLETE ===\n")
    invisible(result)
}

if (sys.nframe() == 0L) {
    main()
}
