# Shared runtime helpers for scripts/tools

tool_run_timestamp <- function() {
    format(Sys.time(), "%Y%m%d_%H%M%S")
}

ensure_tool_output_dir <- function(output_dir = TOOLS_OUTPUT_DIR) {
    if (is.null(output_dir) || length(output_dir) == 0 || is.na(output_dir) || !nzchar(output_dir)) {
        stop("output_dir must be a non-empty path")
    }

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_dir
}

tool_output_path <- function(
    tool_name,
    extension,
    output_dir = TOOLS_OUTPUT_DIR,
    include_timestamp = FALSE,
    suffix = NULL
) {
    if (is.null(tool_name) || length(tool_name) == 0 || !nzchar(tool_name)) {
        stop("tool_name must be a non-empty string")
    }

    if (is.null(extension) || length(extension) == 0 || !nzchar(extension)) {
        stop("extension must be a non-empty string")
    }

    output_dir <- ensure_tool_output_dir(output_dir)
    file_stem <- tool_name

    if (!is.null(suffix) && nzchar(suffix)) {
        file_stem <- paste0(file_stem, "_", suffix)
    }

    if (isTRUE(include_timestamp)) {
        file_stem <- paste0(file_stem, "_", tool_run_timestamp())
    }

    file.path(output_dir, paste0(file_stem, ".", sub("^\\.", "", extension)))
}

load_tool_dataset <- function(dataset_name = "uveal_melanoma_full_cohort", data_dir = PROCESSED_DATA_DIR) {
    if (is.null(dataset_name) || length(dataset_name) == 0 || !nzchar(dataset_name)) {
        stop("dataset_name must be a non-empty string")
    }

    dataset_path <- file.path(data_dir, paste0(dataset_name, ".rds"))
    if (!file.exists(dataset_path)) {
        stop(sprintf("Analytic dataset not found: %s", dataset_path))
    }

    readRDS(dataset_path)
}

normalize_tool_outputs <- function(outputs) {
    if (is.null(outputs)) {
        return(data.frame(
            output_name = character(),
            output_path = character(),
            stringsAsFactors = FALSE
        ))
    }

    if (is.character(outputs)) {
        output_paths <- unname(outputs)
        output_names <- names(outputs)
        if (is.null(output_names)) {
            output_names <- paste0("output_", seq_along(output_paths))
        }

        return(data.frame(
            output_name = output_names,
            output_path = output_paths,
            stringsAsFactors = FALSE
        ))
    }

    if (!is.list(outputs)) {
        stop("outputs must be a character vector or a named list of file paths")
    }

    output_names <- names(outputs)
    if (is.null(output_names)) {
        output_names <- paste0("output_", seq_along(outputs))
    }

    output_paths <- vapply(outputs, function(path_value) {
        if (length(path_value) == 0) {
            return(NA_character_)
        }
        as.character(path_value[[1]])
    }, character(1))

    data.frame(
        output_name = output_names,
        output_path = output_paths,
        stringsAsFactors = FALSE
    )
}

build_tool_run_manifest <- function(
    tool_name,
    outputs,
    dataset_name = NA_character_,
    status = "success",
    notes = NA_character_,
    run_id = tool_run_timestamp()
) {
    output_df <- normalize_tool_outputs(outputs)

    if (nrow(output_df) == 0) {
        output_df <- data.frame(
            output_name = "output",
            output_path = NA_character_,
            stringsAsFactors = FALSE
        )
    }

    data.frame(
        run_id = run_id,
        tool_name = tool_name,
        dataset_name = dataset_name,
        output_name = output_df$output_name,
        output_path = output_df$output_path,
        status = status,
        notes = notes,
        stringsAsFactors = FALSE
    )
}

write_tool_run_summary <- function(
    tool_name,
    outputs,
    dataset_name = NA_character_,
    status = "success",
    notes = NULL,
    output_dir = TOOLS_OUTPUT_DIR,
    run_id = tool_run_timestamp()
) {
    output_dir <- ensure_tool_output_dir(output_dir)
    manifest <- build_tool_run_manifest(
        tool_name = tool_name,
        outputs = outputs,
        dataset_name = dataset_name,
        status = status,
        notes = if (is.null(notes)) NA_character_ else as.character(notes),
        run_id = run_id
    )

    csv_path <- file.path(output_dir, paste0(tool_name, "_run_", run_id, "_summary.csv"))
    txt_path <- file.path(output_dir, paste0(tool_name, "_run_", run_id, "_summary.txt"))

    write.csv(manifest, csv_path, row.names = FALSE)

    summary_lines <- c(
        sprintf("tool_name: %s", tool_name),
        sprintf("dataset_name: %s", ifelse(is.na(dataset_name), "NA", dataset_name)),
        sprintf("run_id: %s", run_id),
        sprintf("status: %s", status),
        sprintf("notes: %s", ifelse(is.na(notes) || length(notes) == 0, "NA", notes)),
        "outputs:"
    )

    output_lines <- apply(manifest, 1, function(row) {
        sprintf("- %s: %s", row[["output_name"]], row[["output_path"]])
    })

    writeLines(c(summary_lines, output_lines), txt_path)

    invisible(list(
        manifest = manifest,
        csv_path = csv_path,
        txt_path = txt_path,
        run_id = run_id
    ))
}