#!/usr/bin/env Rscript

COMPARATOR_VERSION <- 1L
SUPPORTED_TYPES <- c("json", "text", "cohort", "plot_metadata", "workbook")

parse_cli <- function(args) {
    required <- c("base-runtime", "candidate-runtime", "contract", "report")
    if (length(args) != 8L || length(args) %% 2L != 0L) {
        stop_cli_usage()
    }

    values <- list()
    for (index in seq(1L, length(args), by = 2L)) {
        option <- sub("^--", "", args[[index]])
        if (!startsWith(args[[index]], "--") || !option %in% required) {
            stop_cli_usage()
        }
        if (!nzchar(args[[index + 1L]])) {
            stop_cli_usage()
        }
        if (!is.null(values[[option]])) {
            stop_cli_usage()
        }
        values[[option]] <- args[[index + 1L]]
    }

    if (!setequal(names(values), required)) {
        stop_cli_usage()
    }
    values
}

stop_cli_usage <- function() {
    stop(
        paste(
            "Usage: Rscript scripts/maintenance/compare_important_results.R",
            "--base-runtime BASE_RUNTIME --candidate-runtime CANDIDATE_RUNTIME",
            "--contract CONTRACT_FILE --report REPORT_FILE"
        ),
        call. = FALSE
    )
}

is_safe_relative_path <- function(path) {
    is.character(path) &&
        length(path) == 1L &&
        nzchar(path) &&
        !grepl("^(/|~|[A-Za-z]:)", path) &&
        !grepl("(^|/)\\.\\.(/|$)", path, perl = TRUE)
}

load_contract <- function(path) {
    if (!file.exists(path)) {
        stop("Contract file is missing", call. = FALSE)
    }

    contract <- yaml::read_yaml(path)
    if (!is.list(contract) || !identical(as.integer(contract$version), COMPARATOR_VERSION)) {
        stop("Contract version is unsupported", call. = FALSE)
    }

    tolerance <- contract$numeric_tolerance
    if (!is.list(tolerance) ||
        !is.numeric(tolerance$absolute) ||
        length(tolerance$absolute) != 1L ||
        !is.numeric(tolerance$relative) ||
        length(tolerance$relative) != 1L ||
        tolerance$absolute < 0 ||
        tolerance$relative < 0) {
        stop("Contract numeric tolerance is invalid", call. = FALSE)
    }

    comparisons <- contract$comparisons
    if (!is.list(comparisons) || !length(comparisons)) {
        stop("Contract comparisons are missing", call. = FALSE)
    }

    ids <- character(length(comparisons))
    for (index in seq_along(comparisons)) {
        item <- comparisons[[index]]
        if (!is.list(item) ||
            !is.character(item$id) || length(item$id) != 1L || !nzchar(item$id) ||
            !is.character(item$type) || length(item$type) != 1L ||
            !item$type %in% SUPPORTED_TYPES ||
            !is_safe_relative_path(item$path)) {
            stop("Contract comparison entry is invalid", call. = FALSE)
        }
        ids[[index]] <- item$id
    }
    if (anyDuplicated(ids)) {
        stop("Contract comparison IDs must be unique", call. = FALSE)
    }
    contract
}

resolve_artifact <- function(root, relative_path) {
    if (!is_safe_relative_path(relative_path)) {
        stop("Artifact path is not safely relative", call. = FALSE)
    }
    normalized_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
    candidate <- normalizePath(
        file.path(normalized_root, relative_path),
        winslash = "/",
        mustWork = FALSE
    )
    root_prefix <- paste0(normalized_root, "/")
    if (!identical(candidate, normalized_root) && !startsWith(candidate, root_prefix)) {
        stop("Artifact path escapes its runtime root", call. = FALSE)
    }
    candidate
}

numeric_equal <- function(base, candidate, absolute, relative) {
    if (length(base) != 1L || length(candidate) != 1L) {
        return(FALSE)
    }
    if (is.na(base) || is.na(candidate)) {
        return(is.na(base) && is.na(candidate))
    }
    if (identical(base, candidate)) {
        return(TRUE)
    }
    difference <- abs(base - candidate)
    allowance <- max(absolute, relative * max(abs(base), abs(candidate)))
    is.finite(difference) && difference <= allowance
}

compare_atomic <- function(base, candidate, absolute, relative, allow_numeric_tolerance) {
    if (length(base) != length(candidate) || !identical(names(base), names(candidate))) {
        return(FALSE)
    }
    if (is.numeric(base) && is.numeric(candidate) && allow_numeric_tolerance) {
        return(all(vapply(seq_along(base), function(index) {
            numeric_equal(base[[index]], candidate[[index]], absolute, relative)
        }, logical(1))))
    }
    identical(base, candidate)
}

compare_values <- function(base, candidate, absolute, relative, allow_numeric_tolerance = TRUE) {
    if (is.null(base) || is.null(candidate)) {
        return(is.null(base) && is.null(candidate))
    }
    if (is.list(base) && is.list(candidate)) {
        base_names <- names(base)
        candidate_names <- names(candidate)
        if (is.null(base_names) || is.null(candidate_names)) {
            if (!is.null(base_names) || !is.null(candidate_names) || length(base) != length(candidate)) {
                return(FALSE)
            }
            return(all(vapply(seq_along(base), function(index) {
                compare_values(base[[index]], candidate[[index]], absolute, relative, allow_numeric_tolerance)
            }, logical(1))))
        }
        if (!setequal(base_names, candidate_names)) {
            return(FALSE)
        }
        return(all(vapply(base_names, function(name) {
            compare_values(base[[name]], candidate[[name]], absolute, relative, allow_numeric_tolerance)
        }, logical(1))))
    }
    if (is.atomic(base) && is.atomic(candidate)) {
        return(compare_atomic(base, candidate, absolute, relative, allow_numeric_tolerance))
    }
    FALSE
}

read_json_artifact <- function(path) {
    jsonlite::read_json(path, simplifyVector = FALSE)
}

read_text_artifact <- function(path) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
}

archive_xml <- function(path, member) {
    extraction_dir <- tempfile("important-results-xml-")
    dir.create(extraction_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(extraction_dir, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(path, files = member, exdir = extraction_dir)
    xml2::read_xml(file.path(extraction_dir, member))
}

archive_members <- function(path) {
    utils::unzip(path, list = TRUE)$Name
}

attribute_by_local_name <- function(node, name) {
    attributes <- xml2::xml_attrs(node)
    if (!length(attributes)) {
        return(NULL)
    }
    matching_names <- names(attributes)[vapply(names(attributes), function(attribute) {
        sub("^.*:", "", attribute) == name
    }, logical(1))]
    if (!length(matching_names)) {
        return(NULL)
    }
    unname(attributes[[matching_names[[1L]]]])
}

first_node_text <- function(node, xpath) {
    nodes <- xml2::xml_find_all(node, xpath)
    if (!length(nodes)) {
        return(NULL)
    }
    xml2::xml_text(nodes[[1L]], trim = FALSE)
}

resolve_archive_target <- function(target) {
    target <- sub("^/", "", target)
    if (startsWith(target, "xl/")) {
        return(target)
    }
    parts <- strsplit(file.path("xl", target), "/", fixed = TRUE)[[1L]]
    resolved <- character()
    for (part in parts) {
        if (identical(part, "") || identical(part, ".")) {
            next
        }
        if (identical(part, "..")) {
            if (length(resolved)) {
                resolved <- resolved[-length(resolved)]
            }
            next
        }
        resolved <- c(resolved, part)
    }
    paste(resolved, collapse = "/")
}

shared_strings_for <- function(path) {
    if (!"xl/sharedStrings.xml" %in% archive_members(path)) {
        return(character())
    }
    shared_strings <- archive_xml(path, "xl/sharedStrings.xml")
    string_nodes <- xml2::xml_find_all(shared_strings, ".//*[local-name()='si']")
    vapply(string_nodes, function(node) {
        paste(xml2::xml_text(xml2::xml_find_all(node, ".//*[local-name()='t']"), trim = FALSE), collapse = "")
    }, character(1))
}

workbook_manifest <- function(path) {
    members <- archive_members(path)
    required_members <- c("xl/workbook.xml", "xl/_rels/workbook.xml.rels")
    if (!all(required_members %in% members)) {
        stop("Workbook archive is incomplete", call. = FALSE)
    }

    workbook <- archive_xml(path, "xl/workbook.xml")
    relationships <- archive_xml(path, "xl/_rels/workbook.xml.rels")
    relationship_nodes <- xml2::xml_find_all(relationships, ".//*[local-name()='Relationship']")
    relationship_ids <- vapply(relationship_nodes, attribute_by_local_name, character(1), name = "Id")
    relationship_targets <- vapply(relationship_nodes, attribute_by_local_name, character(1), name = "Target")
    relationship_map <- setNames(
        vapply(relationship_targets, resolve_archive_target, character(1)),
        relationship_ids
    )

    sheet_nodes <- xml2::xml_find_all(workbook, ".//*[local-name()='sheets']/*[local-name()='sheet']")
    if (!length(sheet_nodes)) {
        stop("Workbook contains no worksheets", call. = FALSE)
    }
    shared_strings <- shared_strings_for(path)

    sheets <- lapply(sheet_nodes, function(sheet_node) {
        relation_id <- attribute_by_local_name(sheet_node, "id")
        worksheet_member <- unname(relationship_map[[relation_id]])
        if (is.null(worksheet_member) || !nzchar(worksheet_member)) {
            stop("Workbook worksheet relationship is missing", call. = FALSE)
        }
        worksheet <- archive_xml(path, worksheet_member)
        dimension_node <- xml2::xml_find_all(worksheet, ".//*[local-name()='dimension']")
        dimension <- if (length(dimension_node)) {
            attribute_by_local_name(dimension_node[[1L]], "ref")
        } else {
            ""
        }
        cell_nodes <- xml2::xml_find_all(worksheet, ".//*[local-name()='sheetData']//*[local-name()='c']")
        cells <- lapply(cell_nodes, function(cell_node) {
            cell_type <- attribute_by_local_name(cell_node, "t")
            raw_value <- first_node_text(cell_node, "./*[local-name()='v']")
            formula <- first_node_text(cell_node, "./*[local-name()='f']")
            cell_ref <- attribute_by_local_name(cell_node, "r")

            if (identical(cell_type, "s") && !is.null(raw_value) && nzchar(raw_value)) {
                index <- suppressWarnings(as.integer(raw_value)) + 1L
                if (!is.na(index) && index >= 1L && index <= length(shared_strings)) {
                    raw_value <- shared_strings[[index]]
                    cell_type <- "string"
                }
            }
            if (identical(cell_type, "inlineStr")) {
                raw_value <- paste(
                    xml2::xml_text(xml2::xml_find_all(cell_node, ".//*[local-name()='is']//*[local-name()='t']"), trim = FALSE),
                    collapse = ""
                )
                cell_type <- "string"
            }

            numeric_cell <- is.null(cell_type) || identical(cell_type, "n")
            value <- if (is.null(raw_value) || !nzchar(raw_value)) {
                NULL
            } else if (numeric_cell) {
                suppressWarnings(as.numeric(raw_value))
            } else {
                raw_value
            }
            list(
                ref = cell_ref,
                formula = formula,
                kind = if (numeric_cell) "numeric" else "string",
                value = value
            )
        })
        cell_refs <- vapply(cells, `[[`, character(1), "ref")
        if (anyDuplicated(cell_refs)) {
            stop("Workbook contains duplicate cell references", call. = FALSE)
        }
        list(
            name = attribute_by_local_name(sheet_node, "name"),
            dimension = dimension,
            cells = cells[order(cell_refs)]
        )
    })
    sheets
}

compare_workbook_cells <- function(base_cells, candidate_cells, absolute, relative) {
    base_refs <- vapply(base_cells, `[[`, character(1), "ref")
    candidate_refs <- vapply(candidate_cells, `[[`, character(1), "ref")
    if (!identical(base_refs, candidate_refs)) {
        return(FALSE)
    }
    all(vapply(seq_along(base_cells), function(index) {
        base_cell <- base_cells[[index]]
        candidate_cell <- candidate_cells[[index]]
        if (!identical(base_cell$formula, candidate_cell$formula) ||
            !identical(base_cell$kind, candidate_cell$kind)) {
            return(FALSE)
        }
        if (identical(base_cell$kind, "numeric")) {
            if (is.null(base_cell$value) || is.null(candidate_cell$value)) {
                return(is.null(base_cell$value) && is.null(candidate_cell$value))
            }
            return(numeric_equal(base_cell$value, candidate_cell$value, absolute, relative))
        }
        identical(base_cell$value, candidate_cell$value)
    }, logical(1)))
}

compare_workbooks <- function(base_path, candidate_path, absolute, relative) {
    base_sheets <- workbook_manifest(base_path)
    candidate_sheets <- workbook_manifest(candidate_path)
    base_names <- vapply(base_sheets, `[[`, character(1), "name")
    candidate_names <- vapply(candidate_sheets, `[[`, character(1), "name")
    if (!identical(base_names, candidate_names)) {
        return(FALSE)
    }
    all(vapply(seq_along(base_sheets), function(index) {
        base_sheet <- base_sheets[[index]]
        candidate_sheet <- candidate_sheets[[index]]
        identical(base_sheet$dimension, candidate_sheet$dimension) &&
            compare_workbook_cells(base_sheet$cells, candidate_sheet$cells, absolute, relative)
    }, logical(1)))
}

compare_one <- function(item, base_root, candidate_root, absolute, relative) {
    base_path <- resolve_artifact(base_root, item$path)
    candidate_path <- resolve_artifact(candidate_root, item$path)
    result <- list(
        id = item$id,
        type = item$type,
        status = "fail",
        reason = "required artifact missing"
    )
    if (!file.exists(base_path) || !file.exists(candidate_path)) {
        return(result)
    }

    matches <- tryCatch({
        switch(
            item$type,
            json = compare_values(
                read_json_artifact(base_path),
                read_json_artifact(candidate_path),
                absolute,
                relative
            ),
            cohort = compare_values(
                read_json_artifact(base_path),
                read_json_artifact(candidate_path),
                absolute,
                relative,
                allow_numeric_tolerance = FALSE
            ),
            plot_metadata = compare_values(
                read_json_artifact(base_path),
                read_json_artifact(candidate_path),
                absolute,
                relative
            ),
            text = identical(read_text_artifact(base_path), read_text_artifact(candidate_path)),
            workbook = compare_workbooks(base_path, candidate_path, absolute, relative),
            FALSE
        )
    }, error = function(error) FALSE)

    if (isTRUE(matches)) {
        result$status <- "pass"
        result$reason <- "artifacts match"
    } else if (identical(item$type, "cohort")) {
        result$reason <- "ordered cohort membership differs"
    } else if (identical(item$type, "text")) {
        result$reason <- "displayed text differs"
    } else if (identical(item$type, "workbook")) {
        result$reason <- "workbook sheets, dimensions, formulas, or cells differ"
    } else {
        result$reason <- "artifact values or metadata differ"
    }
    result
}

write_report <- function(path, status, comparisons) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
        list(
            comparator_version = COMPARATOR_VERSION,
            status = status,
            comparisons = comparisons
        ),
        path,
        auto_unbox = TRUE,
        pretty = TRUE
    )
}

run_comparison <- function(options) {
    contract <- load_contract(options$contract)
    comparisons <- lapply(contract$comparisons, compare_one, options$`base-runtime`, options$`candidate-runtime`, contract$numeric_tolerance$absolute, contract$numeric_tolerance$relative)
    passed <- all(vapply(comparisons, function(item) identical(item$status, "pass"), logical(1)))
    write_report(options$report, if (passed) "pass" else "fail", comparisons)
    if (passed) 0L else 1L
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
    options <- parse_cli(args)
    tryCatch(
        run_comparison(options),
        error = function(error) {
            write_report(
                options$report,
                "fail",
                list(list(id = "contract", type = "contract", status = "fail", reason = "invalid contract"))
            )
            1L
        }
    )
}

if (sys.nframe() == 0L) {
    required_packages <- c("jsonlite", "xml2", "yaml")
    missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages)) {
        stop("Required comparator packages are unavailable", call. = FALSE)
    }
    quit(save = "no", status = main())
}
