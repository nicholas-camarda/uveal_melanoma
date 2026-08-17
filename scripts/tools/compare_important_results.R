#!/usr/bin/env Rscript

COMPARATOR_VERSION <- 1L
SUPPORTED_TYPES <- c("json", "text", "cohort", "plot_metadata", "workbook")

#' Parse the command-line options accepted by the protected-results comparator.
#'
#' @param args Character vector of command-line arguments, including four
#'   required `--name value` pairs.
#' @return A named character list containing the validated option values.
#' @noRd
parse_cli <- function(args) {
    required <- c("base-runtime", "candidate-runtime", "contract", "report")
    if (length(args) != 8L || length(args) %% 2L != 0L) {
        stop_cli_usage()
    }

    values <- list()
    # Parse pairs explicitly so duplicate or unknown options fail closed.
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

#' Abort with the comparator command-line usage message.
#'
#' @return Does not return; raises a command-line usage error.
#' @noRd
stop_cli_usage <- function() {
    stop(
        paste(
            "Usage: Rscript scripts/tools/compare_important_results.R",
            "--base-runtime BASE_RUNTIME --candidate-runtime CANDIDATE_RUNTIME",
            "--contract CONTRACT_FILE --report REPORT_FILE"
        ),
        call. = FALSE
    )
}

#' Check that an artifact path is a portable, non-escaping relative path.
#'
#' @param path Candidate artifact path.
#' @return `TRUE` when `path` is a single safe relative path; otherwise
#'   `FALSE`.
#' @noRd
is_safe_relative_path <- function(path) {
    is.character(path) &&
        length(path) == 1L &&
        nzchar(path) &&
        !grepl("^(/|~|[A-Za-z]:)", path) &&
        !grepl("(^|/)\\.\\.(/|$)", path, perl = TRUE)
}

#' Read and validate the protected important-results comparison contract.
#'
#' @param path Path to the YAML contract file.
#' @return A validated contract list containing tolerances and comparisons.
#' @noRd
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

    # Validate every entry before any runtime artifact is opened.
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

#' Resolve a contract artifact beneath a runtime root without path traversal.
#'
#' @param root Runtime directory containing the generated artifacts.
#' @param relative_path Safe path supplied by the contract.
#' @return Normalized absolute artifact path.
#' @noRd
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
    # Normalize before checking the prefix so `../` cannot escape the root.
    root_prefix <- paste0(normalized_root, "/")
    if (!identical(candidate, normalized_root) && !startsWith(candidate, root_prefix)) {
        stop("Artifact path escapes its runtime root", call. = FALSE)
    }
    candidate
}

#' Compare two scalar numbers using the contract's absolute/relative tolerance.
#'
#' @param base Baseline scalar value.
#' @param candidate Candidate scalar value.
#' @param absolute Absolute tolerance.
#' @param relative Relative tolerance multiplier.
#' @return `TRUE` when values are equal or within the supplied tolerance.
#' @noRd
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
    # Use the larger magnitude for relative tolerance, while retaining an
    # absolute floor for values near zero.
    difference <- abs(base - candidate)
    allowance <- max(absolute, relative * max(abs(base), abs(candidate)))
    is.finite(difference) && difference <= allowance
}

#' Compare atomic vectors while preserving names and exact non-numeric values.
#'
#' @param base Baseline atomic vector.
#' @param candidate Candidate atomic vector.
#' @param absolute Absolute numeric tolerance.
#' @param relative Relative numeric tolerance multiplier.
#' @param allow_numeric_tolerance Whether numeric values may use tolerance.
#' @return `TRUE` when the vectors have equivalent protected semantics.
#' @noRd
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

#' Recursively compare JSON-like values with optional numeric tolerance.
#'
#' @param base Baseline scalar, vector, or named-list value.
#' @param candidate Candidate scalar, vector, or named-list value.
#' @param absolute Absolute numeric tolerance.
#' @param relative Relative numeric tolerance multiplier.
#' @param allow_numeric_tolerance Whether nested numeric values may use tolerance.
#' @return `TRUE` when both values have equivalent structure and semantics.
#' @noRd
compare_values <- function(base, candidate, absolute, relative, allow_numeric_tolerance = TRUE) {
    if (is.null(base) || is.null(candidate)) {
        return(is.null(base) && is.null(candidate))
    }
    if (is.list(base) && is.list(candidate)) {
        # Named JSON objects are compared by key; unnamed arrays retain order.
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

#' Read a JSON artifact without coercing protected values to tabular vectors.
#'
#' @param path Path to a JSON artifact.
#' @return The parsed JSON value as scalars, vectors, and lists.
#' @noRd
read_json_artifact <- function(path) {
    jsonlite::read_json(path, simplifyVector = FALSE)
}

#' Read a UTF-8 text artifact as its exact sequence of lines.
#'
#' @param path Path to a text artifact.
#' @return Character vector containing the artifact lines.
#' @noRd
read_text_artifact <- function(path) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
}

#' Read one XML member from an OOXML archive with temporary extraction cleanup.
#'
#' @param path Path to an OOXML workbook archive.
#' @param member Archive member path to extract and parse.
#' @return An `xml2` document for the requested archive member.
#' @noRd
archive_xml <- function(path, member) {
    extraction_dir <- tempfile("important-results-xml-")
    dir.create(extraction_dir, recursive = TRUE, showWarnings = FALSE)
    # Extract only the requested member and always remove the temporary tree,
    # including when unzip or XML parsing raises an error.
    on.exit(unlink(extraction_dir, recursive = TRUE, force = TRUE), add = TRUE)
    utils::unzip(path, files = member, exdir = extraction_dir)
    xml2::read_xml(file.path(extraction_dir, member))
}

#' List the member names in an OOXML archive.
#'
#' @param path Path to an OOXML workbook archive.
#' @return Character vector of archive member names.
#' @noRd
archive_members <- function(path) {
    utils::unzip(path, list = TRUE)$Name
}

#' Retrieve an XML attribute without depending on its namespace prefix.
#'
#' @param node XML node whose attributes should be inspected.
#' @param name Local (namespace-independent) attribute name.
#' @return Attribute value, or `NULL` when the attribute is absent.
#' @noRd
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

#' Return the untrimmed text of the first XML node matching an XPath.
#'
#' @param node XML node/document used as the XPath search root.
#' @param xpath XPath expression evaluated below `node`.
#' @return Text of the first matching node, or `NULL` when no node matches.
#' @noRd
first_node_text <- function(node, xpath) {
    nodes <- xml2::xml_find_all(node, xpath)
    if (!length(nodes)) {
        return(NULL)
    }
    xml2::xml_text(nodes[[1L]], trim = FALSE)
}

#' Resolve an OOXML relationship target to a normalized `xl/` member path.
#'
#' @param target Relationship target from workbook relationships XML.
#' @return Normalized archive member path.
#' @noRd
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

#' Load shared-string values referenced by an OOXML workbook.
#'
#' @param path Path to an OOXML workbook archive.
#' @return Character vector indexed by the workbook shared-string table.
#' @noRd
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

# OOXML's built-in IDs are compared by their format code, not by their numeric
# ID.  This lets a producer switch between a built-in format and an equivalent
# custom `<numFmt>` without making an irrelevant style-table change visible.
BUILTIN_NUMBER_FORMATS <- c(
    `0` = "General",
    `1` = "0",
    `2` = "0.00",
    `3` = "#,##0",
    `4` = "#,##0.00",
    `5` = "$#,##0_);($#,##0)",
    `6` = "$#,##0_);[Red]($#,##0)",
    `7` = "$#,##0.00_);($#,##0.00)",
    `8` = "$#,##0.00_);[Red]($#,##0.00)",
    `9` = "0%",
    `10` = "0.00%",
    `11` = "0.00E+00",
    `12` = "# ?/?",
    `13` = "# ??/??",
    `14` = "mm-dd-yy",
    `15` = "d-mmm-yy",
    `16` = "d-mmm",
    `17` = "mmm-yy",
    `18` = "h:mm AM/PM",
    `19` = "h:mm:ss AM/PM",
    `20` = "h:mm",
    `21` = "h:mm:ss",
    `22` = "m/d/yy h:mm",
    `37` = "#,##0_);(#,##0)",
    `38` = "#,##0_);[Red](#,##0)",
    `39` = "#,##0.00_);(#,##0.00)",
    `40` = "#,##0.00_);[Red](#,##0.00)",
    `41` = "_(* #,##0_);_(* \\(#,##0\\);_(* \"-\"_);_(@_)",
    `42` = "_(\"$\"* #,##0_);_(\"$\"* \\ (#,##0\\);_(\"$\"* \"-\"_);_(@_)",
    `43` = "_(* #,##0.00_);_(* \\(#,##0.00\\);_(* \"-\"??_);_(@_)",
    `44` = "_(\"$\"* #,##0.00_);_(\"$\"* \\ (#,##0.00\\);_(\"$\"* \"-\"??_);_(@_)",
    `45` = "mm:ss",
    `46` = "[h]:mm:ss",
    `47` = "mmss.0",
    `48` = "##0.0E+0",
    `49` = "@"
)

#' Canonicalize an OOXML number-format code for semantic comparison.
#'
#' @param code Number-format code as stored in a built-in or custom OOXML
#'   format table.
#' @return A canonical format code string. Formatting tokens and whitespace
#'   are preserved because spaces can change the displayed result.
#' @noRd
canonical_number_format <- function(code) {
    if (is.null(code) || !length(code) || is.na(code[[1L]])) {
        return("General")
    }
    as.character(code[[1L]])
}

#' Parse workbook style tables into effective cell number-format codes.
#'
#' @param path Path to an OOXML workbook archive.
#' @return A list containing one effective format code for each `cellXfs`
#'   style reference.
#' @noRd
workbook_styles <- function(path) {
    members <- archive_members(path)
    if (!"xl/styles.xml" %in% members) {
        return(list(cell_formats = character()))
    }

    styles <- archive_xml(path, "xl/styles.xml")
    custom_nodes <- xml2::xml_find_all(styles, ".//*[local-name()='numFmts']/*[local-name()='numFmt']")
    custom_formats <- setNames(
        vapply(custom_nodes, attribute_by_local_name, character(1), name = "formatCode"),
        vapply(custom_nodes, attribute_by_local_name, character(1), name = "numFmtId")
    )
    cell_xfs <- xml2::xml_find_all(styles, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
    if (!length(cell_xfs)) {
        return(list(cell_formats = character()))
    }

    # A cell's `s` attribute indexes `cellXfs`; only the effective number
    # format matters, so unrelated font/fill/border/style IDs are ignored.
    cell_formats <- vapply(cell_xfs, function(xf) {
        format_id <- attribute_by_local_name(xf, "numFmtId")
        if (is.null(format_id) || !nzchar(format_id)) {
            format_id <- "0"
        }
        code <- if (format_id %in% names(custom_formats)) custom_formats[[format_id]] else NULL
        if (is.null(code) && format_id %in% names(BUILTIN_NUMBER_FORMATS)) {
            code <- BUILTIN_NUMBER_FORMATS[[format_id]]
        }
        if (is.null(code)) {
            # Unknown IDs cannot be safely interpreted; retaining the ID is a
            # fail-closed fallback while still avoiding raw style-ID equality.
            code <- paste0("[unknown-numFmtId=", format_id, "]")
        }
        canonical_number_format(code)
    }, character(1))
    list(cell_formats = unname(cell_formats))
}

#' Resolve a worksheet cell style reference to its effective number format.
#'
#' @param styles Parsed workbook style tables.
#' @param style_id Zero-based OOXML cell style reference, or `NULL` when the
#'   cell has no explicit style.
#' @return Canonical effective number-format code for the cell.
#' @noRd
cell_number_format <- function(styles, style_id) {
    if (is.null(style_id) || !nzchar(style_id)) {
        return("General")
    }
    index <- suppressWarnings(as.integer(style_id)) + 1L
    if (is.na(index) || index < 1L || index > length(styles$cell_formats)) {
        stop("Workbook cell style reference is invalid", call. = FALSE)
    }
    styles$cell_formats[[index]]
}

#' Build a semantic manifest of worksheet dimensions, values, formulas, and formats.
#'
#' @param path Path to an OOXML workbook archive.
#' @return A list of worksheet manifests with ordered cell semantics.
#' @noRd
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
    # Resolve relationship IDs rather than assuming sheet order or filenames.
    relationship_map <- setNames(
        vapply(relationship_targets, resolve_archive_target, character(1)),
        relationship_ids
    )

    sheet_nodes <- xml2::xml_find_all(workbook, ".//*[local-name()='sheets']/*[local-name()='sheet']")
    if (!length(sheet_nodes)) {
        stop("Workbook contains no worksheets", call. = FALSE)
    }
    shared_strings <- shared_strings_for(path)
    styles <- workbook_styles(path)

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
            style_id <- attribute_by_local_name(cell_node, "s")
            raw_value <- first_node_text(cell_node, "./*[local-name()='v']")
            formula <- first_node_text(cell_node, "./*[local-name()='f']")
            cell_ref <- attribute_by_local_name(cell_node, "r")

            # Shared strings store an integer index; inline strings store text
            # beneath the cell, so both must be expanded before comparison.
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
                value = value,
                # Number formats affect displayed numeric cells but have no
                # reader-visible meaning for text/shared-string cells.
                number_format = if (numeric_cell) {
                    cell_number_format(styles, style_id)
                } else {
                    NULL
                }
            )
        })
        # Cell order in XML is not a protected semantic; references are.
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

#' Compare worksheet cells including formulas, values, and effective formats.
#'
#' @param base_cells Baseline ordered cell manifests.
#' @param candidate_cells Candidate ordered cell manifests.
#' @param absolute Absolute numeric tolerance for numeric cached values.
#' @param relative Relative numeric tolerance for numeric cached values.
#' @return `TRUE` when cell references and all protected semantics match.
#' @noRd
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
            !identical(base_cell$kind, candidate_cell$kind) ||
            !identical(base_cell$number_format, candidate_cell$number_format)) {
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

#' Compare two workbooks by worksheet/cell semantics rather than archive bytes.
#'
#' @param base_path Baseline OOXML workbook path.
#' @param candidate_path Candidate OOXML workbook path.
#' @param absolute Absolute numeric tolerance for numeric cached values.
#' @param relative Relative numeric tolerance for numeric cached values.
#' @return `TRUE` when sheets, dimensions, formulas, values, and effective
#'   number formats match.
#' @noRd
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

#' Compare one contract item and return a privacy-safe status record.
#'
#' @param item One validated comparison entry from the contract.
#' @param base_root Baseline runtime root.
#' @param candidate_root Candidate runtime root.
#' @param absolute Absolute numeric tolerance.
#' @param relative Relative numeric tolerance.
#' @return Named list containing only the item ID, type, status, and reason.
#' @noRd
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

#' Write the sanitized comparator report to a requested path.
#'
#' @param path Output JSON report path.
#' @param status Overall `pass` or `fail` status.
#' @param comparisons Per-artifact sanitized comparison records.
#' @return Invisibly returns `NULL` after writing the report.
#' @noRd
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

#' Execute all contract comparisons and return a process status code.
#'
#' @param options Named options list returned by [parse_cli()].
#' @return Integer zero on success and one when any required comparison fails.
#' @noRd
run_comparison <- function(options) {
    contract <- load_contract(options$contract)
    comparisons <- lapply(contract$comparisons, compare_one, options$`base-runtime`, options$`candidate-runtime`, contract$numeric_tolerance$absolute, contract$numeric_tolerance$relative)
    passed <- all(vapply(comparisons, function(item) identical(item$status, "pass"), logical(1)))
    write_report(options$report, if (passed) "pass" else "fail", comparisons)
    if (passed) 0L else 1L
}

#' Run the comparator with fail-closed contract-error reporting.
#'
#' @param args Character vector of command-line arguments; defaults to the
#'   process trailing arguments.
#' @return Integer process status code (zero for a fully passing comparison).
#' @noRd
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
