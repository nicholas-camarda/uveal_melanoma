# Duplicate Function Scanner (sourced files only)
# Finds top-level function definitions across files that are actually sourced
# via scripts/utils/load_all.R and reports cross-file duplicates.

suppressWarnings(suppressMessages({
    library(dplyr)
}))

# Collect sourced files from load_all.R using base regex
sourced_files <- {
    txt <- readLines("scripts/utils/load_all.R", warn = FALSE)
    matches <- regmatches(txt, gregexpr("source\\(\"([^\"]+)\"\\)", txt, perl = TRUE))
    found <- unique(unlist(matches))
    if (length(found) == 0) {
        character(0)
    } else {
        # extract inside quotes
        paths <- sub("^source\\(\"([^\"]+)\"\\)$", "\\1", found)
        unique(paths)
    }
}

# Restrict to scripts that are actually sourced
r_files <- sourced_files[file.exists(sourced_files)]

#' Extract top-level function names from a file using parse data
#' @param path Character, file path
#' @return Tibble with columns file and fun (function name)
# Parse-based extraction of top-level function names: for each FUNCTION token,
# find the sibling SYMBOL on the same assignment
extract_top_level_functions <- function(path) {
    expr <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
    if (is.null(expr)) {
        return(tibble(file = character(), fun = character()))
    }
    pd <- tryCatch(utils::getParseData(expr), error = function(e) NULL)
    if (is.null(pd)) {
        return(tibble(file = character(), fun = character()))
    }
    fun_tokens <- pd[pd$token == "FUNCTION", , drop = FALSE]
    res <- list()
    for (i in seq_len(nrow(fun_tokens))) {
        parent_id <- fun_tokens$parent[i]
        siblings <- pd[pd$parent == parent_id, , drop = FALSE]
        sym <- siblings[siblings$token == "SYMBOL", , drop = FALSE]
        assign <- siblings[siblings$token == "LEFT_ASSIGN", , drop = FALSE]
        if (nrow(sym) == 1 && nrow(assign) >= 1) {
            res[[length(res) + 1]] <- tibble(file = path, fun = sym$text[1])
        }
    }
    if (length(res) == 0) {
        return(tibble(file = character(), fun = character()))
    }
    dplyr::bind_rows(res) %>% dplyr::distinct()
}

fun_map <- do.call(rbind, lapply(r_files, extract_top_level_functions))
fun_map <- tibble::as_tibble(fun_map)

duplicates <- fun_map %>%
    dplyr::distinct(file, fun) %>%
    dplyr::count(fun, name = "file_count") %>%
    dplyr::filter(file_count > 1)

if (nrow(duplicates) == 0) {
    cat("No cross-file duplicate function definitions found among sourced files.\n")
} else {
    cat("Duplicate functions defined across multiple sourced files:\n")
    fun_map %>%
        dplyr::semi_join(duplicates, by = "fun") %>%
        dplyr::arrange(fun, file) %>%
        dplyr::group_by(fun) %>%
        dplyr::summarise(files = paste(unique(file), collapse = ", ")) %>%
        dplyr::mutate(msg = paste0(" - ", fun, ": ", files)) %>%
        dplyr::pull(msg) %>%
        cat(sep = "\n")
    cat("\n")
}
