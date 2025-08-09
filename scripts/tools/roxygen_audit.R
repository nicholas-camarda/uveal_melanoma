# Roxygen Audit
# Scans scripts/ for function definitions and checks whether a roxygen2
# header (lines starting with #') immediately precedes each function.

suppressWarnings(suppressMessages({
  library(stringr)
  library(purrr)
  library(dplyr)
}))

root_dir <- "scripts"
r_files <- list.files(root_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

fun_regex <- "(^|[[:space:]])([A-Za-z0-9_.]+)[[:space:]]*<-[[:space:]]*function[[:space:]]*\\("

#' Check a file for function definitions and nearby roxygen headers
#'
#' @param path Character path to the file to scan
#' @return Tibble with file, line, function name, and logical has_roxy
check_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  fun_lines <- which(str_detect(lines, fun_regex))
  if (length(fun_lines) == 0) return(tibble(file = character(), line = integer(), fun = character(), has_roxy = logical()))
  
  # Extract function names
  get_fun_name <- function(idx) {
    #' Extract function name at a given line
    #' @param idx Integer line index
    #' @return Character function name
    m <- str_match(lines[idx], fun_regex)
    m[,3]
  }
  tibble(
    file = path,
    line = fun_lines,
    fun = map_chr(fun_lines, get_fun_name),
    has_roxy = map_lgl(fun_lines, function(idx) {
      # look up to 5 lines above for any line starting with #' (roxygen)
      start <- max(1, idx - 5)
      any(str_detect(lines[start:idx], "^#'") )
    })
  )
}

res <- map_dfr(r_files, check_file)

missing <- res %>% filter(!has_roxy)
if (nrow(missing) == 0) {
  cat("All functions have nearby roxygen documentation.\n")
} else {
  cat("Functions missing roxygen documentation (no #' within 5 lines above):\n")
  missing %>% mutate(msg = paste0(" - ", file, ":", line, " ", fun)) %>%
    pull(msg) %>% cat(sep = "\n")
  cat("\n")
}
