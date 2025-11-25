# Centralized color palette utilities

#' Return a qualitative palette of up to 8 colors with project-consistent hues
#' @param n Number of colors requested
#' @return Character vector of hex colors
get_qualitative_palette <- function(n) {
    base <- c(
        "#0072B5FF", # blue
        "#BC3C29FF", # red
        "#E18727FF", # orange
        "#20854EFF", # green
        "#7876B1FF", # purple
        "#6F99ADFF", # steel
        "#FFDC91FF", # sand
        "#EE4C97FF" # pink
    )
    if (n <= length(base)) {
        return(base[seq_len(n)])
    }
    # Fallback to RColorBrewer Set1 if more are needed
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        return(RColorBrewer::brewer.pal(min(n, 12), "Set3")[seq_len(n)])
    }
    return(rep(base, length.out = n))
}

#' Consistent colors for GEP classes
#' Supports legacy (Class 1A/1B/2), binary (Class 1/2), and 4-class PRAME system
#' @param levels Character vector of class levels (e.g., c("Class 1 PRAME Negative","Class 1 PRAME Positive", ...))
#' @return Named vector mapping levels to colors
get_gep_class_palette <- function(levels) {
    mapping <- c(
        # Legacy 3-class
        "Class 1A" = "#0072B5FF", # blue
        "Class 1B" = "#E18727FF", # orange
        "Class 2"  = "#BC3C29FF", # red
        # Binary simple
        "Class 1"  = "#0072B5FF",
        # keep Class 2 same red
        # 4-class PRAME system
        "Class 1 PRAME Negative" = "#0072B5FF", # blue
        "Class 1 PRAME Positive" = "#20854EFF", # green
        "Class 2 PRAME Negative" = "#E18727FF", # orange
        "Class 2 PRAME Positive" = "#BC3C29FF", # red
        "GEP Not Tested" = "#9E9E9EFF", # gray
        "GEP Failed/Indeterminate" = "#6F99ADFF",
        "No" = "#9E9E9EFF" # legacy support
    )
    lv <- unique(as.character(levels))
    out <- mapping[lv]
    # For any unknown levels, append from qualitative palette after the known ones
    if (any(is.na(out))) {
        needed <- sum(is.na(out))
        fill <- setdiff(get_qualitative_palette(length(lv)), unname(mapping))
        out[is.na(out)] <- head(fill, needed)
    }
    names(out) <- lv
    out
}

#' Consistent colors for PRAME status
#' @param levels Character vector of PRAME levels (e.g., c("Positive","Negative","Unknown"))
#' @return Named vector mapping levels to colors
get_prame_palette <- function(levels) {
    mapping <- c(
        "Positive" = "#BC3C29FF", # red
        "Negative" = "#20854EFF", # green
        "Unknown"  = "#6F99ADFF" # steel
    )
    lv <- unique(as.character(levels))
    out <- mapping[lv]
    if (any(is.na(out))) {
        needed <- sum(is.na(out))
        fill <- setdiff(get_qualitative_palette(length(lv)), unname(mapping))
        out[is.na(out)] <- head(fill, needed)
    }
    names(out) <- lv
    out
}

#' Consistent colors for treatment group
#' @param levels Character vector of treatment groups (e.g., c("Plaque","GKSRS","Other"))
#' @return Named vector mapping levels to colors
get_treatment_palette <- function(levels) {
    mapping <- c(
        "PBT" = "#0072B5FF", # blue
        "GKSRS"  = "#BC3C29FF", # red
        "Other"  = "#E18727FF" # orange
    )
    lv <- unique(as.character(levels))
    out <- mapping[lv]
    if (any(is.na(out))) {
        needed <- sum(is.na(out))
        fill <- setdiff(get_qualitative_palette(length(lv)), unname(mapping))
        out[is.na(out)] <- head(fill, needed)
    }
    names(out) <- lv
    out
}

#' Consistent colors for recurrence status (Yes/No)
#' Uses colors distinct from treatment palette (blue/red)
#' @param levels Character vector of recurrence levels (e.g., c("Yes","No"))
#' @return Named vector mapping levels to colors
get_recurrence_palette <- function(levels) {
    mapping <- c(
        "No"  = "#20854EFF", # green - no recurrence (good outcome)
        "Yes" = "#7876B1FF"  # purple - recurrence
    )
    lv <- unique(as.character(levels))
    out <- mapping[lv]
    if (any(is.na(out))) {
        needed <- sum(is.na(out))
        fill <- setdiff(get_qualitative_palette(length(lv)), unname(mapping))
        out[is.na(out)] <- head(fill, needed)
    }
    names(out) <- lv
    out
}

#' Dispatcher to choose palette based on variable semantics
#' @param variable_name Name of the variable being visualized (e.g., 'gep_class_simple','prame_status','treatment_group')
#' @param levels Character vector of factor levels in plotting order
#' @return Character vector of colors in the same order as levels
get_palette_by_variable <- function(variable_name, levels) {
    lv <- unique(as.character(levels))
    pal <- switch(variable_name,
        "gep_class_simple" = get_gep_class_palette(lv),
        "biopsy1_gep" = get_gep_class_palette(lv),
        "prame_status" = get_prame_palette(lv),
        "treatment_group" = get_treatment_palette(lv),
        "recurrence1" = get_recurrence_palette(lv),
        # default fallback
        get_qualitative_palette(length(lv))
    )
    # Ensure order corresponds to levels
    if (!is.null(names(pal))) {
        pal[lv]
    } else {
        pal
    }
}
