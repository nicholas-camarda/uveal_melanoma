# Plot Utilities

#' Remove existing plot scales for selected aesthetics
#'
#' Removes scales from a ggplot object when a caller intends to replace them and
#' wants to avoid duplicate-scale messages from ggplot2 or wrappers such as
#' survminer and ggsurvfit.
#'
#' @param plot A ggplot object.
#' @param aesthetics Character vector of aesthetics to remove, such as
#'   `c("colour", "y")`.
#' @return The ggplot object with matching scales removed.
remove_plot_scales <- function(plot, aesthetics = character()) {
    if (is.null(plot) || is.null(plot$scales) || length(plot$scales$scales) == 0) {
        return(plot)
    }

    aesthetics <- unique(tolower(aesthetics))
    keep_scale <- vapply(
        plot$scales$scales,
        function(scale_obj) {
            scale_aesthetics <- tolower(scale_obj$aesthetics %||% character())
            length(intersect(scale_aesthetics, aesthetics)) == 0
        },
        logical(1)
    )

    plot$scales$scales <- plot$scales$scales[keep_scale]
    plot
}
