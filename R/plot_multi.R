#' Apply uniform plot dimensions to a list of plots
#'
#' @param plot_list List of plots
#' @returns List of plots with identical dimensions which can be passed to plot_grid function
#' @export
align_plot_dim <- function(plot_list) {
  pkg_required("patchwork")
  # Alternative: max_plot_dim <- patchwork::align_patches(plot_list); lapply(plot_list, function(x) patchwork::set_dim(plot = x, dim = max_plot_dim))
  patchwork::align_patches(plot_list)
}

#' List of multipanel plots for printing
#'
#' @param plot_list List of ggplot objects
#' @param plots_per_slide Number of plots per slide. Default is `4`
#' @param rows Number of rows of plots per slide. Default is `2`
#' @param cols Number of columns of plots per slide. Default is `2`
#' @returns List of multipanel plots that can be plotted using ppt_plot function for printing
#' @export
multislide_plot_list <- function(plot_list, plots_per_slide = 4, rows = 2, cols = plots_per_slide/rows) {
  pkg_required("cowplot")
  if (inherits(plot_list, "ggplot")) return(plot_list)
  if (!missing(cols) && !missing(plots_per_slide) && missing(rows)) {
    rows <- plots_per_slide/cols
  }
  if (!missing(cols) && missing(plots_per_slide) && !missing(rows)) {
    plots_per_slide <- rows*cols
  }
  n_plots <- length(plot_list)
  # n_slides <- ceiling(n_plots/plots_per_slide)
  slide_first_plot <- seq(from = 1, to = n_plots, by = plots_per_slide)
  lapply(slide_first_plot, function(x) {
    plots <- remove_null(plot_list[x:(x + plots_per_slide - 1)])
    plots <- plots[vapply(plots, function(z) any(!is.na(z)), logical(1), USE.NAMES = FALSE)]
    cowplot::plot_grid(plotlist = plots, nrow = rows, ncol = cols, align = "hv", axis = "trbl")
  })
}
