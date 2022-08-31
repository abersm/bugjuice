#' Plot distribution of values using density function
#'
#' @param df Data frame or numeric vector
#' @param col Continuous variable. Enter as quoted or unquoted variable name
#' @param grouping_var Grouping variable. Enter as quoted or unquoted variable name
#' @param colors Colors used to fill density plot. Default is blue and red
#' @param alpha Alpha for density color. Default is `0.6`
#' @param line_color Line color. Default is `black`
#' @param line_thickness Line thickness. Default is `1`
#' @param show_data If `TRUE`, a symbol is plotted for each observation
#' @param symbol Type of symbol used to show data. Options: `"none"` (default), `"point"`, `"rug"`
#' @param breaks x axis breaks
#' @param x_axis_title x axis title
#' @param x_title Alias for `x_axis_title`
#' @param axis_labels Function used to generate axis labels
#' @param show_legend If `FALSE` (default), legend is not displayed
#' @param scaling_fn Scaling function used to transform variable. Default performs no transformation
#' @param expand_y Expansion to add to y axis. Default is `0.2`
#' @param plot_title Title for plot. Default is `NULL`
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_density <- function(df, col = NULL, grouping_var = NULL, colors = c("#0072B5", "#BC3C29"), alpha = 0.6, line_color = "black", line_thickness = 1, show_data = FALSE, symbol = "none", breaks = waiver(), x_title = NULL, x_axis_title = x_title, axis_labels = axis_label_numeric, show_legend = NULL, scaling_fn = identity, expand_y = 0.02, plot_title = NULL, ...) {
  col <- get_input(col)
  grouping_var <- get_input(grouping_var)
  x <- if (inherits(df, "data.frame")) df[[col]] else df
  x <- scaling_fn(x)
  if (!is.null(grouping_var)) {
    group <- factor(df[[grouping_var]])
    n_groups <- length(levels(group))
    show_legend <- show_legend %||% TRUE
  } else {
    group <- rep("a", length(x))
    n_groups <- 1
    show_legend <- show_legend %||% FALSE
  }
  df <- vec_to_df(x = x, group = group)
  colors <- rep(colors, length.out = n_groups)

  breaks <- pretty(df$x, n = 4)
  breaks_length <- length(breaks)
  if (breaks_length > 6 && is_odd(breaks_length)) {
    breaks <- breaks[seq(1, breaks_length, by = 2)]
  }
  if (breaks_length == 3L) {
    breaks <- seq(breaks[1], breaks[3], by = (breaks[3] - breaks[1])/4)
  }
  x_limits <- range(breaks, df$x)
  ggplot(df, aes(x = x, group = group, fill = group)) +
    geom_density(color = line_color, size = line_thickness, alpha = alpha, na.rm = TRUE, show.legend = show_legend) +
    scale_fill_manual(NULL, values = colors) +
    scale_x_continuous(name = x_axis_title, limits = x_limits, breaks = breaks, expand = rep(0, 4)) +
    scale_y_continuous(expand = c(expand_y, 0, 0, 0)) +
    coord_cartesian(clip = "off") +
    theme_custom(...) +
    ggtitle(plot_title) +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), legend.key.height = if (show_legend) unit(5, "pt") else NULL, legend.key.width = if (show_legend) unit(15, "pt") else NULL)
}
