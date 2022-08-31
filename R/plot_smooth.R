#' Plot regression line or loess smoothed mean per group
#'
#' @param df Data frame in long format containing continuous variable (y), categorical or integer variable (x), and grouping variable (`grouping_var`)
#' @param formula y ~ x format
#' @param y,x Variables used for plotting. Enter as quoted or unquoted variable names
#' @param grouping_var Variable to group by. Enter as quoted or unquoted variable name
#' @param smoothing_fn Function used for smoothing. Options: `"loess"` (default) `"lm"`, `"glm"`
#' @param x_range_data Range of x values for input data. Default is -Inf to Inf. Enter as length 2 numeric vector
#' @param colors Color used for points and lines
#' @param line_type Type of line. Options include `"solid"` (default), `"dashed"`, `"longdash"`, `"twodash"`, `"dotdash"`, `"dotted"`. Enter as quoted linetype
#' @param line_colors Color for lines. Hexadecimal or quoted color names. Default is `"black"`
#' @param line_thickness Thickness of line in mm Default is `1`
#' @param plot_formula Formula for calculation. Default is `y ~ x`
#' @param show_error If `TRUE` (default), error estimate for curve displayed as shaded region
#' @param ci Confidence interval used for error region. Default is `0.95`
#' @param error_fill_color Color of shaded error region. Default is `"grey60"`
#' @param show_points If `FALSE` (default), points not displayed. If `TRUE`, raw data points displayed
#' @param point_shape Shape of points. Options include `"square"`, `"circle"`, `"triangle"`, `"diamond"`, or list of shapes for each group. Enter as quoted or unquoted shape or list of shapes using `c()`
#' @param point_size Point size. Default is `2`
#' @param point_colors Fill color for points. Hexadecimal or quoted color names. Enter using `c()`
#' @param point_border_colors Color for line surrounding points. Default is `"black"`
#' @param point_alpha Alpha value for point fill. Default is `1`
#' @param point_border_thickness Thickness of line surrounding points in pt units. Default is `1`
#' @param y_scale Scaling for y-axis. Options: `"regular"` (default) or `"log"`
#' @param breaks_fn Function used to generate y axis breaks. Default is `pretty`
#' @param n_breaks Number of tick marks for y axis. Default is `4`
#' @param expand_y Expansion of y axis around y = 0. Default is `0.1`
#' @param y_axis_label_fn Vector or function specifying y axis tick labels. Default is `axis_label_numeric.` Use axis_label_x10 for 10^x labels
#' @param y_axis_title Enter as quoted string. Default is `""`
#' @param x_axis_title Enter as quoted string. Default is `"Days post-infection"`
#' @param x_axis_breaks Enter as numeric vector of x axis breaks
#' @param x_min Minimum value to use for x axis
#' @param x_max Maximum value to use for x axis
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""` (i.e. no title)
#' @param show_legend If `TRUE`, legend displayed. If `FALSE`, legend not displayed
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param ... Arguments passed to `theme_custom()`
#' @return Plot with loess smoothed means for each group
#' @export
plot_smooth <- function(df, formula = NULL, y = NULL, x = NULL, grouping_var = NULL, smoothing_fn = stats::loess, x_range_data = c(-Inf, Inf), show_error = TRUE, ci = 0.95, line_type = "solid", colors = c("#2A2D34", "#BC3C29", "#00A1D5", "#6761A8", "#009872"), line_colors = colors, line_thickness = 1,  plot_formula = y ~ x, show_points = FALSE, point_colors = colors, point_shape = "circle", point_size = 2, point_border_thickness = 1, point_border_colors = "black", point_alpha = 1, y_scale = "regular", expand_y = 0.1, y_axis_label_fn = axis_label_numeric, y_axis_title = NULL, x_axis_title = NULL, x_axis_breaks = NULL, breaks_fn = pretty, x_min = NULL, x_max = NULL, plot_title = NULL, n_breaks = 4, show_legend = FALSE, censor_fn = rescale_none, error_fill_color = "grey60", ...) {
  grouping_var <- get_input(grouping_var)
  plot_fn <- "plot_smooth"
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y))
  x <- vars$x
  y <- vars$y
  df$x <- df[[x]]
  df$y <- df[[y]]
  if (is.null(grouping_var)) {
    df$group <- factor("a")
    grouping_var <- "group"
  }
  df$group <- factor(df[[grouping_var]], levels = create_levels(df[[grouping_var]]))
  df <- df[complete.cases(df[c("x", "y", "group")]), ]
  df <- dplyr::filter(df, dplyr::between(x, x_range_data[1], x_range_data[2]))
  if (y_scale == "log") {
    df$y <- ifelse(df$y == 0, 0, log10(df$y))
  }
  n_groups <- n_unique(df$group, na.rm = FALSE)
  line_colors <- rep_len(line_colors, length.out = n_groups)
  point_colors <- rep_len(point_colors, length.out = n_groups)

  # CORE PLOT
  p <- ggplot(df, aes(x, y, group = group)) +
    geom_smooth(aes(color = group), method = smoothing_fn, formula = plot_formula, se = show_error, fill = error_fill_color, level = ci, show.legend = show_legend) +
    scale_color_manual(name = "", values = line_colors)

  # POINTS
  if (show_points) {
    if (length(point_shape) == 1) {
      point_shape <- switch(point_shape, circle = 21, square = 22, triangle = 24, diamond = 23)
    }
    plot_points <- list(geom_point(mapping = aes(fill = group), shape = point_shape, size = point_size, stroke = point_border_thickness, show.legend = show_legend), scale_fill_manual(name = "", values = point_colors))
  } else {
    plot_points <- NULL
  }

  # X AXIS
  x_min <- x_min %||% min(df$x)
  x_max <- x_max %||% max(df$x)
  x_breaks <- x_axis_breaks %||% axis_breaks_continuous()(c(x_min, x_max))

  # Y AXIS
  y_plot_limits <- get_plot_data_limits(p, axis = "y")
  y_breaks <- .create_axis_breaks(.limits = y_plot_limits, .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)
  y_plot_limits <- range(y_plot_limits, y_breaks)

  # PLOT
  p <- p +
    plot_points +
    ggtitle(plot_title) +
    scale_x_continuous(name = x_axis_title, limits = range(x_breaks), breaks = x_breaks, oob = censor_fn) +
    scale_y_continuous(name = y_axis_title, limits = y_plot_limits, expand = c(expand_y, 0, 0, 0), oob = censor_fn, breaks = y_breaks, labels = y_axis_label_fn) +
    theme_custom(...)
  suppressWarnings(print(p))
}
