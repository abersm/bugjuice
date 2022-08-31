#' Box plot
#'
#' @inheritParams plot_bar_point
#' @inheritParams plot_point
#' @param color_scale Component of boxplot to color. Options: `"fill"` (default) or `"color"`
#' @param color_var Variable used to color boxes. Enter as quoted or unquoted variable name
#' @param colors Hexadecimal or quoted color names. Enter using `c()`. Default uses black/red
#' @param alpha Alpha value for box fill. Default is `1`
#' @param border_thickness Thickness of line surrounding boxes in pt units. Default is `0.75`
#' @param border_colors Color of line surrounding boxes. Default is `"black"`
#' @param width Width of boxes. Default is `0.5`
#' @param errorbar_thickness Thickness of errorbar lines. Default is `border_thickness`
#' @param show_points If `TRUE`, points are displayed. Default is `FALSE`
#' @param x_axis_label_angle Angle for x axis labels. Default is `45`
#' @param x_angle Alias for `x_axis_label_angle`
#' @param base_size Font size used in theme
#' @export
plot_box <- function(
  df,
  formula,
  grouping_var = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  color_scale = "fill",
  color_var = NULL,
  colors = c("#2A2D34", "#00A1D5", "#D75725", "#6761A8", "#009872"),
  alpha = 1,
  border_thickness = 0.75,
  border_colors = "black",
  width = NULL,
  errorbar_thickness = border_thickness,
  errorbar_width_multiplier = 0.5,
  show_points = FALSE,
  point_shapes = "circle",
  point_shape_var = NULL,
  point_color_var = NULL,
  point_colors = "#2A2D34",
  point_size = 0.5,
  jitter_width = 0.2,
  show_legend = FALSE,
  legend_title = "",
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  y_axis_title = "",
  x_axis_title = NULL,
  plot_title = "",
  dodge = 0.7,
  n_breaks = 3,
  y_max = NULL,
  y_min = NULL,
  expand_y = 0.1,
  x_axis_labels = waiver(),
  censor_fn = rescale_none,
  x_axis_label_angle = 45,
  x_angle = x_axis_label_angle,
  base_size = 14,
  theme_fn = theme_custom,
  ...) {
  # Clean df
  df <- dplyr::select(df, color_var = {{color_var}}, point_color_var = {{point_color_var}}, point_shape_var = {{point_shape_var}}, grouping_var = {{grouping_var}}, dplyr::everything())
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c("color_var", "point_color_var", "point_shape_var"))
  df$x <- factor(df$x, levels = x_order %||% create_levels(df$x))

  if (is_column(df, "grouping_var")) {
    width <- width %||% (0.64-0.03*n_unique(df$x, na.rm = FALSE))
    df$grouping_var <- as.character(df$grouping_var)
    grouping_var_levels <- create_levels(df$grouping_var, reverse = rev_grouping_var_order)
    df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)
    if (!is_column(df, "color_var")) {
      df$color_var <- df$grouping_var
    }
  } else {
    df$grouping_var <- "a"
    width <- width %||% 0.4
    if (!is_column(df, "color_var")) {
      df$color_var <- df$x
    }
  }
  z <- create_levels(df$color_var)
  df$color_var <- factor(df$color_var, levels = z)
  colors <- rep(colors, length.out = length(z))

  # Points
  if (show_points) {
    n_point_colors <- suppressWarnings(n_unique(df$point_color_var, na.rm = FALSE))
    df$point_color_var <- if (n_point_colors == 0L) {
      n_point_colors <- 1
      "1"
    } else {
      factor(df$point_color_var, levels = create_levels(df$point_color_var))
    }
    point_colors <- rep(point_colors, length.out = n_point_colors)

    n_point_shapes <- suppressWarnings(n_unique(df$point_shape_var, na.rm = FALSE))
    df$point_shape_var <- if (n_point_shapes == 0L) {
      n_point_shapes <- 1
      "1"
    } else {
      factor(df$point_shape_var, levels = create_levels(df$point_shape_var))
    }
    look_up_point_shape <- c(circle = 19, square = 15, triangle = 17, diamond = 18)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
    point_shapes <- rep(point_shapes, length.out = n_point_shapes)
  }

  # Whiskers for box plot
  summary_fn <- function(x) vec_to_df(y = Median(x), ymin = min(x), ymax = max(x))
  p <- ggplot(df, aes(x = x, y = y, group = grouping_var)) +
    stat_summary(geom = "errorbar", fun.data = summary_fn, position = position_dodge(width = dodge), width = width*errorbar_width_multiplier, size = errorbar_thickness)

  # Box plot
  p <- p + geom_boxplot(aes(fill = color_var), color = "black", size = border_thickness, coef = 0, width = width, alpha = alpha, position = position_dodge(width = dodge), show.legend = show_legend, outlier.size = NA, outlier.shape = NA) + scale_fill_manual(NULL, values = colors)

  # Points
  if (show_points) {
    p <- p +
      geom_point(aes(color = point_color_var, shape = point_shape_var), size = point_size, position = position_jitter(width = jitter_width, height = 0), show.legend = FALSE) +
      scale_color_manual(name = NULL, values = point_colors) +
      scale_shape_manual(name = NULL, values = point_shapes)
  }
  p <- p + scale_x_discrete(name = x_axis_title, labels = x_axis_labels)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)
  p <- p + scale_continuous(axis = "y", limits = y_limits, scale = y_scale, n_breaks = n_breaks, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, censor_fn = censor_fn)
  p + ggtitle(plot_title) + coord_cartesian(clip = "off", default = TRUE) + theme_fn(base_size = base_size, x_axis_label_angle = x_angle, ...)
}
