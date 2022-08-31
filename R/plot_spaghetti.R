#' Spaghetti plot
#'
#' @param df Data frame in long format with columns for id, y (continuous variable), x (continuous variable)
#' @param formula y ~ x format
#' @param id Variable to identify subject. Enter as quoted variable name
#' @param line_color Color for lines. Hexadecimal or quoted color names. Default is `"black"`
#' @param line_color_var Variable used to determine line color. Enter as quoted variable name
#' @param line_color_categorical If `TRUE` (default), line color variable determined by `line_color_var` is set to factor (which uses a categorical palette). If `FALSE`, `line_color_var` will be converted to a continuous variable
#' @param line_thickness Thickness of line in mm Default is `1`
#' @param y_scale Scaling for y-axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param y_axis_breaks Numeric vector or function specifying location of ticks along y axis
#' @param y_min Minimum value to use for y axis Default is `NULL`
#' @param y_max Maximum value to use for y axis Default is `NULL`
#' @param expand_y Expansion of y axis around y = 0. Default is `0.1`
#' @param y_axis_labels Vector or function specifying y axis tick labels
#' @param y_axis_title Enter as quoted string. Default is `NULL`
#' @param expand_x Expansion of x axis at ends. Default is `0.1`
#' @param x_axis_title Enter as quoted string. Default is `NULL`
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""`
#' @param show_legend If `FALSE` (default), legend is not displayed. If `TRUE`, legend for lines is shown
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_spaghetti <- function(
  df,
  formula,
  id,
  line_color_var = NULL,
  line_color_categorical = TRUE,
  line_color = "#3B3B3B",
  line_thickness = 1,
  y_scale = "regular",
  n_breaks = 3,
  y_axis_breaks = NULL,
  y_min = NULL,
  y_max = NULL,
  expand_y = 0.1,
  y_axis_labels = NULL,
  y_axis_title = NULL,
  expand_x = 0.1,
  x_axis_title = NULL,
  plot_title = "",
  show_legend = FALSE,
  censor_fn = rescale_none,
  ...) {
  # Create df with all variables
  df <- .create_plot_df(df, formula)
  df$x_numeric <- as_numeric_factor(df$x)
  df$id_var <- df[[id]]

  if (is.null(line_color_var)) {
    df$line_color_var <- factor("a")
  } else {
    df$line_color_var <- df[[line_color_var]]
    if (line_color_categorical) {
      line_color_levels <- create_levels(df$line_color_var)
      df$line_color_var <- factor(df$line_color_var, levels = line_color_levels)
    } else {
      if (all(can_be_numeric(df$line_color_var))) {
        df$line_color_var <- as.numeric(df$line_color_var)
      } else {
        line_color_levels <- create_levels(df$line_color_var)
        df$line_color_var <- factor(df$line_color_var, levels = line_color_levels)
      }
    }
  }

  n_line_colors <- n_unique(df$line_color_var, na.rm = FALSE)
  if (length(line_color) != n_line_colors) {
    if (is.numeric(df$line_color_var)) {
      low <- grDevices::col2rgb("grey80")
      high <- grDevices::col2rgb(line_color[[1]])
      red <- seq(low[1, 1], high[1, 1], length = n_line_colors)/255
      green <- seq(low[3, 1], high[3, 1], length = n_line_colors)/255
      blue <- seq(low[2, 1], high[2, 1], length = n_line_colors)/255
      line_color <- grDevices::rgb(red, blue, green)
    } else {
      line_color <- c(line_color, palettes$color1_hex)
    }
  }

  # y axis breaks
  if (is.null(y_axis_breaks)) {
    y_max <- y_max %||% Max(df$y)
    y_min <- y_min %||% Min(df$y)
  } else {
    y_max <- Max(y_axis_breaks)
    y_min <- Min(y_axis_breaks)
  }

  # Plot
  ggplot(df, aes(x, y)) +
    geom_line(aes(group = id_var, color = line_color_var), size = line_thickness, show.legend = show_legend) +
    scale_color_manual(values = line_color, name = NULL) +
    scale_x_discrete(name = x_axis_title, expand = ggplot2::expansion(mult = expand_x)) +
    scale_continuous(axis = "y", limits = c(y_min, y_max), title = y_axis_title, expand_lower = expand_y, scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, censor_fn = censor_fn) +
    ggtitle(plot_title) +
    coord_cartesian(clip = "off") +
    theme_custom(...)
}
