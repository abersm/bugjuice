#' Violin plot
#'
#' @param df Data frame in long format
#' @param formula y ~ x format
#' @param grouping_var Variable to group by. Must have only 2 levels. Enter as quoted or unquoted variable name
#' @param rev_grouping_var_order If `FALSE` (default), levels of grouping_var ordered alphabetically. If `TRUE`, order of grouping_var levels is reversed
#' @param x_order Order of x values along x axis. Options: `NULL` (default), `"ascending"`, `"descending"`, or list of quoted values default is `NULL`
#' @param fn_x_order Function applied to y to determine order of x. Default is `mean`
#' @param rev_x_order Whether to reverse order of x values along x axis. Default is `FALSE`
#' @param x,y Variables for x and y axis, respectively. Enter as quoted or unquoted variable names
#' @param colors Fill color for violin plots. Enter as hexadecimal or quoted color name(s)
#' @param alpha Alpha value for violin fill. Default is `1`
#' @param color_border Color of lines surrounding violin Default is `"black"`
#' @param border_color Alias for `color_border`
#' @param border_thickness Thickness of line surrounding violin in pt units. Default is `0.75`
#' @param width Width of violins. Default is `0.8`
#' @param scaling Method used to make violin size equal across groups. Options: `"width"` (default), `"area"`, `"count"`
#' @param show_legend If `FALSE` (default), no legend displayed. If `TRUE`, legend is displayed
#' @param y_scale Scaling for y-axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param y_title Enter as quoted string. Default is `""`
#' @param y_axis_title Alias for `y_title`
#' @param x_title Enter as quoted string. Default is `""`
#' @param x_axis_title Alias for `x_title`
#' @param x_breaks Numeric vector or function specifying location of ticks along x axis
#' @param x_axis_breaks Alias for `x_breaks`
#' @param x_labels Vector or function specifying x axis tick labels
#' @param x_axis_labels Alias for `x_labels`
#' @param y_breaks Numeric vector or function specifying location of ticks along y axis
#' @param y_axis_breaks Alias for `y_breaks`
#' @param y_labels Vector or function specifying y axis tick labels
#' @param y_axis_labels Alias for `y_labels`
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param y_max Maximum y axis value. Default is `max(df_subgrouped$ms)`
#' @param y_min Minimum value to use for y axis Default is `0`
#' @param expand_y Expansion of y axis around y = 0. Default is `0.1`
#' @param trim If `TRUE` (default), violin tails are trimmed. If `FALSE`, violin tails not trimmed.
#' @param lines Quantile line (horizontal) in violin. Default displays lines at 25th, 50th, and 75th percentile. Enter `NULL` to remove lines
#' @param linetype Line type for border of violins. Options include `"solid"` (default), `"dashed"`, `"dotted"`, `"longdash"`. Enter as quoted or unquoted linetype
#' @param linetype_quantile Line type for quantile lines. Options include `"solid"` (default), `"dashed"`, `"dotted"`, `"longdash"`. Enter as quoted or unquoted linetype
#' @param aspect_ratio Plot aspect ratio. Default is `1`
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""` (i.e. no title)
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param show_points If `FALSE` (default), points not displayed. If `TRUE`, points displayed
#' @param point_method Type of points. Options include `"jitter"` (default) or `"beeswarm"`. Enter as quoted type
#' @param point_size Size of points. Default is `1`
#' @param point_colors Color of points. Default is `"black"`
#' @param point_shapes Shape of points. Default is solid circle (no fill color)
#' @param point_width Width of point spread. Default is width of violin*0.15
#' @param dodge Argument for position dodge. Default is width*1.1
#' @param beeswarm_method Method for `geom_quasirandom` function. Default is `"smiley"`
#' @param n_bins Number of bins. Used in `ggbeeswarm::geom_quasirandom`. Default is `30`
#' @param band_width Bandwidth. Used in `ggbeeswarm::geom_quasirandom`. Default is `n_bins/10`
#' @param ... Arguments passed to theme_custom
#' @export
plot_violin <- function(
    df,
    formula,
    grouping_var = NULL,
    rev_grouping_var_order = FALSE,
    x_order = NULL, fn_x_order = mean, rev_x_order = FALSE,
    x = NULL, y = NULL,
    colors = c("#2A2D34", "#00A1D5", "#D75725", "#6761A8", "#009872"),
    color_border = "black",
    border_color = color_border,
    alpha = 1,
    width = 0.7,
    border_thickness = 0.75,
    show_legend = FALSE,
    y_scale = "regular",
    y_title = NULL, y_axis_title = y_title,
    x_title = NULL, x_axis_title = x_title,
    plot_title = NULL,
    n_breaks = 3,
    x_breaks = NULL,
    x_axis_breaks = x_breaks,
    x_labels = NULL, x_axis_labels = x_labels,
    y_breaks = NULL, y_axis_breaks = y_breaks,
    y_labels = NULL, y_axis_labels = y_labels,
    y_max = NULL, y_min = NULL,
    expand_y = 0.1,
    trim = TRUE,
    lines = c(0.25, 0.5, 0.75),
    linetype = "solid", linetype_quantile = "dotted",
    aspect_ratio = 1,
    censor_fn = rescale_none,
    show_points = FALSE,
    point_method = c("jitter", "beeswarm"), beeswarm_method = "smiley",
    point_size = 1, point_colors = "black", point_shapes = 19,
    point_width = width*0.15, dodge = width*1.1,
    n_bins = 30, band_width = n_bins/10, scaling = "width",
    ...) {
  plot_fn <- "plot_violin"

  # Recode df
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_violin")
  x <- vars$x
  y <- vars$y
  df$x <- df[[x]]
  df$y <- df[[y]]
  grouping_var <- get_input(grouping_var)

  # Determine order of x levels
  x_levels <- if (is.null(x_order)) {
    create_levels(df$x)
  } else if (length(x_order) == 1) {
    if (x_order == "ascending") {
      attr(fct_reorder(df$x, df$y, fn_x_order, .increasing = TRUE), "levels")
    } else if (x_order == "descending") {
      attr(fct_reorder(df$x, df$y, fn_x_order, .increasing = FALSE), "levels")
    }
  } else {
    x_order
  }

  if (rev_x_order) {
    x_levels <- Rev(x_levels)
  }

  # Convert x to a factor
  df$x <- factor(df$x, levels = x_levels)

  # Colors
  n_x_values <- length(attr(df$x, "levels"))
  n_colors <- n_x_values - length(colors)
  if (n_colors > 0) {
    colors <- c(colors, rep("white", n_colors))
  }

  # Core plot
  if (is.null(grouping_var)) {
    p <- ggplot(df, aes(x = x, y = y, fill = x))
  } else {
    df$grouping_var <- df[[grouping_var]]
    p <- ggplot(df, aes(x = x, y = y, fill = grouping_var))
  }
  p <- p + scale_fill_manual(values = colors)

  n_lines <- length(lines)
  if (n_lines == 0L || linetype == linetype_quantile) {
    p <- p + geom_violin(color = border_color, size = border_thickness, width = width, alpha = alpha, trim = trim, scale = scaling, draw_quantiles = lines, linetype = linetype, show.legend = show_legend)
  } else {
    if (is_odd(n_lines)) {
      mid_line <- Median(lines)
      other_lines <- lines[lines != mid_line]
    } else {
      other_lines <- lines
      mid_line <- NULL
    }
    clr_fn <- function(x) {
      z <- t.default(grDevices::col2rgb(x))/255
      z[] <- ifelse(z <= 0.03928, z/12.92, ((z + 0.055)/1.055)^2.4)
      z <- as.numeric(z %*% c(0.2126, 0.7152, 0.0722))
      ifelse(z > 0.179, "black", "white")
    }
    border_colors <- clr_fn(colors)
    mapping <- if (is.null(grouping_var)) aes(color = x) else aes(color = grouping_var)
    p <- p +
      geom_violin(mapping = mapping, size = border_thickness*0.9, width = width, alpha = alpha, trim = trim, scale = scaling, draw_quantiles = mid_line, linetype = linetype, position = position_dodge(width = dodge), show.legend = show_legend) +
      geom_violin(mapping = mapping, size = border_thickness*0.8, width = width, alpha = 0, trim = trim, scale = scaling, draw_quantiles = other_lines, linetype = linetype_quantile, show.legend = FALSE, position = position_dodge(width = dodge)) +
      scale_color_manual(values = border_colors) +
      geom_violin(fill = NA, color = border_color, size = border_thickness, width = width, alpha = 0, trim = trim, scale = scaling, draw_quantiles = NULL, show.legend = FALSE)
  }

  # Points
  if (show_points) {
    point_method <- match.arg(point_method, choices = c("jitter", "beeswarm"))
    if (point_method == "beeswarm") {
      pkg_required("ggbeeswarm")
      p <- p + ggbeeswarm::geom_quasirandom(color = point_colors, method = beeswarm_method, width = point_width, nbins = n_bins, bandwidth = band_width, size = point_size, shape = point_shapes, show.legend = FALSE, dodge.width = dodge)
    } else {
      p <- p + geom_jitter(size = point_size, color = point_colors, shape = point_shapes, show.legend = FALSE, position = position_jitterdodge(jitter.width = point_width, dodge.width = dodge, jitter.height = 0))
    }
  }

  # x axis
  x_breaks <- x_axis_breaks %||% x_levels
  x_labels <- x_axis_labels %||% x_breaks
  p <- p + scale_x_discrete(name = x_axis_title, breaks = x_breaks, labels = x_labels)

  # y axis
  y_max <- y_max %||% max(get_plot_data_limits(p)$y, na.rm = TRUE)
  y_min <- y_min %||% min(get_plot_data_limits(p)$y, na.rm = TRUE)
  p +
    scale_continuous(axis = "y", limits = c(y_min, y_max), scale = y_scale, n_breaks = n_breaks, title = y_axis_title, breaks = y_axis_breaks, labels = y_axis_labels, expand_lower = expand_y, censor_fn = censor_fn) +
    coord_cartesian(clip = "off") +
    ggtitle(plot_title) +
    theme_custom(aspect_ratio = aspect_ratio, ...)
}
