#' Bar plot with overlying points
#'
#' @inheritParams plot_bar
#' @inheritParams plot_point
#' @param color_var Variable used to color points. Default uses `grouping_var.` Enter as quoted or unquoted variable name
#' @param bar_border_color Color for bar border. Default is `"black"`
#' @param show_legend_points If `FALSE` (default) legend for points is not displayed
#' @param show_legend_bars If `FALSE` (default) legend for bars is not displayed
#' @export
plot_bar_point <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  x = NULL, y = NULL,
  beeswarm = FALSE, beeswarm_method = "smiley",
  width = 0.2,
  beeswarm_width = width, n_bins = NULL, band_width = 1 + n_bins/5,
  dodge = NULL,
  jitter_width = width,
  colors = c("#0072B5", "#BC3C29", "#8C8C8C", "#2A2D34", "#009872", "#6761A8"),
  color_var = NULL,
  point_shapes = c("circle", "circle"),
  point_size = 5,
  point_alpha = 0.9,
  bar_alpha = 0,
  bar_border_thickness = 0.75,
  point_border_thickness = 1,
  bar_border_color = "black",
  point_border_color = "black",
  bar_width = NULL,
  point_shape_var = NULL,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width_multiplier = 0.5,
  show_legend_points = FALSE,
  show_legend_bars = FALSE,
  legend_title = "",
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_axis_breaks = NULL,
  x_axis_labels = NULL,
  y_title = waiver(), y_axis_title = y_title,
  x_title = waiver(), x_axis_title = x_title,
  plot_title = NULL,
  show_sig = TRUE,
  stars = TRUE,
  sig_font_color = "black",
  sig_bar_color = "black",
  sig_bar_thickness = 0.75,
  show_ns = TRUE,
  ns_symbol = "ns",
  sig_text_size = 12,
  sig_star_size = 22,
  sig_bar_nudge = 0.09,
  sig_star_nudge = 0.07,
  sig_text_nudge = 0.12,
  step_increase = if (y_scale == "regular") sig_bar_nudge + 0.05 else 0,
  sig_method = "p_by_normality",
  p_case = "upper",
  p_spaces = TRUE,
  n_breaks = 4,
  breaks_fn = pretty,
  y_max = NULL,
  y_min = 0,
  expand_y = 0,
  expand_x = waiver(),
  censor_fn = rescale_none,
  show_all = TRUE,
  seed = 1234,
  ...) {
  # Plotting function
  plot_fn <- "plot_bar_point"
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_bar_point")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y

  # Set seed
  old <- .Random.seed
  on.exit({
    .Random.seed <<- old
  })
  set.seed(seed)

  # Data setup
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, color_var = {{color_var}}, point_shape_var = {{point_shape_var}}, dplyr::everything())
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c("grouping_var", "color_var", "point_shape_var"))
  x_levels <- x_order %||% create_levels(df$x)
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)

  # grouping_var
  df_names <- names(df)
  if (!is_column(df, "grouping_var")) {
    df$grouping_var <- "a"
    bar_width <- bar_width %||% 0.4
  }
  df$grouping_var <- as.character(df$grouping_var)
  grouping_var_levels <- create_levels(df$grouping_var, reverse = rev_grouping_var_order)
  df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)

  # color_var, point_shape_var
  if (!is_column(df, "color_var")) {
    df$color_var <- factor(df$grouping_var)
  }
  if (!is_column(df, "point_shape_var")) {
    df$point_shape_var <- factor(1)
  }

  colors <- rep(colors, length.out = n_unique(df$color_var, na.rm = FALSE))
  if (!inherits(df$point_shape_var, "factor")) {
    df$point_shape_var <- factor(df$point_shape_var)
  }

  # Change y values if log scale is used
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)
  if (startsWith(y_scale, "log")) {
    df$y <- ifelse(df$y == 0, 1, df$y)
    if (Min(y_limits) <= 0) {
      y_limits[1] <- 1
    }
    sig_bar_nudge <- 1
    sig_star_nudge <- 0.75
    sig_text_nudge <- 1.75
  }

  # Bars
  dodge <- dodge %||% if (beeswarm) 0.8 else 0.7
  p <- ggplot(df, aes(x = x_numeric, y = y, group = grouping_var))
  bar_width <- bar_width %||% (0.64 - 0.03*n_unique(df$x, na.rm = FALSE))
  p <- p + stat_summary(aes(fill = color_var, group = grouping_var), geom = "bar", fun = summary_fn, alpha = bar_alpha, width = bar_width, size = bar_border_thickness, color = bar_border_color, position = position_dodge(width = dodge), show.legend = show_legend_bars) +
    scale_fill_manual(name = legend_title, values = colors)

  # Error bars
  if (show_errorbar) {
    p <- p + errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = bar_border_thickness, width = errorbar_width_multiplier*bar_width, dodge = dodge)
  }

  # Points
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shapes]
  names(point_shapes) <- NULL
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1) {
    point_args <- list(mapping = aes(fill = color_var), shape = unique_shapes, color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend_points)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = aes(fill = color_var, shape = point_shape_var), color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend_points)
    scale_shape <- scale_shape_manual(name = NULL, values = point_shapes)
  }
  point <- if (beeswarm) {
    pkg_required("ggbeeswarm")
    n_bins <- n_bins %||% (30/nrow(df))
    do.call(ggbeeswarm::geom_quasirandom, c(point_args, method = beeswarm_method, width = beeswarm_width, dodge.width = dodge, nbins = n_bins, bandwidth = band_width))
  } else {
    do.call(geom_point, c(point_args, position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge, jitter.height = 0)))
  }
  p <- p + point + scale_shape

  # Axis titles
  x_axis_title <- x_axis_title %W% gsub(pattern = "_", replacement = " ", x =  str_capitalize(x))
  y_axis_title <- y_axis_title %W% gsub(pattern = "_", replacement = " ", x =  str_capitalize(y))

  # y axis
  if (startsWith(y_scale, "log")) {
    y_values <- df$y
    minimum_y_value <- min(y_values)
    if (minimum_y_value < 1) {
      if (minimum_y_value < 0) {
        w <- round_up(minimum_y_value, 2)
        warning("Some y values are < 0 (min = ", w, ")", call. = FALSE)
        message("Will transform y values by adding ", w, " to each value")
        df$y <- y_values + minimum_y_value
      } else if (minimum_y_value == 0) {
        z <- y_values[y_values > 0 & y_values < 1]
        if (length(z) > 0) {
          y_limits[1] <- min(z)
        } else {
          df$y <- y_values + 1
          y_limits[1] <- min(df$y)
        }
      }
    }
  }

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title and theme
  p <- p + coord_cartesian(clip = "off", default = TRUE) + ggtitle(plot_title) + theme_custom(...)

  # Significance annotation
  if (show_sig) {
    if (n_unique(df$grouping_var, na.rm = FALSE) > 1) {
      fn_sig_anno <- .plot_sig_anno_grouped
      if (startsWith(y_scale, "log")) {
        if (missing(sig_bar_nudge)) {
          y_breaks <- .create_axis_breaks(.limits = c(if (y_limits[1] == 0) 1 else y_limits[1], y_limits[2]), .scale = y_scale, .n = n_breaks, .breaks_fn = breaks_fn)
          y_breaks <- if (y_scale == "log2") log2(y_breaks) else log10(y_breaks)
          sig_bar_nudge <- 0.25*(Max(y_breaks) - Min(y_breaks)) - 0.05*length(y_breaks) + 0.06*point_size - 0.18
        }
        if (missing(sig_text_nudge)) {
          sig_text_nudge <- 1.61*sig_bar_nudge - 0.09
        }
        if (missing(sig_star_nudge)) {
          sig_star_nudge <- 0.66*sig_bar_nudge + 0.05
        }
      }
    } else {
      fn_sig_anno <- .plot_sig_anno
    }
    p <- p + fn_sig_anno(df = df, method = sig_method, dodge = dodge, y_max_type = "raw", y_scale = y_scale, bar_nudge = sig_bar_nudge, star_nudge = sig_star_nudge, text_nudge = sig_text_nudge, star_size = sig_star_size, text_size = sig_text_size, p_case = p_case, p_spaces = p_spaces, bar_thickness = sig_bar_thickness, bar_color = sig_bar_color, font_color = sig_font_color, show_ns = show_ns, stars = stars, ns_symbol = ns_symbol, step_increase = step_increase, breaks_fn = breaks_fn)
    if (show_all) {
      y_plot_limits <- get_plot_data_limits(p, axis = "y")
      p <- p + scale_axis_clean(plot_limits = y_plot_limits, scale = y_scale, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis())
      return(p)
    }
  }

  # Plot without significance annotation
  p + scale_continuous(limits = y_limits, axis = "y", scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
}

#' Alias for `plot_bar_point()`
#'
#' @rdname plot_bar_point
#' @export
pbp <- plot_bar_point
