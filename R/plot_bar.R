# Continuous --------------------------------------------------------------

#' Bar plot
#'
#' @inheritParams plot_point
#' @param bar_alpha Alpha value for bar fill. Default is `0`
#' @param bar_border_thickness Thickness of line surrounding bars in pts. Default is `0.75`
#' @param width,bar_width Width of bars. Default is 0.2*number of x values
#' @param bar_color_var Variable used to determine bar color. Enter as quoted or unquoted variable name
#' @param bar_border_colors Bar border color. Default is `"black"`
#' @param errorbar_width_multiplier Multiplied by `bar_width` to determine width of error bars. Default is `0.5`
#' @param bar_colors Bar color. Enter as character vector of color names or hexadecimal codes
#' @param show_all If `TRUE` (default), all significance bars are displayed
#' @export
plot_bar <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  x = NULL, y = NULL,
  bar_color_var = NULL,
  colors = c("#2A2D34", "#BC3C29", "#00A1D5", "#009872", "#6761A8"),
  bar_colors = colors,
  bar_alpha = 1,
  bar_border_thickness = 0.75,
  bar_border_colors = "black",
  width = NULL,
  bar_width = width,
  dodge = 0.7,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width_multiplier = 0.5,
  show_legend = FALSE,
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
  sig_bar_nudge = 0.06,
  sig_star_nudge = 0.05,
  sig_text_nudge = 0.09,
  step_increase = if (y_scale == "regular") sig_bar_nudge + 0.05 else 0,
  sig_method = "p_by_normality",
  p_case = "upper",
  p_spaces = TRUE,
  n_breaks = 3,
  breaks_fn = pretty,
  y_max = NULL,
  expand_x = waiver(),
  censor_fn = rescale_none,
  show_all = TRUE,
  ...) {
  # Plotting function
  plot_fn <- "plot_bar"

  # Data setup
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, bar_color_var = {{bar_color_var}}, dplyr::everything())
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_bar")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c("grouping_var", "bar_color_var"))
  x_levels <- x_order %||% create_levels(df$x)
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)
  df_names <- names(df)

  # Setup for mapped vs. set variables
  set_args <- list(geom = "bar", fun = Mean, size = bar_border_thickness, position = position_dodge(width = dodge), color = bar_border_colors, show.legend = show_legend, alpha = bar_alpha)
  mapped_args <- alist(x = x_numeric, y = y, fill = bar_color_var, group = grouping_var)

  # bar_color_var, grouping_var
  if (is_column(df, "grouping_var")) {
    set_args$width <- bar_width <- bar_width %||% (0.64-0.03*n_unique(df$x, na.rm = FALSE))
    df$grouping_var <- as.character(df$grouping_var)
    grouping_var_levels <- create_levels(df$grouping_var, reverse = rev_grouping_var_order)
    df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)
    if (!is_column(df, "bar_color_var")) {
      df$bar_color_var <- df$grouping_var
    }
  } else {
    df$grouping_var <- "a"
    set_args$width <- bar_width <- bar_width %||% 0.4
    if (!is_column(df, "bar_color_var")) {
      df$bar_color_var <- df$x
    }
  }
  bar_colors <- rep(bar_colors, length.out = n_unique(df$bar_color_var, na.rm = FALSE))
  if (is.numeric(df$bar_color_var)) {
    df$bar_color_var <- factor(df$bar_color_var, levels = df$bar_color_var)
  }

  # Build plot
  set_args$mapping <- mapping <- do.call("aes", mapped_args)
  p <- ggplot(df, mapping)

  # Bars
  bar <- do.call("stat_summary", set_args)
  p <- p + bar + scale_fill_manual(name = legend_title, values = bar_colors)

  # Error bars
  if (show_errorbar) {
    p <- p + errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = bar_border_thickness, width = errorbar_width_multiplier*bar_width, dodge = dodge, error_limits = "upper")
  }

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- x_axis_labels %||% x_levels
  if (!missing(x_axis_labels) && is.null(x_breaks)) {
    x_labels <- NULL
  }
  x_axis_title <- x_axis_title %W% gsub(pattern = "_", replacement = " ", x = str_capitalize(x))
  p <- p + scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Significance annotation
  if (show_sig) {
    y_max_type <- if (show_errorbar) "error" else "summary"
    if (startsWith(y_scale, "log")) {
      sig_bar_nudge <- 1
      sig_star_nudge <- 0.75
      sig_text_nudge <- 1.75
    }

    fn_sig_anno <- if (n_unique(df$grouping_var, na.rm = FALSE) > 1) .plot_sig_anno_grouped else .plot_sig_anno
    p <- p + fn_sig_anno(df = df, method = sig_method, dodge = dodge, y_max_type = y_max_type, summary_fn = summary_fn, error_fn = error_fn, bar_nudge = sig_bar_nudge, star_nudge = sig_star_nudge, text_nudge = sig_text_nudge, star_size = sig_star_size, text_size = sig_text_size, p_case = p_case, p_spaces = p_spaces, bar_thickness = sig_bar_thickness, bar_color = sig_bar_color, font_color = sig_font_color, show_ns = show_ns, stars = stars, ns_symbol = ns_symbol, step_increase = step_increase, breaks_fn = breaks_fn)
  }

  # y axis
  y_axis_title <- y_axis_title %W% gsub(pattern = "_", replacement = " ", x = str_capitalize(y))
  y_max <- if (!is.null(y_max)) {
    y_max
  } else if (!is.null(y_axis_breaks)) {
    Max(y_axis_breaks)
  } else {
    df_by_x <- dplyr::group_by(df, x, grouping_var)
    fn <- if (show_errorbar) {
      function(x) summary_fn(x) + error_fn(x)
    } else {
      summary_fn
    }
    df_by_x <- dplyr::summarize(df_by_x, y = fn(y), .groups = "drop")
    Max(df_by_x$y)
  }

  p <- if (show_all) {
    y_axis_breaks <- y_axis_breaks %||% .create_axis_breaks(.limits = c(0, y_max), .scale = y_scale, .breaks_fn = breaks_fn, .n = n_breaks)
    plot_limits <- get_plot_data_limits(p, axis = "y")
    p + scale_axis_clean(axis = "y", plot_limits = plot_limits, axis_limits = c(0, y_max), scale = y_scale, n_breaks = n_breaks, labels = y_axis_labels, title = y_axis_title, censor_fn = censor_fn, breaks_fn = breaks_fn, guide = guide_clean_axis())
  } else {
    p + scale_continuous(axis = "y", limits = c(0, y_max), scale = y_scale, n_breaks = n_breaks, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, censor_fn = censor_fn)
  }

  # Plot title and theme
  p + coord_cartesian(clip = "off", default = TRUE) + ggtitle(plot_title) + theme_custom(...)
}

#' Alias for `plot_bar()`
#'
#' @rdname plot_bar
#' @export
pb <- plot_bar
