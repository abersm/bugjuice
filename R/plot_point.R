#' Point plot
#'
#' @param df Data frame in long format containing continuous variable, categorical or integer variable, and grouping variable
#' @param formula y ~ x format
#' @param grouping_var Variable to group by. Enter as quoted or unquoted variable name
#' @param ... Arguments passed to `theme_fn()`
#' @param x,y Variables for x  and y axis, respectively. Enter as quoted or unquoted variable names
#' @param x_order Order of x values. Enter as character vector. Default is `NULL`
#' @param rev_x_order If `FALSE` (default), order of x axis variable is not reversed
#' @param grouping_var_order Order for levels of `grouping_var` along x axis. Enter as character vector
#' @param rev_grouping_var_order If `TRUE`, levels of `grouping_var` are reversed
#' @param beeswarm If `FALSE` (default), jittered points. If `TRUE`, beeswarm plot
#' @param beeswarm_method Method for `geom_quasirandom` function. Default is `"smiley"`
#' @param width Alias for either `jitter_width` or `beeswarm_width`
#' @param beeswarm_width Width argument for beeswarm points
#' @param n_bins Number of bins. Used in `ggbeeswarm::geom_quasirandom` function. Default uses 30/number of points
#' @param band_width Bandwidth. Used in `ggbeeswarm::geom_quasirandom` function. Default is `1 + n_bins/5`
#' @param dodge Entry for position_dodge. Default is `0.7`
#' @param jitter_width Width between groups of points. Default is `0.2`
#' @param point_shapes Point shapes. Options: `"circle"` (default), `"square"`, `"triangle"`, `"diamond"`. Enter as quoted shape
#' @param point_shape_var Variable used to determine point shapes. Default uses `grouping_var.` Enter as quoted or unquoted variable name
#' @param point_color_var Variable used to determine point colors. Enter as quoted or unquoted variable name. Default uses `grouping_var`
#' @param colors Alias for `point_colors`
#' @param point_colors Color of points. Enter as character vector of color names or hexadecimal codes
#' @param alpha Alias for `point_alpha`
#' @param point_alpha Transparency of point colors. Default is `0.9`
#' @param point_size Point size. Default is `5`
#' @param point_border_thickness Thickness of line surrounding points in pts. Default is `1`
#' @param point_border_color Color of line surrounding points. Default is `"black"`
#' @param show_errorbar If `TRUE`, error bars are displayed
#' @param summary_fn Function used to determine midpoint of error bars. Default is `Mean`
#' @param error_fn Function used to determine ends of error bars. Default is `SE`
#' @param errorbar_width Width of error bars. Default is `n_unique(df$x)/10`
#' @param errorbar_thickness Line thickness of error bar. Default is `0.75`
#' @param show_legend If `FALSE` (default), legend is not displayed
#' @param legend_title Legend title. Default is blank
#' @param y_axis_title Enter as quoted string
#' @param y_title Alias for `y_axis_title`
#' @param y_scale Scaling for y axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param y_axis_breaks Numeric vector or function specifying location of ticks along y axis
#' @param y_axis_labels Vector or function specifying y axis tick labels
#' @param breaks_fn Function used to generate y axis breaks. Passed to `.create_axis_breaks`. Default is `pretty`
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param y_min Minimum value to use for y axis. Default is `0`
#' @param y_max Maximum y axis value. Default is `NULL`
#' @param expand_y Expansion of y axis around y = 0. Default is `0`
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param x_title Alias for `x_axis_title`
#' @param x_axis_title Enter as quoted string
#' @param x_axis_breaks Numeric vector or function specifying location of ticks along x axis
#' @param x_axis_labels Vector or function specifying x axis tick labels
#' @param expand_x Argument entered for expand argument of `scale_x_continuous`. Default is `waiver()`
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""` (i.e. no title)
#' @param show_sig If `TRUE` (default), significance annotation displayed. If `FALSE`, no significance annotation displayed
#' @param sig_method Options: `"p_by_normality"` (default), `"mann_whitney"`, `"t_test_welch"`, `"t_test_no_welch"`. Enter as quoted test type
#' @param stars If TRUE (default), significance stars are used instead of actual P values
#' @param show_ns If `FALSE` (default), no annotation for P >= 0.05. If `TRUE`, non-significant comparisons displayed
#' @param ns_symbol Symbol to use when P >= 0.05. Only used if show_ns is `TRUE.` default is `"ns"`
#' @param sig_bar_nudge Nudge factor along y axis for significance bars. Distance between point or bar and significance bar is sig_bar_nudge x maximum y value on plot. Default is `0.06`
#' @param sig_star_nudge Nudge factor along y axis for significance stars. Distance between point or bar and significance stars is sig_star_nudge*maximum y value on plot. Default is `0.05`
#' @param sig_text_nudge Nudge factor along y axis for significance annotation text. Distance between point or bar and significance annotation text is sig_text_nudge x maximum y value on plot. Default is `0.09`
#' @param step_increase Step increase as proportion of view port. Default is `0.11`
#' @param sig_bar_thickness Line thickness for significance bars in pts. Default is `0.75`
#' @param sig_bar_color Color for significance bars and annotations. Default is `"black"`
#' @param sig_font_color Color for text in significance annotations. Default is `"black"`
#' @param sig_star_size Size of significance stars in pts. Only relevant when `stars = TRUE`
#' @param sig_text_size Font size for P values in pts. Only used when `stars = FALSE`
#' @param p_case Case of p used in significance annotations when `stats = FALSE.` Options: `"upper"` (default) or `"lower"`. Enter as quoted text
#' @param p_spaces If `TRUE` (default), spaces placed between p, equality symbol, and p-value in significance annotation. If `FALSE`, no spaces placed between p, equality symbol, and p-value in significance annotation. Only relevant when `stars = FALSE`
#' @param seed Length 1 integer vector to set seed for reproducibility when using jittering. Default is `1234`
#' @param theme_fn Theme function. Default is `theme_custom`
#' @importFrom dplyr select rename filter group_by ungroup mutate left_join summarize n_distinct
#' @importFrom scales pretty_breaks trans_breaks trans_format math_format
#' @import ggplot2
#' @export
plot_point <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  ...,
  x = NULL, y = NULL,
  beeswarm = FALSE, beeswarm_method = "smiley",
  width = 0.2,
  beeswarm_width = width, n_bins = 25, band_width = 1 + n_bins/5,
  dodge = if (beeswarm) 0.8 else 0.7,
  jitter_width = width,
  x_order = NULL,
  rev_x_order = FALSE,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  point_shapes = c("circle", "circle"),
  point_shape_var = NULL,
  point_color_var = NULL,
  colors = c("#0072B5", "#BC3C29", "#8C8C8C", "#2A2D34", "#009872", "#6761A8"),
  point_colors = colors,
  alpha = 0.9,
  point_alpha = alpha,
  point_size = 5,
  point_border_thickness = 1,
  point_border_color = "black",
  show_errorbar = FALSE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width = NULL,
  errorbar_thickness = 0.75,
  show_legend = FALSE,
  legend_title = "",
  y_title = waiver(), y_axis_title = y_title,
  y_scale = "regular",
  y_axis_breaks = NULL, y_axis_labels = NULL, breaks_fn = pretty, n_breaks = 4,
  y_min = NULL, y_max = NULL,
  expand_y = 0.1,
  censor_fn = rescale_none,
  x_title = waiver(), x_axis_title = x_title,
  x_axis_breaks = NULL, x_axis_labels = NULL,
  expand_x = waiver(),
  plot_title = NULL,
  show_sig = TRUE, sig_method = "p_by_normality",
  stars = TRUE, show_ns = TRUE, ns_symbol = "ns",
  sig_bar_nudge = if (y_scale == "log") 0.9 else 0.11,
  sig_star_nudge = if (y_scale == "log") -0.45 else sig_bar_nudge - 0.03,
  sig_text_nudge = if (y_scale == "log") 0.5 else sig_bar_nudge + 0.03,
  step_increase = if (y_scale == "log") 0 else sig_bar_nudge + 0.05,
  sig_bar_thickness = 0.75,
  sig_bar_color = "black",
  sig_font_color = "black",
  sig_star_size = 22,
  sig_text_size = 12,
  p_case = "upper",
  p_spaces = TRUE,
  seed = 1234,
  theme_fn = theme_custom) {
  # Plotting function
  plot_fn <- "plot_point"

  # Set seed
  old <- .Random.seed
  on.exit({
    .Random.seed <<- old
  })
  set.seed(seed)

  # Dots
  if (...length() > 0L) {
    dots <- eval(substitute(alist(...)), envir = parent.frame())
    if (is.null(names(dots))) {
      subset_criteria <- unlist(lapply(dots, function(z) eval(z, envir = df)), use.names = FALSE)
      df <- df[subset_criteria, ]
      theme_dots <- FALSE
    } else {
      theme_dots <- TRUE
    }
  } else {
    theme_dots <- FALSE
  }

  # Data
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, point_color_var = {{point_color_var}}, point_shape_var = {{point_shape_var}}, dplyr::everything())
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_point")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c("grouping_var", "point_color_var", "point_shape_var"))
  x_levels <- x_order %||% create_levels(df$x, reverse = rev_x_order)
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)

  # grouping_var
  df_names <- names(df)
  if (!is_column(df, "grouping_var")) {
    df$grouping_var <- "a"
  } else if (!is.factor(df$grouping_var)) {
    df$grouping_var <- as.character(df$grouping_var)
  }
  grouping_var_levels <- grouping_var_order %||% create_levels(df$grouping_var, reverse = rev_grouping_var_order)
  df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)

  # point_color_var
  if (!is_column(df, "point_color_var")) {
    if ("grouping_var" %in% df_names) {
      df$point_color_var <- df$grouping_var
    } else if (length(point_colors) == 1L) {
      df$point_color_var <- "a"
    } else {
      df$point_color_var <- df$x_numeric
    }
  }

  # point_shape_var
  if (!is_column(df, "point_shape_var")) {
    df$point_shape_var <- factor("a")
  } else if (!is.factor(df$point_shape_var)) {
    df$point_shape_var <- factor(df$point_shape_var)
  }
  if (!is.factor(df$point_color_var)) {
    df$point_color_var <- factor(df$point_color_var)
  }
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # Axis titles
  if (is_waiver(x_axis_title)) {
    x_axis_title <- gsub(pattern = "_", replacement = " ", x =  str_capitalize(x), fixed = TRUE)
  }
  if (is_waiver(y_axis_title)) {
    y_axis_title <- gsub(pattern = "_", replacement = " ", x =  str_capitalize(y), fixed = TRUE)
  }

  # y axis
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)
  # Change y values if log scale is used
   if (startsWith(y_scale, "log")) {
     y_values <- df$y
     minimum_y_value <- min(y_values)
     if (minimum_y_value < 1) {
       if (minimum_y_value < 0) {
         warning("Some y values are < 0 (min = ", round_up(minimum_y_value, 2), ")", call. = FALSE)
         message("Will transform y values by adding ", round_up(minimum_y_value, 2), " to each value")
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

  # Points
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shapes]
  names(point_shapes) <- NULL
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1) {
    point_args <- list(mapping = aes(fill = point_color_var), shape = unique_shapes, color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = aes(fill = point_color_var, shape = point_shape_var), color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- scale_shape_manual(name = NULL, values = point_shapes)
  }

  # Points
  p <- if (beeswarm) {
    pkg_required("ggbeeswarm")
    n_bins <- n_bins %||% (30/Nrow(df))
    do.call(ggbeeswarm::geom_quasirandom, c(point_args, method = beeswarm_method, width = beeswarm_width, dodge.width = dodge, nbins = n_bins, bandwidth = band_width))
  } else {
    do.call(geom_point, c(point_args, position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge, jitter.height = 0)))
  }
  p <- ggplot(df, aes(x = x_numeric, y = y, group = grouping_var)) +
    p +
    scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape

  # Error bars
  if (show_errorbar) {
    p <- p + errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = errorbar_thickness, width = errorbar_width, dodge = dodge)
  }

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title/theme
  p <- p + coord_cartesian(clip = "off", default = TRUE) + ggtitle(plot_title)
  p <- if (theme_dots) p + theme_fn(...) else p + theme_fn()

  # Significance annotation
  n_groups <- n_unique(df$grouping_var, na.rm = FALSE)
  if (missing(show_sig)) {
    show_sig <- !(n_groups > 4L || n_unique(df$x_numeric, na.rm = FALSE) > 5L)
  }
  if (show_sig) {
    if (n_groups > 1) {
      fn_sig_anno <- .plot_sig_anno_grouped
      if (startsWith(y_scale, "log")) {
        if (missing(sig_bar_nudge)) {
          y_breaks <- .create_axis_breaks(
            .limits = c(if (y_limits[1] == 0) 1 else y_limits[1], y_limits[2]),
            .scale = y_scale,
            .n = n_breaks,
            .breaks_fn = breaks_fn
          )
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
    p <- p + fn_sig_anno(
      df = df,
      method = sig_method,
      dodge = dodge,
      y_max_type = "raw",
      y_scale = y_scale,
      bar_nudge = sig_bar_nudge,
      star_nudge = sig_star_nudge,
      text_nudge = sig_text_nudge,
      star_size = sig_star_size,
      text_size = sig_text_size,
      p_case = p_case,
      p_spaces = p_spaces,
      bar_thickness = sig_bar_thickness,
      bar_color = sig_bar_color,
      font_color = sig_font_color,
      show_ns = show_ns,
      stars = stars,
      ns_symbol = ns_symbol,
      step_increase = step_increase,
      breaks_fn = breaks_fn
    )
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    p + scale_axis_clean(plot_limits = y_plot_limits, scale = y_scale, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis())
  } else {
    p + scale_continuous(limits = y_limits, axis = "y", scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  }
}

#' Alias for plot_point
#'
#' @rdname plot_point
#' @export
pp <- plot_point
