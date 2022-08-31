#' Forest plot
#'
#' @param df Data frame
#' @param risk,risk_lower,risk_upper Columns in df containing risk ratio, lower, and upper bound of confidence interval, respectively
#' @param label_var Variable used to create row labels in plot. Enter as quoted or unquoted variable names
#' @param ordering_var Variable used to reorder rows in forest plot. Enter as quoted or unquoted variable names. Only relevant when reorder is `TRUE`
#' @param reorder If `TRUE` (default), rows in forest plot will be reordered using
#' @param x_axis_title Title for x axis
#' @param hr_label HR label
#' @param color_var Variable used to determine color or HR/CI bars. Enter as quoted or unquoted variable name
#' @param base_size Base size for plot font (passed to `theme_custom()`). Default is `12`
#' @param label_size Font size for labels
#' @param hjust Horizontal adjustment
#' @param ratio Aspect ratio of plot
#' @param add_n_bands Number of extra grey/white bands to add above
#' @param n_bands Total number of grey/white bands
#' @param x_min,x_max Lower and upper limit for x axis values, respectively
#' @param colors Colors for HR/CI bars
#' @param point_shape Shapes used to display HR. Options: `circle` (default), `diamond`, `square`, `triangle`
#' @param point_size Size of HR points
#' @param errorbar_width Width of error bars
#' @param line_thickness Line thickness for HR/CI bars
#' @param point_border_thickness Border thickness for HR points
#' @param vert_line_thickness Thickness of reference vertical line
#' @param vert_linetype Linetype for reference vertical line
#' @param x_label_fn Function used to generate x axis labels
#' @param scale Scale for HR
#' @param log_scale Type of log scale
#' @param n_rows Number of rows
#' @param scales Scales
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_forest <- function(
  df,
  risk, risk_lower, risk_upper,
  label_var = "variable",
  ordering_var = "HR",
  reorder = FALSE,
  x_axis_title = NULL,
  hr_label = "regular",
  color_var = NULL,
  base_size = 12,
  label_size = 8,
  hjust = 1,
  ratio = 2.5,
  add_n_bands = NULL,
  n_bands = NULL,
  x_min = NULL,
  x_max = NULL,
  colors = c("#0072B5", "#BC3C29", "black"),
  point_shape = "square",
  point_size = 1.5,
  errorbar_width = 0.15,
  line_thickness = 0.9,
  point_border_thickness = 0.7,
  vert_line_thickness = 0.5,
  vert_linetype = "dashed",
  x_label_fn = NULL,
  scale = "log",
  log_scale = "log10",
  n_rows = 1,
  scales = "fixed",
  ...) {
  shape <- switch(point_shape, square = 22, circle = 21, diamond = 23, triangle = 24)
  color_var <- get_input(color_var)
  label_var <- get_input(label_var)
  ordering_var <- get_input(ordering_var)
  if (is.null(color_var)) {
    df$color_var <- "a"
    color_var <- "color_var"
  }
  total_bands <- n_unique(df[[label_var]])
  log_fn <- match_fun(log_scale)
  if (scale != "log") {
    x_min <- x_min %||% floor(Min(df[[risk_lower]]))
    x_max <- x_max %||% ceiling(Max(df[[risk_upper]]))
    x_breaks <- pretty(c(x_min, x_max))
  } else if (log_scale == "log10") {
    x_min <- x_min %||% (10^(floor(log_fn(Min(df[[risk_lower]])))))
    x_max <- x_max %||% (10^(ceiling(log_fn(Max(df[[risk_upper]])))))
    x_breaks <- (scales::trans_breaks(log_scale, function(x) 10^x))(c(x_min, x_max))
    x_breaks <- x_breaks[log_fn(x_breaks) %% 1 == 0]
    x_label_fn <- x_label_fn %||% match_fun(log_scale)
  } else {
    x_min <- x_min %||% (2^(floor(log_fn(Min(df[[risk_lower]])))))
    x_max <- x_max %||% (2^(ceiling(log_fn(Max(df[[risk_upper]])))))
    x_breaks <- (scales::trans_breaks(log_scale, function(x) 2^x))(c(x_min, x_max))
    x_breaks <- x_breaks[log_fn(x_breaks) %% 1 == 0]
    x_label_fn <- x_label_fn %||% match_fun(log_scale)
  }
  if (is.null(n_bands) && !is.null(add_n_bands)) {
    total_bands <- total_bands + add_n_bands
  }
  if (is.null(add_n_bands) && !is.null(n_bands)) {
    if (n_bands > total_bands) {
      total_bands <- n_bands
    }
  }
  p <- if (reorder) {
    df$label_var_reordered <- fct_reorder(df[[label_var]], df[[ordering_var]])
    ggplot(df, aes(label_var_reordered, .data[[risk]]))
  } else {
    ggplot(df, aes(.data[[label_var]], .data[[risk]]))
  }
  y_scaling <- if (scale != "log") {
    scale_y_continuous(x_axis_title, limits = c(x_min, x_max), expand = c(0, 0, 0, 0), breaks = x_breaks)
  } else {
    scale_y_continuous(x_axis_title, trans = scales::log_trans(base = as.numeric(gsub("[^0-9.-]+", "", log_scale))), limits = c(x_min, x_max), expand = c(0, 0, 0, 0), breaks = x_breaks, labels = x_label_fn)
  }
  p +
    annotate("rect", fill = "grey90", xmin = seq(0.5, total_bands - 0.5, by = 2), xmax = seq(1.5, (total_bands + 0.5), by = 2), ymin = x_min, ymax = Inf) +
    geom_hline(yintercept = 1, linetype = vert_linetype, color = "grey20", size = vert_line_thickness) +
    geom_errorbar(aes(ymin = .data[[risk_lower]], ymax = .data[[risk_upper]], color = .data[[color_var]]), width = errorbar_width, size = line_thickness, show.legend = FALSE) +
    geom_point(aes(color = .data[[color_var]], fill = .data[[color_var]]), shape = shape, size = point_size, show.legend = FALSE, stroke = point_border_thickness) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    y_scaling +
    scale_x_discrete(NULL) +
    theme_custom(base_size = base_size, ...) +
    coord_flip() +
    theme(axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_text(margin = margin(0, -2.5, 0, 0, unit = "pt"), size = label_size, hjust = hjust), axis.line.x = element_line(size = line_thickness), axis.ticks.x = element_line(size = line_thickness), axis.ticks.length.x = unit(3.5, units = "pt"), plot.margin = margin(r = 5, unit = "mm"), aspect.ratio = ratio)
}
