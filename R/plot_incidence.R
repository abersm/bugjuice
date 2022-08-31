#' Frequency plot showing incidence over time
#'
#' @param df Data frame or vector of dates
#' @param col Column containing dates if df is a data frame. Enter as quoted or unquoted variable name
#' @param colors Color of bars. Default is `"#328EC3"`
#' @param bar_colors Alias for colors
#' @param month_format Options: `"number"` (default), `"name"`, `"abbreviation"`
#' @param sep Separator between month, day, and year
#' @param leading_zero If `FALSE` (default), leading 0s are removed from month and day if < 10
#' @param full_year If `FALSE` (default), last 2 digits are used. If `TRUE`, year is displayed using 4 digits
#' @param n_bins Number of bins for x axis. Default is 20
#' @param x_axis_title Title for x axis
#' @param expand_x Expansion multiplier for x axis. Default is `0.1` if n_bins > 10 and `0.15` if n_bins <= 10. Only relevant if x is treated as numeric
#' @param x_angle Angle for x axis labels. Default is `45`
#' @param y_axis_title Title for y axis. Default is "n"
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_incidence <- function(df, col = NULL, colors = "#328EC3", bar_colors = colors, month_format = c("number", "name", "abbreviation"), leading_zero = FALSE, full_year = FALSE, sep = "-", n_bins = 20, x_axis_title = NULL, expand_x = if (n_bins > 10) 0.1 else 0.15, x_angle = 45, y_axis_title = "n", ...) {
  month_format <- match.arg(month_format, choices = c("number", "name", "abbreviation"))
  col <- get_input(col)
  if (inherits(df, "data.frame")) {
    df <- df[[col]]
  }
  month_format <- switch(month_format, number = "%m", name = "%B", "%b")
  year_format <- if (full_year) "%Y" else "%y"
  date_format <- paste(month_format, if (!leading_zero && month_format != "%m") "%e" else "%d", year_format, sep = sep)
  df <- df[!is.na(df)]
  p <- ggplot(vec_to_df(x = df), aes(x = x)) + geom_histogram(fill = bar_colors, alpha = 0.8, color = "black", bins = n_bins) + scale_x_date(x_axis_title, date_labels = date_format, expand = expansion(mult = c(expand_x, expand_x)), oob = rescale_none) + theme_custom(x_axis_label_angle = x_angle, ...) + theme(axis.title.y = element_text(angle = 0, margin = margin(r = 15)))
  y <- ggplot_build(p)$data[[1]]$count
  y_axis_breaks <- pretty(c(0, y))
  y_axis_breaks <- y_axis_breaks[y_axis_breaks %% 1 == 0]
  y_axis_limits <- range(y_axis_breaks)
  p <- p + scale_y_continuous(y_axis_title, limits = y_axis_limits, breaks = y_axis_breaks, expand = c(0, 0, 0, 0))
  p <- p + annotate("text", label = paste0("N = ", length(df)), x = max(df), y = y_axis_limits[2], hjust = 1, vjust = 1)
  suppressWarnings(suppressMessages(print(p)))
}
