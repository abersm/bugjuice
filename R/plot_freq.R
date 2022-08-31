#' Frequency bar plot
#'
#' @param df Data frame containing column `col` or vector of values
#' @param col Categorical variable. Enter as quoted or unquoted variable name
#' @param order_by_freq Options: `"both"` (default, both x axis variable and `grouping_var` ordered by frequency), `"x"` (x axis variable is ordered by frequency), `"grouping_var"` (`"grouping_var"` is ordered by frequency), `"none"` (no variables are ordered by frequency). Default is to order groups by decreasing frequency. If `rev_x_order` or `rev_grouping_var_order` is `TRUE`, groups are ordered by increasing frequency. Only relevant when `x_order = NULL` and `grouping_var_order = NULL`
#' @param x_axis_label_angle Angle of x axis labels
#' @param y_axis_title_angle Angle for y axis title. Default is `0`
#' @param aspect_ratio Aspect ratio for plot
#' @param show_anno If `TRUE` (default), group size is displayed above each bar
#' @param anno_type Options: `"counts"` (default), `"percent"`
#' @param anno_font_size Font size for counts in pts
#' @param anno_font_color Font color for counts. Default is `"black"`
#' @param anno_y_nudge Amount to nudge counts above bar as percent of y axis range. Default is `0.03`
#' @param show_n If `TRUE`, total sample size is displayed on plot
#' @param n_anno_font_size Font size for sample size annotation in pts. Default is `12`
#' @param n_anno_font_color Font color for sample size annotation. Default is `"black"`
#' @param n_anno_x Position of sample size annotation along x axis. Options: `"right"` (default),`"left"`
#' @param n_anno_x_nudge Amount to nudge sample size annotation along x axis. Default is `0.2`
#' @param n_prefix Prefix for sample size annotation. Default is `"N = "`
#' @inheritParams plot_bar
#' @inheritParams plot_point
#' @export
plot_freq <- function(
  df,
  col = NULL,
  grouping_var = NULL,
  formula = NULL,
  order_by_freq = c("both", "x", "grouping_var", "none"),
  x_order = NULL,
  rev_x_order = FALSE,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  bar_color_var = NULL,
  colors = "#2A2D34",
  bar_border_thickness = 0.75,
  bar_width = NULL,
  width = bar_width,
  dodge = 0.7,
  show_legend = FALSE,
  legend_title = "",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_axis_breaks = waiver(),
  x_axis_labels = waiver(),
  x_axis_label_angle = NULL,
  y_axis_title_angle = 0,
  y_axis_title = "N",
  x_axis_title = NULL,
  plot_title = NULL,
  n_breaks = 3,
  breaks_fn = pretty,
  y_max = NULL,
  censor_fn = rescale_none,
  aspect_ratio = NULL,
  show_anno = TRUE, anno_type = c("counts", "percent"), anno_font_size = NULL, anno_font_color = "black", anno_y_nudge = 0.03,
  show_n = FALSE, n_anno_font_size = 12, n_anno_font_color = "black", n_anno_x = "right", n_anno_x_nudge = 0.2, n_prefix = "N = ",
  ...) {
  if (is.data.frame(df)) {
    df <- cbind(df, dplyr::select(df, x = {{col}}, grouping_var = {{grouping_var}}, bar_color_var = {{bar_color_var}}))
    if (!is.null(formula)) {
      x <- all.vars(formula)
      counts <- x[1L]
      x <- x[2L]
      df$x <- df[[x]]
      df$counts <- df[[counts]]
    } else {
      z <- Intersect(c("x", "grouping_var", "bar_color_var"), names(df))
      df <- dplyr::group_by(df, !!!dplyr::syms(z))
      df <- dplyr::summarize(df, counts = dplyr::n())
    }
  } else {
    counts <- n_per_group(df)
    df <- vec_to_df(x = names(counts), counts = as.vector(counts))
  }
  df_names <- names(df)
  order_by_freq <- match.arg(order_by_freq, choices = c("both", "x", "grouping_var", "none"))
  anno_type <- match.arg(anno_type, choices = c("counts", "percent"))

  # Order of x variable along x axis
  if (!is.factor(df$x)) {
    df$x <- as.character(df$x)
  }
  x_levels <- x_order %||% if (order_by_freq %in% c("none", "grouping_var")) {
    create_levels(df$x, reverse = rev_x_order)
  } else {
    df[order(df$counts, decreasing = !rev_x_order), ]$x
  }
  df$x <- factor(df$x, levels = x_levels)

  # Set width (must keep before ordering of grouping_var)
  if (grouping_var %!in% df_names) {
    df$grouping_var <- "a"
    width <- width %||% 0.5
  } else {
    width <- width %||% (0.2*n_unique(df$grouping_var, na.rm = FALSE))
  }

  # Order of grouping_var along x axis
  if (!is.factor(df$grouping_var)) {
    df$grouping_var <- as.character(df$grouping_var)
  }
  grouping_var_levels <- grouping_var_order %||% if (order_by_freq %in% c("none", "x")) {
    create_levels(df$grouping_var, reverse = rev_grouping_var_order)
  } else {
    grouping_var_levels <- names(n_per_group(df$grouping_var))
    if (rev_grouping_var_order) {
      Rev(grouping_var_levels)
    } else {
      grouping_var_levels
    }
  }
  df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)

  # Bar color
  if ("bar_color_var" %!in% df_names) {
    df$bar_color_var <- if (length(colors) == 1) "a" else df$grouping_var
  }

  # Clean variables for plotting
  df <- df[complete.cases(df[, c("x", "counts", "grouping_var", "bar_color_var")]), ]
  n_bars <- n_unique(df$x, na.rm = FALSE)*n_unique(df$grouping_var, na.rm = FALSE)
  aspect_ratio <- aspect_ratio %||% if (n_bars < 4) .phi else 1
  y_max <- y_max %||% Max(df$counts)
  y_axis_breaks <- y_axis_breaks %||% .create_axis_breaks(.limits = c(0, y_max), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)
  y_axis_limits <- c(Min(y_axis_breaks), Max(y_axis_breaks))
  y_axis_labels <- y_axis_labels %||% function(x) format(x, big.mark = ",", scientific = FALSE)
  if (is.null(x_axis_label_angle)) {
    x_axis_label_angle <- if (max(nchar(attr(df$x, "levels")), na.rm = TRUE) > 5) 45 else 0
  }

  # Count annotations
  if (show_anno) {
    df$y_pos <- df$counts + Diff(y_axis_limits)*anno_y_nudge
    n <- sum(df$counts)
    df$label <- if (anno_type == "counts") df$counts else paste0(round_up(df$counts/n*100, digits = 1), "%")
    anno_font_size <- convert(anno_font_size %||% if (n_bars < 5) 12 else 6, "pt", "mm")
    counts_annotation <- geom_text(aes(x = x, y = y_pos, label = label), size = anno_font_size, color = anno_font_color, inherit.aes = FALSE, vjust = 0, hjust = 0.5)
  } else {
    counts_annotation <- NULL
  }

  # Plot
  p <- ggplot(df, aes(x = x, y = counts, group = grouping_var)) +
    geom_col(aes(fill = bar_color_var), show.legend = show_legend, position = position_dodge(width = dodge), color = "black", width = width, size = bar_border_thickness) +
    scale_fill_manual(name = legend_title, values = colors) +
    scale_y_continuous(name = y_axis_title, limits = y_axis_limits, labels = y_axis_labels, expand = c(0, 0, 0, 0), oob = censor_fn) +
    scale_x_discrete(name = x_axis_title, breaks = x_axis_breaks, labels = x_axis_labels) +
    coord_cartesian(clip = "off", default = TRUE) +
    counts_annotation +
    ggtitle(plot_title) +
    theme_custom(aspect_ratio = aspect_ratio, x_axis_label_angle = x_axis_label_angle, y_axis_title_angle = y_axis_title_angle, ...)
  if (!show_n) return(p)
  n_anno_font_size <- convert(n_anno_font_size, "pt", "mm")
  n_anno_x <- match.arg(n_anno_x, choices = c("right", "left"))
  x_pos <- if (n_anno_x == "right") n_bars else 1
  p <- p + annotate("text", label = paste0(n_prefix, sum(df$counts)), x = x_pos + n_anno_x_nudge, y = y_axis_limits[2], hjust = 1, vjust = 1, size = n_anno_font_size, color = n_anno_font_color)
  suppressWarnings(suppressMessages(p))
}
