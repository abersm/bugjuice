# Plots -------------------------------------------------------------------

#' Plot correlation coefficient, p value, linear regression line
#'
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""`
#' @param show_error If `FALSE` (default), error region not displayed If `TRUE`, error region displayed with color as determined by error_color
#' @param error_color Color used to fill error region around line. Default is grey
#' @param show_corr_coef If `TRUE` (default), correlation coefficient and p value are displayed. If `FALSE`, correlation coefficient and p value are not displayed
#' @param anno_position Position of correlation coefficient and p value. Can enter as numeric vector of length 2 containing x and y coordinates or a combination of t or b and l or r in quotes
#' @param method Method for performing correlation test. Options include `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param corr_prefix Prefix for correlation coefficient annotation. If `NULL`, rho used for spearman, R for Pearson, tau for kendall
#' @param p_prefix Prefix for P values. Default is `"P"`. Enter as quoted text
#' @param show_n If `TRUE` (default), number of observations displayed as component of correlation annotation
#' @param n_case Case for number of observations annotation. Default is `"lower"`
#' @param show_rsq If `TRUE` (default), R squared and p value are displayed. If `FALSE`, R squared and p value are not displayed
#' @inheritParams plot_scatter
#' @inheritParams plot_line
#' @export
plot_cor <- function(df, formula, grouping_var = NULL, line_colors = "#0072B5", line_thickness = 1, point_colors = "#3B3B3B", point_color_var = NULL, point_size = 4, point_alpha = 1, y_scale = "regular", y_axis_breaks = NULL, y_max = NULL, y_min = NULL, expand_y = 0.1, y_axis_labels = NULL, y_axis_title = NULL, x_scale = "regular", x_axis_breaks = NULL, x_max = NULL, x_min = NULL, expand_x = 0.1, x_axis_title = NULL, plot_title = "", show_error = FALSE, error_color = "grey60", show_legend = FALSE, show_corr_coef = TRUE, anno_position = "best", method = "spearman", corr_prefix = NULL, p_prefix = "P", show_n = TRUE, n_case = "lower", show_rsq = TRUE, ...) {
  # Data setup
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, point_color_var = {{point_color_var}}, dplyr::everything())
  df <- .create_plot_df(df, formula, .vars_remove_na = c("grouping_var", "point_color_var"))
  df_names <- names(df)

  # Point color
  if (any(df_names == "point_color_var")) {
    if (!inherits(df$point_color_var, "factor")) {
      df$point_color_var <- as.character(df$point_color_var)
    }
    df$point_color_var <- factor(df$point_color_var, levels = create_levels(df$point_color_var))
  } else {
    df$point_color_var <- "a"
  }
  n_point_colors <- n_unique(df$point_color_var, na.rm = FALSE)
  point_colors <- rep(point_colors, length.out = n_point_colors)

  # Line color
  if (length(line_colors) > 1) {
    if (!(any(df_names) == "grouping_var")) {
      line_colors <- rep(line_colors, length.out = n_point_colors)
      df$line_color_var <- df$point_color_var
    } else {
      line_colors <- rep(line_colors, length.out = n_unique(df$grouping_var, na.rm = FALSE))
      df$line_color_var <- df$grouping_var
    }
  } else {
    df$line_color_var <- "a"
  }

  # Axis limits
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)
  x_limits <- .set_axis_limits(.min = x_min, .max = x_max, .breaks = x_axis_breaks, .values = df$x)

  # Plot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(fill = point_color_var), shape = 21, size = point_size, show.legend = show_legend, alpha = point_alpha) +
    scale_fill_manual(values = point_colors, name = NULL) +
    geom_smooth(aes(group = line_color_var), method = lm, se = show_error, formula = y ~ x, size = line_thickness, fill = error_color, show.legend = FALSE) +
    scale_color_manual(values = line_colors) +
    scale_continuous(axis = "y", scale = y_scale, limits = y_limits, title = y_axis_title, expand_lower = expand_y) +
    scale_continuous(axis = "x", scale = x_scale, limits = x_limits, title = x_axis_title, expand_lower = expand_x) +
    coord_cartesian(clip = "off") +
    theme_custom(...)
  if (!show_corr_coef && !show_rsq && !show_n) {
    return(p)
  } else {
    if (x_scale == "log") {
      df$x <- log10(df$x + 1)
    }
    if (y_scale == "log") {
      df$y <- log10(df$y + 1)
    }

    # Annotation position
    if (all(is.character(anno_position))) {
      anno_position <- tolower(anno_position)
      if (anno_position == "best") {
        y_delta <- diff(y_limits)
        plot_data <- ggplot_build(p)
        plot_data <- plot_data$data[[2L]]
        y_left <- plot_data$y[which.min(plot_data$x)]
        delta_left <- abs(y_left/y_delta)
        delta_left <- Max(c(delta_left, 1 - delta_left))
        y_right <- plot_data$y[which.max(plot_data$x)]
        delta_right <- abs(y_right/y_delta)
        delta_right <- Max(c(delta_right, 1 - delta_right))
        anno_slope <- if (y_left > y_right) "negative" else "positive"
        anno_horizontal <- if (delta_left > delta_right) "l" else "r"
        anno_vertical <- if (anno_slope == "negative") {
          if (anno_horizontal == "l") "b" else "t"
        } else {
          if (anno_horizontal == "l") "t" else "b"
        }
        anno_position <- paste0(anno_vertical, anno_horizontal)
      }
      label_y_pos <- get_plot_axis_breaks(p)
      label_y_pos <- label_y_pos$y
      label_y_pos <- range(label_y_pos, na.rm = TRUE)
      label_y_pos <- if (grepl(pattern = "b", x = anno_position, fixed = TRUE)) {
        label_y_pos[1L] + 0.2*diff(label_y_pos) - expand_y*diff(label_y_pos)
      } else {
        label_y_pos[1L] + 0.9*diff(label_y_pos)
      }
      label_x_pos <- get_plot_axis_breaks(p)
      label_x_pos <- range(label_x_pos$x, na.rm = TRUE)
      label_x_pos <- if (grepl(pattern = "l", x = anno_position, fixed = TRUE)) {
        label_x_pos[1L] + 0.1*diff(label_x_pos) - expand_x*diff(label_x_pos)
      } else {
        label_x_pos[1L] + 0.57*diff(label_x_pos)
      }
      if (y_scale == "log") {
        label_y_pos <- 10^label_y_pos
      }
      if (x_scale == "log") {
        label_x_pos <- 10^label_x_pos
      }
    }

    # Correlation coefficient annotation
    separator <- if (show_corr_coef && show_rsq) ", " else "\n"
    if (show_corr_coef) {
      corr_p <- suppress(cor.test(df$x, df$y, use = "pairwise.complete.obs", method = method))
      corr_p <- corr_p$p.value
      corr_prefix <- corr_prefix %||% case_when(method == "spearman" ~ "\u03c1", method == "kendall" ~ "\u03c4", TRUE ~ "R")
      corr_prefix <- paste(corr_prefix, "=", " ")
      corr_r <- suppress(cor(df$x, df$y, use = "pairwise.complete.obs", method = method))
      corr_annotation <- paste0(corr_prefix, sprintf("%.2f", corr_r))
      corr_p_annotation <- format_p_value(corr_p, p_prefix = p_prefix)
      corr_annotation <- paste(corr_annotation, corr_p_annotation, sep = separator)
    }

    # R squared annotation
    if (show_rsq) {
      fit <- Lm(df = df, formula = y ~ x)
      rsq_annotation <- paste0("R", "\u00b2", " = ", sprintf("%.2f", lm_glance(fit)$r_sq))
      rsq_p_annotation <- format_p_value(lm_glance(fit)$p, p_prefix = p_prefix)
      rsq_annotation <- paste(rsq_annotation, rsq_p_annotation, sep = separator)
    }
    if (show_corr_coef && show_rsq) {
      corr_annotation <- paste(corr_annotation, rsq_annotation, sep = "\n")
    } else if (show_rsq) {
      corr_annotation <- rsq_annotation
    }

    # N annotation
    if (show_n) {
      n_case <- tolower(n_case)
      n_prefix <- ifelse(n_case == "lower", "n = ", "N = ")
      n_annotation <- df[complete.cases(df[, c("x", "y")]), ]
      n_annotation <- nrow(n_annotation)
      n_annotation <- paste0(n_prefix, n_annotation)
      corr_annotation <- paste(corr_annotation, n_annotation, sep = "\n")
    }
    p + annotate(x = label_x_pos, y = label_y_pos, hjust = 0, label = corr_annotation, geom = "text", parse = FALSE)
  }
}

#' Correlation heatmap
#'
#' @param df Data frame in wide form (columns will be correlated with one another)
#' @param ... Variables to be included in heatmap
#' @param colors Color for heatmap. Default from blue (low) to red (high)
#' @param method Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param min_variance Minimum variance allowed for variable above which variable will be excluded from heatmap. Default is `1` (includes all columns)
#' @export
plot_cor_heatmap <- function(df, ..., colors = clr_continuous(c("#0072B5", "white", "#BC3C29"), n_colors = 100), method = c("spearman", "pearson", "kendall"), min_variance = 1) {
  pkg_required("corrplot")
  df <- if (n_dots(...) == 0) df else dplyr::select(df, ...)
  method <- match.arg(method, choices = c("spearman", "pearson", "kendall"))
  df <- df[vars_numeric(df)]
  incl_vars <- vapply(df, Var, numeric(1), USE.NAMES = TRUE)
  df <- df[names(incl_vars)[incl_vars > min_variance]]
  d_cor_mat <- suppressWarnings(cor(df, method = method, use = "pairwise.complete.obs"))
  corrplot::corrplot(d_cor_mat, method = "color", outline = TRUE, type = "lower", order = "AOE", tl.col = "black", tl.srt = 45, tl.cex = 0.5, diag = FALSE, col = colors)
}
