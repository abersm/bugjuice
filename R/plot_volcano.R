#' Volcano plot
#'
#' @param df Data frame containing columns for fc, p value, and variable name
#' @param var Variable in df containing rownames (gene names if expression data). Enter as quoted variable name. Default is `"gene"`
#' @param fc Variable in df containing fold change values. Enter as quoted variable name. Default is `"fc"`
#' @param pval Variable in df containing p values. Enter as quoted variable name. Default is `"p"`
#' @param cond_color Conditional statement used to color data points. Can also enter comma separated list of quoted values for var wrapped in `c()`
#' @param cond_label Conditional statement used to select which data points to label. Can also enter comma separated list of quoted values for var wrapped in `c()`
#' @param point_size Size of points in mm. Default is `2.5`
#' @param point_border_thickness Thickness of point border. Default is `0.5`
#' @param point_alpha Transparency of points. Default is `0.7`
#' @param colors Colors used to fill data points
#' @param fc_threshold Threshold fc value. Used to generated vertical line. If `NULL`, no line drawn
#' @param pval_threshold Threshold p value. Used to generated horizontal line. If `NULL`, no line drawn
#' @param line_type Line type for threshold lines. Default is `"dashed"`
#' @param line_color Color for threshold lines. Default is `"#909090"`
#' @param line_thickness Thickness for threshold lines in lines units. Default is `0.5`
#' @param x_axis_title Title for x axis. Default is `"Fold change"`
#' @param y_axis_title Title for y axis. Default is `"-log10 P value"`
#' @param x_axis_breaks,y_axis_breaks Breaks for x and y axis respectively
#' @param expand_x,expand_y Expansion for x and y axis respectively. Enter as length 2 numeric vector for % expansion of lower and upper end of axis respectively. Default uses 5% for y axis and 30% for x axis
#' @param str_clean_fn Function applied to values of var. Default capitalizes entries
#' @param italic If `TRUE` (default), var labels converted to italic
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_volcano <- function(
    df,
    var = "gene",
    fc = "fc",
    pval = "p",
    cond_color = NULL,
    cond_label = NULL,
    point_size = 2.5,
    point_alpha = 0.7,
    point_border_thickness = 0.5,
    colors = c("#BC3C29", "#3B3B3B"),
    fc_threshold = 2,
    pval_threshold = 0.01,
    line_type = "dashed",
    line_color = "#909090",
    line_thickness = 0.5,
    x_axis_title = expression("log"[2]*" fold change"),
    y_axis_title = expression("-log"[10]*" "*italic("P")*" value"),
    x_axis_breaks = waiver(),
    y_axis_breaks = waiver(),
    expand_x = c(0.3, 0.3),
    expand_y = c(0.05, 0.05),
    str_clean_fn = function(x) str_capitalize(x, other_letters_lower = TRUE),
    italic = TRUE,
    ...) {
  pkg_required("ggrepel")
  df[[fc]] <- as.numeric(as.character(df[[fc]]))
  df[[pval]] <- as.numeric(as.character(df[[pval]]))
  if (!is.null(cond_label)) {
    if (is.character(cond_label)) {
      df_label <- df[df[[var]] %in% cond_label, , drop = FALSE]
    } else {
      df_label <- dplyr::filter(df, cond_label)
    }
    df_label[[fc]] <- log2(df_label[[fc]])
    df_label[[pval]] <- -log10(df_label[[pval]])
  }
  if (!is.null(cond_color)) {
    if (is.character(cond_color)) {
      df$color_var <- ifelse(df[[var]] %in% cond_color, "a", "b")
    } else {
      df <- dplyr::mutate(df, color_var = dplyr::case_when(cond_color ~ "a", TRUE ~ "b"))
    }
  }
  df[[fc]] <- log2(df[[fc]])
  df[[pval]] <- -log10(df[[pval]])
  if (is.null(cond_color)) {
    p_val <- pval_threshold %||% 0.01
    p_val <- -log10(p_val)
    fc_val <- fc_threshold %||% 2
    fc_val <- log2(fc_val)
    df$color_var <- ifelse(df[[pval]] > p_val & abs(df[[fc]]) >= fc_val, "a", "b")
  }
  if (is.null(cond_label)) {
    p_val <- pval_threshold %||% 0.01
    p_val <- -log10(p_val)
    df1 <- df[df[[pval]] > p_val, , drop = FALSE]
    df1 <- df1[order(df1[[fc]]), , drop = FALSE]
    df_label_top <- dplyr::slice_head(df1, n = 5)
    df_label_bottom <- dplyr::slice_tail(df1, n = 5)
    df_label <- dplyr::bind_rows(df_label_top, df_label_bottom)
    df_label <- dplyr::distinct(df_label, .data[[var]], .keep_all = TRUE)
  }
  if (!is.null(pval_threshold)) {
    horizontal_line <- ggplot2::geom_hline(yintercept = -log10(pval_threshold), linetype = line_type, size = line_thickness, color = line_color)
  } else {
    horizontal_line <- NULL
  }
  vertical_line <- if (!is.null(fc_threshold)) {
    ggplot2::geom_vline(xintercept = c(log2(1/fc_threshold), log2(fc_threshold)), linetype = line_type, size = line_thickness, color = line_color)
  }
  df_label$label <- str_clean_fn(df_label[[var]])
  if (italic) {
    df_label$label <- paste0("italic(", df_label$label, ")")
    parse_text <- TRUE
  } else {
    parse_text <- FALSE
  }
  ggplot(df, aes(.data[[fc]], .data[[pval]])) +
    geom_point(aes(fill = color_var), shape = 21, alpha = point_alpha, stroke = point_border_thickness, size = point_size, show.legend = FALSE) +
    scale_fill_manual(values = c("a" = colors[1], "b" = colors[2])) +
    horizontal_line +
    vertical_line +
    scale_y_continuous(name = y_axis_title, breaks = x_axis_breaks, expand = expansion(expand_y)) +
    scale_x_continuous(name = x_axis_title, breaks = x_axis_breaks, expand = expansion(expand_x)) +
    ggrepel::geom_text_repel(data = df_label,aes(label = label), box.padding = 0.5, max.overlaps = Inf, parse = parse_text) +
    theme_custom(..., axis_line_thickness = 0.7)
}
