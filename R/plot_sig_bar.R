#' geom function to generate significance bars
#'
#' @inheritParams ggplot2::geom_segment
#' @export
geom_sig_bar <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., lineend = "square", linejoin = "mitre", na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSigBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(lineend = lineend, linejoin = linejoin, na.rm = na.rm, ...))
}

#' ggproto for significance bar
#'
#' @export
GeomSigBar <- ggplot2::ggproto(
  "GeomSigBar",
  ggplot2::Geom,
  required_aes = c("x", "xend", "y", "label"),
  non_missing_aes = c("linetype", "size", "shape"),
  default_aes = ggplot2::aes(colour = "black", size = 0.65, linetype = 1, bar_nudge = 0.08, star_nudge = 0.02, text_nudge = 0.05, step_increase = 0.11, star_size = 20, text_size = 12, vjust = 0.5, hjust = 0.5, parse = FALSE),
  draw_panel = function(data, panel_params, coord, lineend = "square", linejoin = "mitre", na.rm = TRUE) {
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0 || inherits(data, "waiver") || (nrow(data) == 1 && anyNA(c(data$x, data$xend, data$y, data$label)))) {
      return(ggplot2::zeroGrob())
    }
    coord <- coord$transform(data, panel_params)
    if (n_unique(coord$group) < 2 && nrow(coord) > 2) {
      coord <- .step_increase(coord)
    }
    y_bar <- coord$y + coord$bar_nudge
    x_label <- (coord$x + coord$xend)/2
    y_label <- y_bar + ifelse(grepl(pattern = "\\*", x = coord$label), coord$star_nudge, coord$text_nudge)
    label_size <- ifelse(grepl(pattern = "\\*", x = coord$label), coord$star_size, coord$text_size)
    labels <- if (coord$parse[1]) parse_safe(coord$label) else coord$label
    color <- coord$colour
    grid::gList(
      grid::textGrob(
        label = labels,
        x = x_label, y = y_label, default.units = "native",
        gp = grid::gpar(col = color, fontsize = label_size)),
      grid::segmentsGrob(
        coord$x, y_bar, coord$xend, y_bar, default.units = "native",
        gp = grid::gpar(
          col = color, fill = color, lwd = coord$size*.pt, lty = coord$linetype, lineend = lineend, linejoin = linejoin), arrow = NULL))
  })

#' Add significance annotation to plot
#'
#' @param plot ggplot object
#' @param bar_nudge Nudge factor along y axis for significance bar as proportion of viewport. Default is `0.08`
#' @param star_nudge udge factor along y axis for stars as proportion of viewport. Default is `0.02`
#' @param text_nudge Nudge factor along y axis for text as proportion of viewport. Default is `0.05`
#' @param step_increase Step increase as proportion of viewport. Default is `0.11`
#' @param bar_thickness Thickness of significance bars. Default is `0.65`
#' @param color Color of significance bar and annotation. Default is `"black"`
#' @param colour Alias for `color`
#' @param size Alias for `bar_thickness`
#' @param vjust Vertical alignment. Default is `0.5`
#' @param hjust Horizontal alignment. Default is `0.5`
#' @param ... Arguments passed to `.create_df_sig`
#' @export
add_p_vals <- function(plot, bar_nudge = 0.08, star_nudge = 0.02, text_nudge = 0.05, step_increase = 0.11, size = 0.65, color = "black", colour = color, bar_thickness = size, vjust = 0.5, hjust = 0.5, ...) {
  # Plot environment
  plot_env <- plot$plot_env

  # Axis arguments
  axis_args <- list()
  axis_args$axis <- "y"
  axis_args$scale <- plot_env$y_scale
  axis_args$title <- get_plot_axis_title(plot, axis = "y")
  axis_args$breaks <- get_plot_axis_breaks(plot, axis = "y")
  axis_args$expand_lower <- plot_env$expand_y
  axis_args$censor_fn <- plot_env$censor_fn

  # Significance annotation
  y_max_type <- switch(plot_env$plot_fn, plot_bar = if (plot_env$show_errorbar) "error" else "summary", plot_point = , plot_bar_point = "raw")
  df_pvalues <- .create_df_sig(plot$data, y_max_type = y_max_type, summary_fn = plot_env$summary_fn, error_fn = plot_env$error_fn, ...)
  plot_sig <- geom_sig_bar(data = df_pvalues, mapping = ggplot2::aes(x = x, xend = xend, y = y, label = label), bar_nudge = bar_nudge, star_nudge = star_nudge, text_nudge = text_nudge, step_increase = step_increase, size = size, colour = color, vjust = vjust, hjust = hjust)
  plot_sig <- plot + plot_sig

  # Plot axis
  axis_args$plot_limits <- range(plot_env$y_min, axis_args$breaks, get_plot_data_limits(plot_sig, "y"), na.rm = TRUE)
  plot_axis <- do.call(scale_axis_clean, axis_args)

  # Final plot
  suppress({
    plot_sig + plot_axis
    })
}
