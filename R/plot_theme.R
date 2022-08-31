#' Custom ggplot2 theme
#'
#' @param base_size Size of text in pts. Default is `16`
#' @param font_family Font used in plots. Default uses helvetica
#' @param aspect_ratio Aspect ratio, y/x. Default is `1`
#' @param ratio Alias for `aspect_ratio`
#' @param axis_tick_length Axis tick length in pts. Default is `base_size/2.5`
#' @param x_axis_tick_length,y_axis_tick_length Length of axis ticks in pts. Default is `base_size/2.5`
#' @param axis_line_thickness Axis and tick thickness in mm. Default is `0.7`
#' @param x_axis_line_thickness,y_axis_line_thickness Default is `0.7`
#' @param plot_margin_top Extra space added above plot in pts. Default is `base_size/2`
#' @param plot_margin_bottom Extra space added below plot in pts. Default is `base_size/2`
#' @param plot_margin_right Extra space added to right of plot in pts. Default is `base_size/2`
#' @param plot_margin_left Extra space added to left of plot in pts. Default is `base_size/2`
#' @param plot_title_font_face Title font face. Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param plot_title_font_color Title font color. Default is `"black"`
#' @param plot_title_font_size Title font size in pts. Default is `base_size + 2`
#' @param x_axis_title_font_size,y_axis_title_font_size Default is `base_size + 2`
#' @param x_axis_title_margin_top Distance from x axis to x axis title. Units in pts. Default is `5`
#' @param x_axis_label_font_face,y_axis_label_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param x_axis_label_angle,y_axis_label_angle Angle of x and y axis tick labels. Default is `0`
#' @param x_angle,y_angle Alias for `x_axis_label_angle` and `y_axis_label_angle`, respectively
#' @param x_axis_label_hjust,x_axis_label_vjust Horizontal and vertical justification of x axis text. Default is `0.5`
#' @param x_axis_label_font_size,y_axis_label_font_size Axis tick label font size in pts. Default is `18`
#' @param x_axis_label_margin_top Margin between x axis tick labels to tick mark in pts. Default is `0.3*base_size`
#' @param x_axis_label_margin_right Margin to the right of x axis tick labels. Default is `0`
#' @param y_axis_title_angle Angle of text used for y axis title. Default is 90
#' @param y_axis_title_margin_right Distance from y axis to x axis title in pts. Default is `0.5*base_size`
#' @param y_axis_label_margin_right Margin between y axis tick labels to tick mark. Units in pts. Default is `0.3*base_size`
#' @param legend_position Legend position. Default is `"right"`
#' @param legend_direction Legend direction. Options: `"vertical"` (default), `"horizontal"`
#' @param legend_title_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param legend_title_font_color Font color of legend title. Default is `"black"`
#' @param legend_title_font_size Units in pts. Default is `base_size`
#' @param legend_label_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param legend_label_font_color Font color of legend labels Default is `"black"`
#' @param legend_label_font_size Units in pts. Default is `base_size`
#' @param legend_margin Space between text labels in legend. Units in pts. Default is `base_size/2`
#' @param legend_symbol_size Size of symbols in legend. Units in pts. Default is `base_size*0.75`
#' @param legend_symbol_border_thickness Thickness of symbols in legend. Units in pt. Default is `1`
#' @param legend_spacing Spacing between legend labels. Units in pts. Default is `1`
#' @param facet_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param facet_font_color Default is `"white"`
#' @param facet_fill_color Facet box fill color. Default is `"#4D4D4D"`
#' @param facet_spacing_x,facet_spacing_y Horizontal and vertical spacing between facets. Units in pts. Default is `base_size/2`
#' @export
theme_custom <- function(
  base_size = 16,
  font_family = "",
  aspect_ratio = 1, ratio = aspect_ratio,
  axis_tick_length = base_size/2.5,
  axis_line_thickness = 0.7,
  x_axis_line_thickness = axis_line_thickness,
  y_axis_line_thickness = axis_line_thickness,
  x_axis_tick_length = axis_tick_length,
  y_axis_tick_length = axis_tick_length,
  plot_margin_top = (20/16)*base_size,
  plot_margin_bottom= base_size/2,
  plot_margin_left = base_size/2,
  plot_margin_right = base_size/2,
  plot_title_font_face = "plain",
  plot_title_font_color = "black",
  plot_title_font_size = base_size + 2,
  x_axis_title_font_size = base_size + 2,
  x_axis_label_font_size = base_size,
  y_axis_title_font_size = base_size + 2,
  y_axis_label_font_size = base_size,
  x_axis_title_margin_top = base_size/4,
  x_axis_label_margin_top = 0.3*base_size,
  x_axis_label_margin_right = 0,
  y_axis_title_angle = 90,
  y_axis_title_margin_right = base_size/2,
  y_axis_label_margin_right = 0.3*base_size,
  x_axis_label_font_face = "plain",
  x_axis_label_angle = 0,
  x_angle = x_axis_label_angle,
  x_axis_label_vjust = 0.5,
  x_axis_label_hjust = 0.5,
  y_axis_label_font_face = "plain",
  y_axis_label_angle = 0,
  y_angle = y_axis_label_angle,
  legend_position = "right",
  legend_title_font_face = "plain",
  legend_title_font_color = "black",
  legend_title_font_size = base_size,
  legend_label_font_face = "plain",
  legend_label_font_color = "black",
  legend_label_font_size = base_size,
  legend_direction = "vertical",
  legend_margin = base_size/2,
  legend_symbol_size = base_size*0.75,
  legend_symbol_border_thickness = 1,
  legend_spacing = 1,
  facet_font_face = "plain",
  facet_font_color = "white",
  facet_fill_color = "#4D4D4D",
  facet_spacing_x = base_size/2,
  facet_spacing_y = base_size/2) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  ggplot2::theme(
    # Lines
    line = element_line(
      color = "black",
      size = axis_line_thickness,
      linetype = 1,
      lineend = "square"),

    # Rectangles
    rect = element_rect(
      fill = "transparent",
      color = "black",
      size = axis_line_thickness,
      linetype = 1),

    # Text
    text = element_text(
      family = font_family,
      face = "plain",
      color = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
      debug = FALSE),

    # Axis
    # Axis lines
    axis.line = element_line(
      color = "black",
      linetype = 1,
      size = axis_line_thickness,
      lineend = "square"),
    axis.line.x = element_line(
      color = "black",
      linetype = 1,
      size = x_axis_line_thickness,
      lineend = "square"),
    axis.line.y = element_line(
      color = "black",
      linetype = 1,
      size = y_axis_line_thickness,
      lineend = "square"),

    # Axis ticks
    axis.ticks.x = element_line(
      color = "black",
      size = x_axis_line_thickness,
      linetype = 1,
      lineend = "square",
      arrow = FALSE),
    axis.ticks.y = element_line(
      color = "black",
      size = y_axis_line_thickness,
      linetype = 1,
      lineend = "square",
      arrow = FALSE),
    axis.ticks.length.x = grid::unit(x_axis_tick_length, "pt"),
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = grid::unit(y_axis_tick_length, "pt"),
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,

    # Axis text
    axis.text.x = element_text(
      face = x_axis_label_font_face,
      color = "black",
      angle = x_angle,
      size = x_axis_label_font_size,
      margin = ggplot2::margin(t = x_axis_label_margin_top, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust),
    axis.text.y = element_text(
      face = y_axis_label_font_face,
      color = "black",
      angle = y_angle,
      size = y_axis_label_font_size,
      margin = ggplot2::margin(r = y_axis_label_margin_right, unit = "pt"),
      hjust = 1),

    # Axis title
    axis.title.x = element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = x_axis_title_margin_top, unit = "pt"),
      vjust = 0.5),
    axis.title.y = element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = y_axis_title_angle,
      margin = ggplot2::margin(r = y_axis_title_margin_right, unit = "pt"),
      vjust = 0.5),

    # Legend
    legend.position = legend_position,
    legend.direction = legend_direction,
    legend.justification = "center",
    legend.background = element_blank(),
    legend.spacing = grid::unit(legend_spacing, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = ggplot2::margin(legend_margin, legend_margin, legend_margin, legend_margin, unit = "pt"),
    legend.key = element_blank(),
    legend.key.size = grid::unit(legend_symbol_border_thickness, unit = "pt"),
    legend.key.height = grid::unit(legend_symbol_size, unit = "pt"),
    legend.key.width = grid::unit(legend_symbol_size, unit = "pt"),
    legend.title = element_text(
      face = legend_title_font_face,
      color = "black",
      size = legend_title_font_size,
      hjust = 0,
      margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")),
    legend.title.align = NULL,
    legend.text = element_text(
      color = legend_label_font_color,
      face = legend_label_font_face,
      size = legend_label_font_size,
      hjust = 0,
      margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")),
    legend.text.align = NULL,
    legend.box = NULL,
    legend.box.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
    legend.box.background = element_blank(),
    legend.box.spacing = grid::unit(base_size, "pt"),

    # Panel (plotting area bounded by axes)
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = grid::unit(facet_spacing_x, "pt"),
    panel.spacing.y = grid::unit(facet_spacing_y, "pt"),
    panel.ontop = FALSE,

    # Strip
    strip.background = element_rect(
      fill = facet_fill_color,
      color = "black",
      size = 1),
    strip.text = element_text(
      face = facet_font_face,
      color = facet_font_color,
      size = base_size,
      margin = ggplot2::margin(0.4*base_size, 0.4*base_size, 0.4*base_size, 0.4*base_size, unit = "pt")),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = grid::unit(base_size/4, "pt"),
    strip.switch.pad.wrap = grid::unit(base_size/4, "pt"),

    # Plot (area including panel area, plot title, axis title, legend)
    plot.background = element_blank(),
    plot.title = element_text(
      face = plot_title_font_face,
      color = plot_title_font_color,
      size = plot_title_font_size,
      hjust = 0.5,
      vjust = 1,
      margin = ggplot2::margin(b = base_size/2, unit = "pt")),
    plot.subtitle = element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = base_size/2, unit = "pt")),
    plot.caption = element_text(
      size = ggplot2::rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = base_size/2, unit = "pt")),
    plot.tag = element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5),
    plot.tag.position = "topleft",
    plot.margin = ggplot2::margin(t = plot_margin_top, r = plot_margin_right, b = plot_margin_bottom, l = plot_margin_left, unit = "pt")) +
    ggplot2::theme(aspect.ratio = ratio)
}

#' Theme with transparent background and no grid lines
#'
#' @inheritParams theme_custom
#' @param x_axis_label_angle Angle for x axis tick labels
#' @param x_angle Alias for `x_axis_label_angle`
#' @param ... Arguments passed to `theme()`
#' @export
theme_plain <- function(base_size = 14, axis_line_thickness = 0.7, aspect_ratio = NULL, y_axis_title_angle = 90, x_axis_label_angle = 0, x_angle = x_axis_label_angle, x_axis_label_hjust = 0.5, x_axis_label_vjust = 0.5, x_axis_label_margin_right = 0, x_axis_title_font_size = base_size + 2, y_axis_title_font_size = base_size + 2, ...) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  ggplot2::theme(
    text = element_text(size = base_size, color = "black"),
    rect = element_blank(),
    line = element_line(color = "black", size = axis_line_thickness, linetype = 1, lineend = "square"),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = base_size, color = "black"),
    axis.line = element_line(color = "black", size = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks.length = grid::unit(base_size/2.5, "pt"),
    axis.text.x = element_text(
      face = "plain",
      color = "black",
      angle = x_angle,
      size = base_size,
      margin = ggplot2::margin(t = 0.3*base_size, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust),
    axis.text.y = element_text(
      face = "plain",
      color = "black",
      angle = 0,
      size = base_size,
      margin = ggplot2::margin(r = 0.3*base_size, unit = "pt"),
      hjust = 1),
    axis.title.x = element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = base_size/3, unit = "pt"),
      vjust = 0.5),
    axis.title.y = element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = 90,
      margin = ggplot2::margin(r = base_size/2, unit = "pt"),
      vjust = 0.5),
    aspect.ratio = aspect_ratio,
    ...)
}

#' Blank/empty theme
#'
#' No grid lines, axis lines/ticks, axis labels/titles, background color, border, or legend
#' @param ... Arguments passed to `ggplot2::theme`
#' @export
theme_blank <- function(...) {
  ggplot2::theme(
    line = element_blank(),
    text = element_blank(),
    rect = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    ...
  )
}

#' Theme for cowplot functions
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @rdname theme_custom
#' @export
theme_clean <- function(base_size = 14, font_family = "") {
  theme_void(base_size = base_size, base_family = font_family) %+replace%
    theme(
      line = element_blank(),
      rect = element_blank(),
      text = element_text(family = font_family, face = "plain", color = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
      axis.line = element_blank(), axis.line.x = NULL, axis.line.y = NULL,
      axis.text = element_blank(), axis.text.x = NULL, axis.text.x.top = NULL, axis.text.y = NULL, axis.text.y.right = NULL,
      axis.ticks = element_blank(), axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(), axis.title.x = NULL, axis.title.x.top = NULL, axis.title.y = NULL, axis.title.y.right = NULL,
      legend.background = element_blank(),
      legend.spacing = unit(base_size, "pt"), legend.spacing.x = NULL, legend.spacing.y = NULL,
      legend.margin = margin(0, 0, 0, 0),
      legend.key = element_blank(), legend.key.size = unit(1.1*base_size, "pt"), legend.key.height = NULL, legend.key.width = NULL,
      legend.text.align = NULL,
      legend.title = element_text(hjust = 0), legend.title.align = NULL,
      legend.position = "none",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL, legend.box.margin = margin(0, 0, 0, 0), legend.box.background = element_blank(), legend.box.spacing = grid::unit(base_size, "pt"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(), panel.grid.major = NULL, panel.grid.minor = NULL,
      panel.spacing = unit(base_size/2, "pt"), panel.spacing.x = NULL, panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = element_blank(),
      strip.text = element_blank(), strip.text.x = NULL, strip.text.y = NULL,
      strip.placement = "inside", strip.placement.x = NULL, strip.placement.y = NULL,
      strip.switch.pad.grid = unit(0, "cm"), strip.switch.pad.wrap = unit(0, "cm"),
      plot.background = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.tag = element_text(face = "bold", hjust = 0, vjust = 0.7), plot.tag.position = c(0, 1),
      plot.margin = margin(0, 0, 0, 0),
      complete = TRUE)
}
