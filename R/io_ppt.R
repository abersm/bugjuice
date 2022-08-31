#' Export 1 or more plots to powerpoint
#'
#' Functionality from David Gohel's excellent package officer
#' @param plot_object Plot or list of plots
#' @param file_name File name for plot. Enter as quoted file name
#' @param directory Path to directory where file should be saved. Default is working directory
#' @param height,width Plot height and width (in inches) respectively. Default is `5` for both
#' @param y Vertical position (in inches) of top of plot relative to top of slide. Default is centered
#' @param top Alias for `y`
#' @param x Position (in inches) of leftmost edge of plot relative to leftmost edge of slide. Default is centered
#' @param left Alias for `x`
#' @param editable If `TRUE` (default), plot will be stored as svg. If `FALSE`, plot will be stored as png
#' @param font_family Font family. Default is `"Helvetica"`. Enter as quoted or unquoted font name
#' @param landscape Layout of plot in powerpoint. Options: `"horizontal"` (default), `"vertical"`
#' @param slide_layout Options: `"Plot_center"` (default, centered 5" x 5" plot), `"Plot_center_small"` (centered 3.5" x 3.5" plot) `"Plot_5_5"` (5" x 5" plot on left edge of slide), `"Plot_7_7"` (7" x 7" plot on left edge of slide), `"Plot_5_7"` (5" tall, 7" wide plot on left edge of slide), `"Plot"` (5" tall, 11.5" wide plot on left edge of slide). Enter as quoted slide layout
#' @param theme Name of theme (slide master). Must be a theme present in pptx file `.pptx_template_path`
#' @param fullsize If `FALSE` (default), plot size according to entered or default parameters. If `TRUE`, plot printed to entire size of slide
#' @param fit_within_slide If `TRUE` (default), attempt is made to fit plot within slide boundaries. If FALSE, plot can extend beyond slide limits
#' @export
ppt <- function(
  plot_object,
  file_name = NULL,
  directory = getwd(),
  height = NULL,
  width = NULL,
  y = NULL, top = y,
  x = NULL, left = x,
  editable = TRUE,
  font_family = "Helvetica",
  landscape = "horizontal",
  slide_layout = "Plot_center",
  theme = "default_helvetica",
  fullsize = FALSE,
  fit_within_slide = TRUE) {
  pkg_required("officer")
  pkg_required("rvg")
  file_name <- .safe_file_path(x = file_name %||% get_input(plot_object), ext = "pptx", directory = directory)
  n_plots <- if (inherits(plot_object, "ggplot")) 1L else length(plot_object)
  font_family <- str_capitalize(font_family)

  # Plot dimensions
  if (is.null(height) || is.null(width)) {
    ratios <- vapply(if (n_plots == 1L) list(plot_object) else plot_object, function(y) {
      z <- y$theme$aspect.ratio
      if (!is.numeric(z) || inherits(y, "patchwork")) 1 else z
    }, numeric(1))
    if (is.null(width)) {
      height <- height %||% 5
      width <- height/ratios
    } else {
      height <- width*ratios
    }
  }
  height <- rep(height, length.out = n_plots)
  width <- rep(width, length.out = n_plots)

  # Plot position on slide
  top <- top %||% ((7.5 - height)/2)
  top[top < 0] <- 0
  top <- rep(top, length.out = n_plots)
  left <- left %||% ((40/3 - width)/2)
  left[left < 0] <- 0
  left <- rep(left, length.out = n_plots)

  # Add plots to slides
  blank <- .add_blank_slide(slide_layout = slide_layout)
  capture_plot <- if (editable) function(x) rvg::dml(ggobj = x) else identity
  if (n_plots == 1L) {
    out <- .ppt_add_to_body(blank, capture_plot(plot_object), fullsize = fullsize, top = top[1L], left = left[1L], height = height[1L], width = width[1L], font = font_family, editable = editable)
    print(out, target = file_name)
  } else {
    blank <- .ppt_add_to_body(blank, capture_plot(plot_object[[1L]]), fullsize = fullsize, top = top[1L], left = left[1L], height = height[1L], width = width[1L], font = font_family, editable = editable)
    slides <- function(j) {
      blank <- officer::add_slide(blank, layout = slide_layout, master = theme)
      blank <- .ppt_add_to_body(blank, capture_plot(plot_object[[j]]), fullsize = fullsize, top = top[j], left = left[j], height = height[j], width = width[j], font = font_family, editable = editable)
    }
    n_plots <- length(plot_object)
    lapply(2:n_plots, slides)
    invisible(print(blank, target = file_name))
  }
}

# Code --------------------------------------------------------------------

#' Export plot to powerpoint using plot code
#'
#' Functionality from David Gohel's excellent package officer
#' @param ... Code to generate plot
#' @param directory Directory where plot will be saved. Default is working directory
#' @inheritParams ppt
#' @export
ppt_by_code <- function(..., file_name = "plot", directory = getwd(), height = 5, width = 5, top = 1.67, left = 1, font_family = "Helvetica", slide_layout = "Plot_center", fullsize = FALSE) {
  pkg_required("rvg")
  file_name <- .safe_file_path(file_name, "pptx", directory)
  out <- .ppt_add_to_body(.add_blank_slide(slide_layout = slide_layout), rvg::dml(code = ...), fullsize = fullsize, top = top, left = left, height = height, width = width, font = str_capitalize(font_family))
  print(out, target = file_name)
}

# Helpers -----------------------------------------------------------------

#' Path to pptx template
#'
#' @returns Path to template pptx file
#' @noRd
.pptx_template_path <- system.file("templates", "pptx_template.pptx", package = "bugjuice")

#' Convert to pptx units
#'
#' @param x Numeric vector
#' @param from,to Units of input and desired output respectively. Enter as strings
#' @param from_ppt If `TRUE` (default), conversion is from ppt units to R. If `FALSE`, conversion is from R units to ppt
#' @returns Numeric in units of `to`
#' @noRd
.ppt_convert_units <- function(x, from = "pt", to = "pt", from_ppt = TRUE) {
  # Convert to inches. In powerpoint, 1 in = 72.009 pt
  x <- if (from_ppt && from %in% c("pt", "pts")) {
    x/72.009
  } else {
    convert(x, from, "in")
  }
  convert(x, "in", to, pt_per_in = if (!from_ppt) 72.009 else 72.27)
}

#' Plot coordinates on slide
#'
#' @param n Number of plots on page
#' @param landscape Slide layout Options: `"horizontal"` (default) or `"vertical"`. Only relevant when > 1 plot per page
#' @param plot1_height,plot2_height Height of plots 1 and 2 in inches Default is `5`
#' @param plot1_width,plot2_width Widths of plots 1 and 2 in inches Default is `5`
#' @param plot1_top,plot2_top Vertical distance from top of slide to top of plots 1 and 2 in inches
#' @param plot1_left,plot2_left Horizontal distance from left edge of slide to left edge of plots 1 and 2 in inches
#' @param plot1_ratio,plot2_ratio Aspect ratios of plots 1 and 2
#' @param gap_between Space between plots 1 and 2 in inches. Only relevant when > 2 plots per page
#' @param path_to_template Path to template for graphics device
#' @returns List of plot coordinates x1, y1, etc
#' @noRd
.ppt_plot_coord <- function(
    n,
    landscape = "horizontal",
    plot1_height = 5, plot2_height = 5,
    plot1_width = 5, plot2_width = 5,
    plot1_top = NULL, plot2_top = NULL,
    plot1_left = NULL, plot2_left = NULL,
    plot1_ratio = NULL, plot2_ratio = NULL,
    gap_between = NULL,
    path_to_template = .pptx_template_path) {
  # Determine slide dimensions
  if (path_to_template == .pptx_template_path) {
    slide_height <- 7.5
    slide_width <- 40/3
  } else {
    slide_template <- officer::read_pptx(path_to_template)
    slide_dimensions <- officer::slide_size(slide_template)
    slide_height <- slide_dimensions$height
    slide_width <- slide_dimensions$width
  }

  # Set plot height and width
  if (n == 1) {
    plot2_height <- 0
    plot2_width <- 0
  }
  plot1_height <- plot1_height %||% (plot1_width*plot1_ratio)
  plot1_width <- plot1_width %||% (plot1_height/plot1_ratio)
  plot2_height <- plot2_height %||% (plot2_width*plot2_ratio)
  plot2_width <- plot2_width %||% (plot2_height/plot2_ratio)

  # Set coordinates on slide for top left corner of plot
  if (landscape == "horizontal") {
    # y
    plot1_y <- plot1_top %||% ((slide_height - plot1_height)/2)
    if (n > 1L) {
      plot2_y <- plot2_top %||% ((slide_height - plot2_height)/2)
    }
    # x
    if (n == 1L) {
      plot1_x <- plot1_left %||% ((slide_width - plot1_width)/2)
    } else {
      excess_x <- slide_width - plot1_width - plot2_width
      gap_between <- gap_between %||% (excess_x/3)
      gap_between <- if (gap_between < 0) 0 else gap_between
      excess_x <- excess_x - gap_between
      plot1_x <- plot1_left %||% (excess_x/2)
      plot2_x <- plot2_left %||% (plot1_x + plot1_width + gap_between)
    }
  } else {
    # x
    plot1_x <- plot1_left %||% ((slide_width - plot1_width)/2)
    if (n > 1) {
      plot2_x <- plot2_left %||% ((slide_width - plot2_width)/2)
    }

    # y
    if (n == 1L) {
      plot1_y <- plot1_top %||% ((slide_height - plot1_height)/2)
    } else {
      excess_y <- slide_height - plot1_height - plot2_height
      gap_between <- gap_between %||% (excess_y/3)
      gap_between <- if (gap_between < 0) 0 else gap_between
      excess_y <- excess_y - gap_between
      plot1_y <- plot1_top %||% (excess_y/2)
      plot2_y <- plot2_top %||% (plot1_y + plot1_height + gap_between)
    }
  }
  out <- list(x1 = plot1_x, y1 = plot1_y)
  if (n != 1L) {
    out$x2 <- plot2_x
    out$y2 <- plot2_y
  }
  out
}

#' Add blank ppt slide
#'
#' Functionality from David Gohel's excellent package officer
#' @param slide_layout Options: `"Plot_center"` (default, centered 5" x 5" plot), `"Plot_center_small"` (centered 3.5" x 3.5" plot) `"Plot_5_5"` (5" x 5" plot on left edge of slide), `"Plot_7_7"` (7" x 7" plot on left edge of slide), `"Plot_5_7"` (5" tall, 7" wide plot on left edge of slide), `"Plot"` (5" tall, 11.5" wide plot on left edge of slide). Enter as quoted slide layout
#' @param theme Name of theme (slide master). Must be a theme present in pptx file `.pptx_template_path`
#' @noRd
.add_blank_slide <- function(slide_layout = "Plot_center", theme = "default_helvetica") {
  blank <- officer::read_pptx(.pptx_template_path)
  officer::add_slide(blank, layout = slide_layout, master = theme)
}

#' Add object to body of slide
#'
#' @param slide Powerpoint slide
#' @param obj Object to add to slide. Can be `dml` object or ggplot object. Wrap plot code or ggplot object in `rvg::dml` to make plot editable
#' @param fullsize If `FALSE` (default), plot size according to entered or default parameters. If `TRUE`, plot printed to entire size of slide
#' @param top,left Position of top left corner of `obj` on `slide` in inches. Arguments passed to `officer::ph_location_fullsize` or `officer::ph_location_template`
#' @param height,width Height and width of `obj` in inches
#' @param font Sans serif font used in plot. Default is `"Helvetica"`. Enter as capitalized sans serif font
#' @param editable If `TRUE` (default), `obj` will be editable (must wrap `obj` in `rvg::dml` to make editable)
#' @param bg_color Background color for plot. If `NULL` (default), background will be transparent
#' @noRd
.ppt_add_to_body <- function(slide, obj, fullsize = FALSE, top, left, height, width, font = "Helvetica", editable = TRUE, bg_color = NULL) {
  location_fn <- if (fullsize) officer::ph_location_fullsize else officer::ph_location_template
  officer::ph_with(
    x = slide,
    value = obj,
    location = location_fn(top = top, left = left, height = height, width = width),
    fonts = list(sans = font),
    editable = editable,
    bg = bg_color
  )
}
