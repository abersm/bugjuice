# Limits ------------------------------------------------------------------

#' Data limits for all data frames used to generate plot
#'
#' @param x ggplot object
#' @param axis Options: `"both"` (default), `"x"`, `"y"`
#' @returns List containing `x` and `y`, each a length 2 numeric vector with limits in raw data units
#' @export
get_plot_data_limits <- function(x, axis = "both") {
  plot_data <- ggplot2::ggplot_build(x)$data
  switch(axis,
         y = {
           y_vars <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
           y_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% y_vars
             z[idx]
           }), use.names = FALSE)
           if (length(y_axis) == 0L) return(NULL) else Range(y_axis)
         },
         x = {
           x_vars <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")
           x_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% x_vars
             z[idx]
           }), use.names = FALSE)
           if (length(x_axis) == 0L) return(NULL) else Range(x_axis)
         },
         both = {
           y_vars <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
           x_vars <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")
           x_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% x_vars
             z[idx]
           }), use.names = FALSE)
           y_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% y_vars
             z[idx]
           }), use.names = FALSE)
           x_axis <- if (length(x_axis) == 0L) NULL else Range(x_axis)
           y_axis <- if (length(y_axis) == 0L) NULL else Range(y_axis)
           list(x = x_axis, y = y_axis)
         })
}

#' Axis limits
#'
#' Limits for axis line. When clip = "off", data limits will extend past axis limits
#'
#' @inheritParams get_plot_data_limits
#' @param units Units for output. Options: `"data"` (default, raw data), `"npc"`
#' @returns If `axis = "x"` or `axis = "y"`, output is a length 2 numeric vector containing limits. If `axis = "both"`, output is a list containing `x` and `y`, each a length 2 numeric vector containing axis limits
#' @export
get_plot_axis_limits <- function(x, axis = "both", units = "data") {
  if (units == "npc") return(get_plot_limits(x))
  plot_scales <- ggplot2::layer_scales(x)
  switch(
    axis,
    x = {
      plot_scales$x$range$range %||% ggplot2::ggplot_build(x)$layout$panel_scales_x[[1L]]$break_info()$range
    },
    y = {
      # unlist(remove_null(lapply(seq_along(x$scales$scales), function(i) x$scales$scales[[i]]$limits)), use.names = FALSE)
      plot_scales$y$limits %||% ggplot2::ggplot_build(x)$layout$panel_scales_y[[1L]]$break_info()$range
    },
    both = {
      if (is.null(plot_scales$x$range$range) || is.null(plot_scales$y$limits)) {
        plot_build <- ggplot2::ggplot_build(x)
        list(
          x = plot_scales$x$range$range %||% plot_build$layout$panel_scales_x[[1L]]$break_info()$range,
          y = plot_scales$y$limits %||% plot_build$layout$panel_scales_y[[1L]]$break_info()$range)
      } else {
        list(
          x = plot_scales$x$range$range,
          y = plot_scales$y$limits)
      }
    })
}

#' Overall limits for plot (data + axis)
#'
#' @inheritParams get_plot_data_limits
#' @returns List containing `x` and `y` (each length 2 numeric vectors representing min and max value). If plot has multiple facets, output will be a list (1 for each facet), each containing a list with `x` and `y` (each length 2 numeric vectors representing min and max value)
#' @export
get_plot_limits <- function(x) {
  plot_build <- ggplot2::ggplot_build(x)
  plot_summary <- ggplot2::summarise_layout(plot_build)
  # If plot has multiple facets, length of each value for min or max will be number of facets
  if (length(plot_summary$panel) != 1L) {
    out <- lapply(df_to_list_rowwise(plot_summary), function(z) {
      list(
        x = z$xmin, z$xmax,
        y = z$ymin, z$ymax
      )
    })
    names(out) <- sprintf("row%s_col%s", plot_summary$row, plot_summary$col)
    out
  } else {
    list(
      x = c(plot_summary$xmin, plot_summary$xmax),
      y = c(plot_summary$ymin, plot_summary$ymax)
    )
  }
}

# geoms -------------------------------------------------------------------

#' Get names of geoms used by plot
#'
#' @param x ggplot object
#' @returns Geom names as character vector in lower case
#' @export
get_plot_geom_names <- function(x) {
  tolower(vapply(x$layers, function(z) gsub("New|Geom", "", class(z$geom)[[1L]]), character(1), USE.NAMES = FALSE))
}

#' List stat functions used by a plot
#'
#' @rdname get_plot_geom_names
#' @returns Stat names as character vector in lower case
#' @export
get_plot_stat_names <- function(x) {
  tolower(vapply(x$layers, function(z) gsub("New|Stat", "", class(z$geom)[[1L]]), character(1), USE.NAMES = FALSE))
}

#' Determine whether ggplot object contains a given type of geom
#'
#' @param x ggplot object
#' @param geom_class Class of geom used by plot ("GeomErrorbar", "GeomSegment", "GeomText", etc.)
#' @returns LEngth 1 logical vector
#' @noRd
.plot_contains_geom <- function(x, geom_class) {
  if (geom_class %in% c("Bar", "bar", "GeomBar")) {
    geom_class <- "GeomRect"
  } else {
    if (grepl("_", geom_class, fixed = TRUE)) {
      geom_class <- unlist(strsplit(geom_class, "_", fixed = TRUE), use.names = FALSE)
      geom_class <- paste(str_capitalize(geom_class), collapse = "")
    }
    if (grepl("geom", geom_class, fixed = TRUE)) {
      geom_class <- gsub("geom", "Geom", geom_class, fixed = TRUE)
    }
    if (!grepl("Geom", geom_class, fixed = TRUE)) {
      geom_class <- paste("Geom", str_capitalize(geom_class), sep = "")
    }
  }
  any(vapply(seq_along(x$layers), function(z) inherits(x$layers[[z]]$geom, geom_class), logical(1), USE.NAMES = FALSE))
}

# Plot components ---------------------------------------------------------

#' Get plot title
#'
#' @param x ggplot object
#' @returns Plot title as string
#' @export
get_plot_title <- function(x) x$labels$title

#' Get all variables names in plot data
#'
#' Functionality from gginnards
#' @rdname get_plot_title
#' @returns List of variables in data frames used to create plots. Also plot title
#' @export
get_plot_vars <- function(x) x$labels

#' Get variables used to generate x and y axes
#'
#' @inheritParams get_plot_data_limits
#' @returns Raw data input variables used to generate x and/or y axis
#' @export
get_plot_axis_vars <- function(x, axis = "both") {
  labels <- x$labels
  if (axis == "both") {
    list(x = labels$x, y = labels$y)
  } else {
    labels[[axis]]
  }
}

#' Get axis breaks
#'
#' @inheritParams get_plot_data_limits
#' @param units Options: `"data"` (default, raw data units), `"npc"`
#' @export
get_plot_axis_breaks <- function(x, axis = "both", units = "data") {
  plot_layout <- ggplot_build(x)$layout
  if (units == "npc") {
    #x_breaks <- plot_layout$panel_scales_x[[1L]]$break_info()$major
    #y_breaks <- plot_layout$panel_scales_y[[1L]]$break_info()$major
    x_breaks <- plot_layout$panel_params[[1L]]$x$break_positions()
    y_breaks <- plot_layout$panel_params[[1L]]$y$break_positions()
  } else {
    #x_breaks <- plot_layout$panel_scales_x[[1L]]$get_breaks()
    #y_breaks <- plot_layout$panel_scales_y[[1L]]$get_breaks()
    x_breaks <- plot_layout$panel_params[[1L]]$x$breaks
    y_breaks <- plot_layout$panel_params[[1L]]$y$breaks
  }
  switch(axis,
         y = y_breaks,
         x = x_breaks,
         both = list(y = y_breaks, x = x_breaks))
}

#' Get axis tick labels
#'
#' @inheritParams get_plot_data_limits
#' @returns Character vector containing axis tick labels
#' @export
get_plot_axis_labels <- function(x, axis = "both") {
  axis_labels <- ggplot_build(x)$layout$panel_params[[1L]]
  switch(axis,
         x = as.character(axis_labels$x$get_labels()),
         y = as.character(axis_labels$y$get_labels()),
         both = list(
           x = as.character(axis_labels$x$get_labels()),
           y = as.character(axis_labels$y$get_labels())))
}

#' Get axis title
#'
#' @rdname get_plot_axis_labels
#' @export
get_plot_axis_title <- function(x, axis = "both") {
  plot_build <- ggplot_build(x)$layout
  switch(axis,
         x = plot_build$panel_scales_x[[1L]]$name,
         y = plot_build$panel_scales_y[[1L]]$name,
         both = list(
           x = plot_build$panel_scales_x[[1L]]$name,
           y = plot_build$panel_scales_y[[1L]]$name))
}

#' Get plot aspect ratio
#'
#' @param x ggplot object
#' @returns List containing entry into theme function for aspect ratio and actual aspect ratio in data units (if set)
#' @export
get_plot_aspect_ratio <- function(x) {
  z <- get_plot_axis_limits(x)
  list(
    theme = x$theme$aspect.ratio,
    data_units_y_to_x = ggplot_build(x)$layout$coord$ratio,
    axis = (z$y[2L] - z$y[1L])/(z$x[2L] - z$x[1L]))
}

#' Determine whether object is a ggplot object
#'
#' Code from ggplot2
#' @param x Object to test
#' @returns Length 1 logical
#' @noRd
is_ggplot <- function(x) inherits(x, "ggplot")

#' Determine number of ggplot plots in an object
#'
#' @param x Object
#' @returns Length 1 integer
#' @noRd
n_ggplots <- function(x) if (is_ggplot(x)) 1L else length(x)

#' Waiver object
#'
#' Code from ggplot2
#' @returns List with length 0 and class "waiver"
#' @noRd
waiver <- function() structure(list(), class = "waiver")

#' Determine whether object is a waiver object
#'
#' Code from ggplot2
#' @param x Object to test
#' @returns Length 1 logical
#' @noRd
is_waiver <- function(x) inherits(x, "waiver")

#' Determine optimal coordinates for annotation/label placement
#'
#' @param x ggplot object
#' @param geom Name of plotted geom to avoid when placing label. Enter as lower case character vector
#' @param x_nudge_left,x_nudge_right Placement of label along x axis as percent of axis
#' @param y_nudge_top,y_nudge_bottom Placement of label along y axis as percent of axis
#' @param plot_limits List composed of "x" and "y" (each length 2 numeric vector containing min and max value for plot)
#' @returns Length 2 numeric vector containing x and y coordinates for label
#' @noRd
.label_coordinates_optimal <- function(x, geom, x_nudge_left = 0.1, x_nudge_right = 0.8, y_nudge_top = 0.9, y_nudge_bottom = 0.2, plot_limits = get_plot_limits(x)) {
  y_limits <- plot_limits$y
  x_limits <- plot_limits$x
  x_min <- x_limits[1L]
  x_max <- x_limits[2L]
  y_min <- y_limits[1L]
  y_max <- y_limits[2L]
  delta_x_limits <- x_max - x_min
  delta_y_limits <- y_max - y_min
  df_geom <- get_plot_geom_data(x, geom)
  if (!is.data.frame(df_geom)) {
    #df_geom <- Reduce(rbind, df_geom)
    df_geom <- bind_rows(df_geom)
  }
  df_names <- names(df_geom)
  geom_x_limits <- Range(unlist(df_geom[df_names %in% c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")], use.names = FALSE))
  geom_y_limits <- Range(unlist(df_geom[df_names %in% c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")], use.names = FALSE))
  delta_x <- abs(x_limits - geom_x_limits)
  delta_y <- abs(y_limits - geom_y_limits)
  x_multiplier <- if (delta_x[1L] > delta_x[2L]) x_nudge_right else x_nudge_left
  x <- x_min + x_multiplier*delta_x_limits
  y_multiplier <- if (delta_y[1L] > delta_y[2L]) y_nudge_top else y_nudge_bottom
  y <- y_min + y_multiplier*delta_y_limits
  c(x, y)
}
