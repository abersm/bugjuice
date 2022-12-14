#' Remove main geoms from plot
#'
#' @param x ggplot object
#' @param geom_pattern Pattern in geom class to search in layers. If `NULL` (default), all geoms are removed
#' @param nth_instance Integer vector indicating indices of layers with `geom_pattern` that should be removed. If `NULL` (default), all instances are removed
#' @export
remove_geoms <- function(x, geom_pattern = NULL, nth_instance = NULL) {
  layers <- x$layers
  if (is.null(geom_pattern)) {
    layers <- list()
  } else {
    nth_layer <- grep(geom_pattern, lapply(layers, function(z) class(z$geom)), ignore.case = TRUE)
    n <- length(nth_layer)
    if (n == 0L) return(x)
    if (!is.null(nth_instance)) {
      nth_layer <- nth_layer[n]
    }
    layers <- layers[-nth_layer]
  }
  x$layers <- layers
  x
}

#' Edit labels for 1 or more plots
#'
#' Functionality from package kwb.plot
#'
#' @param plot_list List of ggplot objects
#' @param ... Enter name-value pair where name refer to plot component to relabel ("x", "y", "title") and values are new labels to be added entered as quoted string(s). If number of new labels is less than number of plots, terms will be recycled
#' @param indices Plots to apply new new labels to entered as number index. Default includes all plots
#' @param action Method for creating new plot label. Options: `"replace"` (default), `"paste_after"`, `"paste_before"`. Enter as quoted or unquoted action
#' @param sep Text used to separate prior label from new label. Default is `" "`
#' @returns List of ggplot objects with updated labels
#' @export
set_plot_labels <- function(plot_list, ..., indices = seq_along(plot_list), action = "replace", sep = " ") {
  args <- list(...)
  if (length(args)) {
    plot_labels <- lapply(args, function(x) rep(x, length.out = length(indices)))
    plots[indices] <- lapply(seq_along(indices), function(i) {
      p <- plot_list[[indices[i]]]
      new_labels <- lapply(plot_labels, "[", i)
      if (action != "replace") {
        new_labels <- lapply(names(new_labels), function(j) {
          old_label <- ggplot2::labs(p)$labels[[j]]
          if (action == "paste_after") {
            paste(old_label, new_labels[[j]], sep = sep)
          } else {
            paste(new_labels[[j]], old_label, sep = sep)
          }
        })
        names(new_labels) <- names(plot_labels)
      }
      p + do.call(ggplot2::labs, new_labels)
    })
  }
  plots
}

#' Remove plot clipping
#'
#' @param x ggplot object
#' @export
unclip <- function(x) {
  # grob <- ggplot2::ggplotGrob(x)
  # grob$layout$clip[grob$layout$name %in% component] <- "off"
  # cowplot::ggdraw(x)
  ggplot() + coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") + scale_x_continuous(name = NULL) + scale_y_continuous(name = NULL) + theme_clean() + draw_grob(as_grob(x), x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}

#' Resize plot aspect ratio
#'
#' @param x ggplot object to resize. Default is last plot
#' @param ratio New aspect ratio. Defaults to golden ratio
#' @export
plot_resize <- function(x = last_plot(), ratio = 1.618034) x + theme(aspect.ratio = ratio)

#' Remove plot margins
#'
#' @param x ggplot object
#' @returns ggplot object with no margins
#' @export
remove_plot_margins <- function(x) {
  x + theme(plot.margin = margin(0, 0, 0, 0, "pt"))
}

#' Remove legend
#'
#' @export
remove_legend <- function() ggplot2::theme(legend.position = "none")

#' Rotate x axis labels
#'
#' @inheritParams ggplot_add.continuous_axis
#' @returns Not called directly by user
#' @export
ggplot_add.rotated_x_axis <- function(object, plot, object_name) {
  axis <- plot$theme$axis.text.x
  axis$hjust <- object$hjust
  axis$vjust <- object$vjust
  axis$angle <- object$angle
  plot + theme(axis.text.x = axis)
}

#' Rotate x axis labels
#'
#' @param angle Angle to rotate x axis labels. Default is `45`
#' @returns ggplot object with x axis labels rotated `angle` degrees. Enter as plot + rot_x_labs()
#' @export
rotate_x_labels <- function(angle = 45) {
  args <- list(angle = angle)
  if (angle > 0) {
    args$hjust <- 1
    args$vjust <- 1
  } else {
    args$hjust <- 0.5
    args$vjust <- 0.5
  }
  structure(args, class = "rotated_x_axis")
}

#' Remove theme element from plot
#'
#' @param x Character vector of `ggplot2::theme()` arguments to remove from plot
#' @returns theme object with `x` removed. Enter as plot + remove_plot_element("axis.line.x")
#' @export
remove_plot_element <- function(x) {
  new_theme <- theme()
  for (i in x) {
    new_theme[[i]] <- element_blank()
  }
  new_theme
}

#' Remove axis lines
#'
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @returns theme object with axis lines removed. Enter as plot + remove_axis_lines("x")
#' @export
remove_axis_lines <- function(axis = c("x", "y")) remove_plot_element(paste0("axis.line.", axis))

#' Remove axis tick marks
#'
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @returns theme object with axis lines removed. Enter as plot + remove_axis_ticks("x")
#' @export
remove_axis_ticks <- function(axis = c("x", "y")) remove_plot_element(paste0("axis.ticks.", axis))

#' Remove axis labels
#'
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @returns theme object with axis lines removed. Enter as plot + remove_axis_text("x")
#' @export
remove_axis_text <- function(axis = c("x", "y")) remove_plot_element(paste0("axis.text.", axis))

#' Remove axis title
#'
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @returns theme object with axis lines removed. Enter as plot + remove_axis_title("x")
#' @export
remove_axis_title <- function(axis = c("x", "y")) remove_plot_element(paste0("axis.title.", axis))

#' Remove axis
#'
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @returns theme object with axis lines removed. Enter as plot + remove_axis("x")
#' @export
remove_axis <- function(axis = c("x", "y")) {
  z <- c("axis.line.", "axis.ticks.", "axis.text.", "axis.title.")
  z <- if (length(axis) == 1) {
    paste0(z, axis)
  } else {
    c(paste0(z, "x"), paste0(z, "y"))
  }
  remove_plot_element(z)
}
