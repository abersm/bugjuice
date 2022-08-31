#' Draw grob
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @param grob grob object
#' @param x,y x and y starting positions Enter as numeric 0-1. Default is `1` for both
#' @param height,width Height and width of grob Enter as numeric 0-1. Default is `0` for both
#' @param scale Scaling factor for plot. Default is `1`
#' @param clip Clipping for grob. Default is `"inherit"` (inherits from input grob)
#' @param hjust Adjust horizontal position of each label. More negative values move the label further to the right on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `-0.5`
#' @param vjust Adjust vertical position of each label. More positive values move the label further down on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `1.5`
#' @param halign,valign Horizontal and vertical alignment, respectively. Enter as numeric 0-1. Default is `0.5` for both
#' @export
draw_grob <- function(grob, x = 0, y = 0, width = 1, height = 1, scale = 1, clip = "inherit", hjust = 0, vjust = 0, halign = 0.5, valign = 0.5) {
  layer(data = vec_to_df(x = NA), stat = StatIdentity, position = PositionIdentity, geom = GeomDrawGrob, inherit.aes = FALSE, params = list(grob = grob, xmin = x - hjust*width, xmax = x + (1 - hjust)*width, ymin = y - vjust*height, ymax = y + (1 - vjust)*height, scale = scale, clip = clip, halign = halign, valign = valign))
}

#' Geom for draw_grob
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @importFrom grid viewport grobTree
#' @export
GeomDrawGrob <- ggplot2::ggproto("GeomDrawGrob",
                        ggplot2::GeomCustomAnn,
                        draw_panel = function(self, data, panel_params, coord, grob, xmin, xmax, ymin, ymax, scale = 1, clip = "inherit", halign = 0.5, valign = 0.5) {
                          corners <- vec_to_df(x = c(xmin, xmax), y = c(ymin, ymax))
                          data <- coord$transform(corners, panel_params)
                          x_rng <- range(data$x, na.rm = TRUE)
                          y_rng <- range(data$y, na.rm = TRUE)
                          vp_outer <- grid::viewport(x = min(x_rng) + halign*diff(x_rng), y = min(y_rng) + valign*diff(y_rng), width = diff(x_rng), height = diff(y_rng), just = c(halign, valign), clip = clip)
                          vp_inner <- grid::viewport(x = halign, y = valign, width = scale, height = scale, just = c(halign, valign))
                          id <- local({
                            i <- 1
                            function() {
                              i <<- i + 1
                              i
                            }
                          })()
                          inner_grob <- grid::grobTree(grob, vp = vp_inner, name = paste(grob$name, id))
                          grid::grobTree(inner_grob, vp = vp_outer, name = paste("GeomDrawGrob", id))
                        })

#' Convert object to grob
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @param x Plotting object or grob
#' @noRd
as_grob <- function(x) {
  if (inherits(x, "grob")) {
    x
  } else if (inherits(x, "gList")) {
    grid::grobTree(x)
  } else if (inherits(x, c("ggplot", "patchwork"))) {
    # device <- null_dev_env$current
    device <- function(width, height) {
      grDevices::pdf(NULL, width = width, height = height)
      grDevices::dev.control("enable")
    }
    cur_dev <- grDevices::dev.cur()
    device(width = 6, height = 6)
    null_dev <- grDevices::dev.cur()
    on.exit({
      grDevices::dev.off(null_dev)
      if (cur_dev > 1) grDevices::dev.set(cur_dev)
    })
    if (inherits(x, "patchwork")) {
      pkg_required("patchwork")
      patchwork::patchworkGrob(x)
    } else {
      ggplot2::ggplotGrob(x)
    }
  } else {
    grid::nullGrob()
  }
}
