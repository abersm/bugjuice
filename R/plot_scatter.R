#' Scatter plot
#'
#' @param x,y Continuous variables variables. Enter as quoted or unquoted variable names
#' @param point_border_color_var Variable used to determine border color of points. Enter as quoted or unquoted variable name
#' @param point_size_var Variable used to determine point size. Enter as quoted or unquoted variable name
#' @param border_color Alias for `point_border_color`
#' @param point_shape Point shape Options: `"circle"` (default), `"square"`, `"triangle"`, `"diamond"`
#' @param shape,size,stroke Alias for `point_shape`, `point_size`, `point_border_thickness` respectively
#' @param x_scale Scale used for x axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param x_min Minimum value for x axis. Default is `NULL`
#' @param x_max Maximum value for x axis. Default is `NULL`
#' @param scale Alias for `y_scale` and `x_scale` that sets both simultaneously
#' @param expand_y,expand_x Expansion to add to y and x axis, respectively. Default is `0.1`
#' @param expand Alias for `expand_y` and `expand_x.` Sets both simultaneously
#' @inheritParams plot_cor
#' @inheritParams plot_point
#' @export
plot_scatter <- function(
  df,
  formula = NULL, x = NULL, y = NULL,
  size = 3, point_size = size, point_size_var = NULL,
  shape = c("circle", "square", "triangle", "diamond"), point_shape = shape, point_shape_var = NULL,
  colors = c("#0072B5", "#BC3C29", "#868686", "#3B3B3B"), point_colors = colors, point_color_var = NULL,
  border_color = "black", point_border_color = border_color, point_border_color_var = NULL,
  stroke = 0.75, point_border_thickness = stroke,
  alpha = 0.75, point_alpha = alpha,
  n_breaks = 3,
  y_max = NULL, y_min = NULL,
  x_max = NULL, x_min = NULL,
  y_axis_title = waiver(), y_axis_breaks = NULL, y_axis_labels = NULL,
  x_axis_title = waiver(), x_axis_breaks = NULL, x_axis_labels = NULL,
  expand = 0.1, expand_y = expand, expand_x = expand,
  scale = "regular", x_scale = scale, y_scale = scale,
  show_legend = FALSE,
  censor_fn = rescale_none,
  plot_title = NULL,
  ...) {
  plot_fn <- "plot_scatter"
  # Data setup
  df <- dplyr::select(df, point_shape_var = {{point_shape_var}}, point_size_var = {{point_size_var}}, point_color_var = {{point_color_var}}, point_border_color_var = {{point_border_color_var}}, dplyr::everything())
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_scatter")
  x <- vars$x
  y <- vars$y
  df$y <- df[[y]]
  df$x <- df[[x]]
  df_names <- names(df)
  vars_remove_na <- Intersect(c("x", "y", "point_shape_var", "point_size_var", "point_color_var", "point_border_color_var"), df_names)
  df <- df[complete.cases(df[, vars_remove_na]), ]

  # Build point components
  set_args <- list(show.legend = show_legend, alpha = point_alpha, stroke = point_border_thickness)
  mapped_args <- list()

  ## Shape
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shape]
  names(point_shapes) <- NULL
  if (!any(df_names == "point_shape_var") || n_unique(df$point_shape_var, na.rm = FALSE) == 1) {
    set_args$shape <- point_shapes[1]
  } else {
    point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
    mapped_args <- alist(shape = point_shape_var)
    if (is_categorical(df$point_shape_var)) {
      df$point_shape_var <- factor(df$point_shape_var, levels = create_levels(df$point_shape_var))
    }
  }

  ## Size
  if ("point_size_var" %!in% df_names || n_unique(df$point_size_var, na.rm = FALSE) == 1L) {
    set_args$size <- point_size[1L]
  } else {
    point_size <- rep(point_size, length.out = n_unique(df$point_size_var, na.rm = FALSE))
    mapped_args <- c(mapped_args, alist(size = point_size_var))
  }

  ## Color
  n_colors <- suppressWarnings(n_unique(df$point_color_var, na.rm = FALSE))
  if (n_colors %in% c(0, 1)) {
    set_args$fill <- point_colors[1L]
    point_fill <- NULL
  } else {
    mapped_args <- c(mapped_args, alist(fill = point_color_var))
    if (is_categorical(df$point_color_var)) {
      df$point_color_var <- factor(df$point_color_var, levels = create_levels(df$point_color_var))
      point_colors <- rep(point_colors, length.out = n_colors)
      point_fill <- scale_fill_manual(values = point_colors)
    } else {
      point_fill <- scale_fill_gradient(low = point_colors[1L], high = point_colors[[length(point_colors)]])
    }
  }

  ## Border color
  n_border_colors <-suppressWarnings(n_unique(df$point_border_color_var, na.rm = FALSE))
  if (n_border_colors %in% c(0, 1)) {
    set_args$color <- point_border_color[1L]
  } else {
    point_border_color <- rep(point_border_color, length.out = n_border_colors)
    mapped_args <- c(mapped_args, alist(color = point_border_color_var))
    if (is_categorical(df$point_border_color_var)) {
      df$point_border_color_var <- factor(df$point_border_color_var, levels = create_levels(df$point_border_color_var))
    }
  }

  set_args$mapping <- if (length(mapped_args) == 0) NULL else do.call("aes", mapped_args)
  point <- do.call("geom_point", set_args)

  # Axes
  x_limits <- range(df$x)
  y_limits <- range(df$y)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)

  # Plot
  ggplot(df, aes(x = x, y = y)) +
    point +
    point_fill +
    scale_color_manual(name = NULL, values = point_border_color) +
    scale_shape_manual(name = NULL, values = point_shapes) +
    scale_size() +
    scale_continuous(limits = c(x_min %||% x_limits[1], x_max %||% x_limits[2]), axis = "x", scale = x_scale, breaks = x_axis_breaks, labels = x_axis_labels, title = x_axis_title, expand_lower = expand_x, n_breaks = n_breaks, censor_fn = censor_fn) +
    scale_continuous(limits = c(y_min %||% y_limits[1], y_max %||% y_limits[2]), axis = "y", scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn) +
    coord_cartesian(clip = "off") +
    ggtitle(plot_title) +
    theme_custom(...)
}
