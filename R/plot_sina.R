#' Sina plot
#'
#' Functionality from Thomas Lin Pedersen's excellent package ggforce
#' @inheritParams plot_point
#' @inheritParams plot_violin
#' @param method Method used to make width equal across groups. Options: `"density"` (default), `"counts"`
#' @param n_bins Number of bins
#' @param band_width Bandwidth. Options: `"nrd0"` (default), `"nrd"`, `"ucv"`, `"bcv"`, `"sj"`, `"sj-ste"`, `"sj-dpi"`, or integer
#' @param seed Number to input into random number generator for reproducibility
#' @export
plot_sina <- function(
    df,
    formula,
    grouping_var = NULL,
    ...,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    x_order = NULL,
    fn_x_order = mean,
    rev_x_order = FALSE,
    x = NULL, y = NULL,
    colors = c("#0072B5", "#BC3C29", "#8C8C8C", "#2A2D34", "#009872", "#6761A8"),
    point_color_var = NULL,
    point_shape_var = NULL,
    point_shapes = c("circle", "circle"),
    point_border_color = "black",
    alpha = 0.9,
    point_alpha = alpha,
    width = 0.2,
    dodge = 0.7,
    point_border_thickness = 0.75,
    show_legend = FALSE,
    legend_title = "",
    y_scale = "regular",
    y_title = waiver(),
    y_axis_title = y_title,
    x_title = waiver(),
    x_axis_title = x_title,
    plot_title = NULL,
    n_breaks = 3,
    breaks_fn = pretty,
    x_breaks = NULL,
    x_axis_breaks = x_breaks,
    x_labels = NULL,
    x_axis_labels = x_labels,
    y_breaks = NULL,
    y_axis_breaks = y_breaks,
    y_labels = NULL,
    y_axis_labels = y_labels,
    y_max = NULL,
    y_min = NULL,
    expand_y = 0.1,
    expand_x = waiver(),
    censor_fn = rescale_none,
    point_size = 2,
    point_colors = colors,
    band_width = "nrd0",
    n_bins = NULL,
    seed = NA,
    scaling = c("width", "count", "area"),
    method = c("density", "counts"),
    show_sig = TRUE,
    sig_method = "p_by_normality",
    stars = TRUE,
    show_ns = TRUE,
    ns_symbol = "ns",
    sig_bar_nudge = if (y_scale == "log") 0.9 else 0.11,
    sig_star_nudge = if (y_scale == "log") -0.45 else sig_bar_nudge - 0.03,
    sig_text_nudge = if (y_scale == "log") 0.5 else sig_bar_nudge + 0.03,
    step_increase = if (y_scale == "log") 0 else sig_bar_nudge + 0.05,
    sig_bar_thickness = 0.75,
    sig_bar_color = "black",
    sig_font_color = "black",
    sig_star_size = 22,
    sig_text_size = 12,
    p_case = "upper",
    p_spaces = TRUE) {
  # Plotting function
  plot_fn <- "plot_sina"

  # Dots
  if (n_dots(...) > 0L) {
    dots <- eval(substitute(alist(...)), envir = parent.frame())
    if (is.null(names(dots))) {
      subset_criteria <- unlist(lapply(dots, function(z) eval(z, envir = df)), use.names = FALSE)
      df <- df[subset_criteria, ]
      theme_dots <- FALSE
    } else {
      theme_dots <- TRUE
    }
  } else {
    theme_dots <- FALSE
  }

  # Data setup
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, point_color_var = {{point_color_var}}, point_shape_var = {{point_shape_var}}, dplyr::everything())


  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_sina")
  x <- vars$x
  y <- vars$y
  df$y <- df[[y]]
  df$x <- df[[x]]
  df_names <- names(df)
  vars_remove_na <- Intersect(c("x", "y", "grouping_var", "point_color_var", "point_shape_var"), df_names)
  df <- df[complete.cases(df[, vars_remove_na]), ]

  x_levels <- if (is.null(x_order)) {
    create_levels(df$x)
  } else if (length(x_order) == 1L) {
    if (x_order == "ascending") {
      attr(fct_reorder(df$x, df$y, fn_x_order, .increasing = TRUE), "levels")
    } else if (x_order == "descending") {
      attr(fct_reorder(df$x, df$y, fn_x_order, .increasing = FALSE), "levels")
    }
  } else {
    x_order
  }
  if (rev_x_order) {
    x_levels <- Rev(x_levels)
  }

  # Create numeric and factor versions of x
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)
  df$x <- factor(df$x, levels = x_levels)

  # grouping_var
  if (grouping_var %!in% df_names) {
    df$grouping_var <- "a"
  }
  if (!is.factor(df$grouping_var)) {
    df$grouping_var <- as.character(df$grouping_var)
  }
  grouping_var_levels <- grouping_var_order %||% create_levels(df$grouping_var, reverse = rev_grouping_var_order)
  df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)

  # point_color_var
  if ("point_color_var" %!in% df_names) {
    if ("grouping_var" %in% df_names) {
      df$point_color_var <- df$grouping_var
    } else if (length(point_colors) == 1L) {
      df$point_color_var <- "a"
    } else {
      df$point_color_var <- df$x
    }
  }

  # point_shape_var
  if ("point_shape_var" %!in% df_names) {
    df$point_shape_var <- "a"
  }
  if (!is.factor(df$point_color_var)) {
    df$point_color_var <- factor(df$point_color_var)
  }
  if (!is.factor(df$point_shape_var)) {
    df$point_shape_var <- factor(df$point_shape_var)
  }
  colors <- rep(colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # Axis titles
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)

  # y axis
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)
  # Change y values if log scale is used
  if (startsWith(y_scale, "log")) {
    y_values <- df$y
    minimum_y_value <- min(y_values)
    if (minimum_y_value < 1) {
      if (minimum_y_value < 0) {
        warning("Some y values are < 0 (min = ", round_up(minimum_y_value, 2), ")", call. = FALSE)
        message("Will transform y values by adding ", round_up(minimum_y_value, 2), " to each value")
        df$y <- y_values + minimum_y_value
      } else if (minimum_y_value == 0) {
        z <- y_values[y_values > 0 & y_values < 1]
        if (length(z) > 0) {
          y_limits[1] <- min(z)
        } else {
          df$y <- y_values + 1
          y_limits[1] <- min(df$y)
        }
      }
    }
  }

  # Points
  scaling <- match.arg(scaling, choices = c("width", "count", "area"))
  method <- match.arg(method, choices = c("density", "counts"))
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shapes]
  names(point_shapes) <- NULL
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = aes(fill = point_color_var), shape = unique_shapes, color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = aes(fill = point_color_var, shape = point_shape_var), color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- scale_shape_manual(name = NULL, values = point_shapes)
  }

  # Points
  p <- do.call(geom_sina, c(point_args, scaling = scaling, method = method, width = width, position = position_dodge(width = dodge), seed = seed, n_bins = n_bins, band_width = band_width))
  p <- ggplot(df, aes(x = x_numeric, y = y, group = grouping_var)) +
    p +
    scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title/theme
  p <- p + coord_cartesian(clip = "off", default = TRUE) + ggtitle(plot_title)
  p <- if (theme_dots) p + theme_custom(...) else p + theme_custom()

  # Significance annotation
  if (show_sig) {
    if (n_unique(df$grouping_var, na.rm = FALSE) > 1L) {
      fn_sig_anno <- .plot_sig_anno_grouped
      if (startsWith(y_scale, "log")) {
        if (missing(sig_bar_nudge)) {
          y_breaks <- .create_axis_breaks(.limits = c(if (y_limits[1L] == 0) 1 else y_limits[1L], y_limits[2L]), .scale = y_scale, .n = n_breaks, .breaks_fn = breaks_fn)
          y_breaks <- if (y_scale == "log2") log2(y_breaks) else log10(y_breaks)
          sig_bar_nudge <- 0.25*(Max(y_breaks) - Min(y_breaks)) - 0.05*length(y_breaks) + 0.06*point_size - 0.18
        }
        if (missing(sig_text_nudge)) {
          sig_text_nudge <- 1.61*sig_bar_nudge - 0.09
        }
        if (missing(sig_star_nudge)) {
          sig_star_nudge <- 0.66*sig_bar_nudge + 0.05
        }
      }
    } else {
      fn_sig_anno <- .plot_sig_anno
    }
    p <- p + fn_sig_anno(df = df, method = sig_method, dodge = dodge, y_max_type = "raw", y_scale = y_scale, bar_nudge = sig_bar_nudge, star_nudge = sig_star_nudge, text_nudge = sig_text_nudge, star_size = sig_star_size, text_size = sig_text_size, p_case = p_case, p_spaces = p_spaces, bar_thickness = sig_bar_thickness, bar_color = sig_bar_color, font_color = sig_font_color, show_ns = show_ns, stars = stars, ns_symbol = ns_symbol, step_increase = step_increase, breaks_fn = breaks_fn)
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    p + scale_axis_clean(plot_limits = y_plot_limits, scale = y_scale, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis())
  } else {
    p + scale_continuous(limits = y_limits, axis = "y", scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  }
}

#' ggproto for sina plot
#'
#' @export
StatSina <- ggplot2::ggproto("StatSina", ggplot2::Stat,
                    required_aes = c("x", "y"),
                    setup_data = function(data, params) {
                      data
                    },
                    setup_params = function(data, params) {
                      params$width <- params$width %||% (ggplot2::resolution(data$x %||% 0)*0.9)
                      if (is.null(params$bin_width) && is.null(params$n_bins)) {
                        params$n_bins <- 50
                      }
                      params
                    },
                    compute_panel = function(self, data, scales, scaling = TRUE, method = "density", band_width = "nrd0", kernel = "gaussian", bin_width = NULL, n_bins = NULL, width = 1, adj = 1, bin_limit = 1, seed = NA) {
                      n_bins <- if (!is.null(bin_width)) {
                        # If input consists of dates, need to coerce x_range and bin_width to numeric
                        x_range <- scales$y$dimension() + 1e-8
                        boundary <- bin_width/2
                        shift <- floor((x_range[1] - boundary)/bin_width)
                        origin <- boundary + shift*bin_width
                        max_x <- x_range[2] + (1 - 1e-08)*bin_width
                        breaks <- seq(origin, max_x, bin_width)
                        fuzz <- (1e-08)*Median(Diff(breaks))
                        breaks <- sort(breaks)
                        # If closed == "right"
                        fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
                        # If closed == "left": fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
                        structure(list(breaks = breaks, fuzzy = breaks + fuzzes, right_closed = TRUE), class = "ggplot2_bins")
                      } else {
                        x_range <- scales$y$dimension() + 1e-8
                        boundary <- NULL
                        # n_bins <- as.integer(n_bins)
                        if (n_bins == 1) {
                          z <- diff(x_range)
                          boundary <- x_range[1]
                        } else {
                          z <- (x_range[2] - x_range[1])/(n_bins - 1)
                        }
                        if (is.null(boundary)) {
                          boundary <- z/2
                        }

                        # If input consists of dates, need to coerce x_range, z, boundry to numeric
                        shift <- floor((x_range[1] - boundary)/z)
                        origin <- boundary + shift*z
                        max_x <- x_range[2] + (1 - 1e-08)*z
                        breaks <- seq(origin, max_x, z)
                        fuzz <- (1e-08)*Median(Diff(breaks))
                        breaks <- sort(breaks)
                        # If closed == "right"
                        fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1))
                        # If closed == "left": fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
                        structure(list(breaks = breaks, fuzzy = breaks + fuzzes, right_closed = TRUE), class = "ggplot2_bins")
                      }
                      data <- ggplot2::ggproto_parent(Stat, self)$compute_panel(data, scales, scaling = scaling, method = method, band_width = band_width, kernel = kernel, n_bins = n_bins$breaks, width = width, adj = adj, bin_limit = bin_limit)
                      data$sinawidth <- switch(
                        scaling,
                        # area: original densities scaled to max width of 1 for plotting purposes only
                        area = data$density/max(data$density),
                        # count: original densities scaled to max width of 1, then scaled according to number of observations
                        count = data$density/max(data$density)*data$n/max(data$n),
                        # width: constant width (each density scaled to maximum of 1)
                        width = data$scaled)
                      if (!is.na(seed)) {
                        new_seed <- sample(.Machine$integer.max, 1L)
                        set.seed(seed)
                        on.exit(set.seed(new_seed))
                      }
                      data$xmin <- data$x - width/2
                      data$xmax <- data$x + width/2
                      data$x_diff <- runif(nrow(data), min = -1, max = 1)*width*data$sinawidth/2
                      data$width <- width
                      if (all(is_integerish(data$y))) {
                        data$y <- jitter(data$y)
                      }
                      data
                    },
                    compute_group = function(data, scales, scaling = TRUE, method = "density", band_width = "nrd0", kernel = "gaussian", width = 1, adj = 1, bin_limit = 1, n_bins = NULL) {
                      n_rows <- nrow(data)
                      if (n_rows == 0) return(NULL)
                      if (n_rows < 3) {
                        data$density <- 0
                        data$scaled <- 1
                      } else if (method == "density") {
                        range <- range(data$y, na.rm = TRUE)
                        band_width <- if (is.character(band_width)) {
                          switch(band_width,
                                 nrd0 = {
                                   hi <- SD(data$y)
                                   if (!(lo <- min(hi, Diff(Quantile(data$y, probs = c(0.25, 0.75))/1.34)))) {
                                     (lo <- hi) || (lo <- abs(data$y[1L])) || (lo <- 1)
                                   }
                                   0.9*lo*length(data$y)^(-0.2)
                                 },
                                 nrd = {
                                   r <- Quantile(data$y, c(0.25, 0.75))
                                   1.06*min(SD(data$y), (r[2L] - r[1L])/1.34)*length(data$y)^(-1/5)
                                 },
                                 ucv = stats::bw.ucv(data$y),
                                 bcv = stats::bw.bcv(data$y),
                                 sj = ,
                                 `sj-ste` = stats::bw.SJ(data$y, method = "ste"),
                                 `sj-dpi` = stats::bw.SJ(data$y, method = "dpi"),
                                 stop(sprintf("Input 'band_width' (%s) to geom_sina() not recognized", band_width), call. = FALSE))
                        } else {
                          band_width
                        }
                        w <- data$w
                        if (is.null(w)) {
                          w <- rep(1/n_rows, n_rows)
                        }
                        dens <- if (n_rows < 2) {
                          vec_to_df(x = NA_real_, density = NA_real_, scaled = NA_real_, ndensity = NA_real_, count = NA_real_, n = NA_integer_)
                        } else {
                          d <- stats::density(data$y, weights = w, bw = band_width, adjust = adj, kernel = kernel, n = 512, from = range[1], to = range[2])
                          vec_to_df(x = d$x, density = d$y, scaled =  d$y/max(d$y, na.rm = TRUE), ndensity = d$y/max(d$y, na.rm = TRUE), count = d$y*n_rows, n = n_rows)
                        }
                        densf <- stats::approxfun(dens$x, dens$density, rule = 2)
                        data$density <- densf(data$y)
                        data$scaled <- data$density/max(dens$density)
                        data
                      } else {
                        bin_index <- cut(data$y, n_bins, include.lowest = TRUE, labels = FALSE)
                        data$density <- tapply(bin_index, bin_index, length)[as.character(bin_index)]
                        data$density[data$density <= bin_limit] <- 0
                        data$scaled <- data$density/max(data$density)
                      }

                      # Compute width if x has multiple values
                      width <- if (n_unique(data$x) > 1) {
                        Diff(range(data$x))*width
                      } else {
                        width
                      }
                      data$width <- width
                      data$n <- nrow(data)
                      data$x <- mean(range(data$x))
                      data
                    },
                    finish_layer = function(data, params) {
                      z <- (data$xmax - data$xmin)/data$width
                      data$x <- data$x + data$x_diff*z
                      data
                    },
                    extra_params = "na.rm")

#' stat function to generate sina plot
#'
#' @param scaling Scaling for shape of sina plot. Options: `"area"` (default), `"count"`, `"width"`
#' @param method Method to determine spread of points along x axis
#' @param width Width of each group along x axis
#' @param adj Adjusts bandwidth by a specified multiplier
#' @param bin_width Width of each bin
#' @param n_bins Number of bins
#' @param seed Value used to set seed for reproducibility
#' @param na.rm If `TRUE` (default), missing values are removed
#' @inheritParams ggplot2::stat_identity
#' @export
stat_sina <- function(mapping = NULL, data = NULL, geom = "sina", position = "dodge", scaling = "area", method = "density", width = NULL, adj = 1, bin_width = NULL, n_bins = NULL, seed = NA, ..., na.rm = TRUE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = StatSina, geom = geom, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(scaling = scaling, method = method, band_width = "nrd0", kernel = "gaussian", width = width, adj = adj, bin_limit = 1, bin_width = bin_width, n_bins = n_bins, seed = seed, na.rm = na.rm, ...))
}

#' geom function to generate sina plot
#'
#' @inheritParams ggplot2::geom_point
#' @export
geom_sina <- function(mapping = NULL, data = NULL, stat = "sina", position = "dodge", ..., na.rm = TRUE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...))
}
