#' Add continuous axis scale for ggplot
#'
#' Must be exported to work correctly. Not called directly by user
#' @param object Output from `scale_continuous`
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns Called indirectly via by `scale_continuous()`
#' @export
ggplot_add.continuous_axis <- function(object, plot, object_name) {
  x_or_y <- object$axis
  axis_fn <- match_fun(sprintf("scale_%s_continuous", x_or_y))
  limits <- object$limits %||% get_plot_data_limits(plot, axis = x_or_y) %||% get_plot_axis_limits(plot, axis = x_or_y)
  switch(object$scale,
         regular = {
           trans <- scales::identity_trans()
           labels <- object$labels %||% axis_label_numeric
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "regular", .breaks_fn = object$breaks_fn, .n = object$n_breaks)
         },
         scientific = {
           trans <- scales::identity_trans()
           labels <- object$labels %||% axis_label_x10
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "regular", .breaks_fn = object$breaks_fn, .n = object$n_breaks)
         },
         log2 = {
           trans <- scales::log2_trans()
           labels <- object$labels %||% scales::trans_format("log2", scales::math_format(2^.x))
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "log2", .n = object$n_breaks, .breaks_fn = object$breaks_fn)
         },
         log = ,
         log10 = {
           trans <- scales::log10_trans()
           labels <- object$labels %||% scales::trans_format("log10", scales::math_format(10^.x))
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "log10", .n = object$n_breaks, .breaks_fn = object$breaks_fn)
         },
         stop(sprintf("In scale_continuous(scale = '%s'), '%s' is not a recognized scale. Options for 'scale' include ''regular', 'scientific', 'log2', 'log10', or 'log'", scale), call. = FALSE))

  position <- object$position %||% if (x_or_y == "y") "left" else "bottom"
  title <- object$title %W% get_plot_axis_title(plot, x_or_y)
  suppress({
    plot + axis_fn(name = title, limits = range(limits, breaks, na.rm = TRUE), breaks = breaks, labels = labels, trans = trans, oob = object$censor_fn, position = position, expand = c(object$expand_lower, 0, object$expand_upper, 0), guide = guide_clean_axis())
  })
}

#' Programmatically create axis for continuous variable
#'
#' @param axis Options include `"y"` (default) or `"x"`
#' @param scale Options include `"regular"` (default), `"scientific"`, `"log10"`, `"log2"`
#' @param title Axis title
#' @param limits Minimum and maximum values in data. Must enter in order, i.e. c(lower, upper)
#' @param n_breaks Desired number of axis breaks. Default is `4`
#' @param breaks Numeric vector or function specifying location of ticks along axis
#' @param labels Vector or function specifying axis tick labels
#' @param expand_lower,expand_upper Expansion around lower and upper range of axis. Default is `0.1` for `expand_lower` and `0` for `expand_upper`
#' @param position Location of axis. Options: `"left"` (default for y axis), `"right"`, "bottom" (default for x axis), "top"
#' @param censor_fn Function used to transform data outside axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param breaks_fn Function used to create breaks. Enter as call using (). Default is `pretty`. Alternative is `scales::breaks_extended(n = n_breaks, only.loose = TRUE)`
#' @param ... Arguments passed to scale function
#' @returns List passed to `ggplot_add.continuous_axis`. Enter as `plot + scale_continuous()`
#' @export
scale_continuous <- function(axis = "y", scale = "regular", title = "", limits = NULL, breaks = NULL, labels = NULL, expand_lower = 0, expand_upper = 0, n_breaks = 4, position = NULL, censor_fn = rescale_none, breaks_fn = pretty, ...) {
  structure(list(axis = axis, scale = scale, title = title, limits = limits, breaks = breaks, labels = labels, expand_lower = expand_lower, expand_upper = expand_upper, n_breaks = n_breaks, position = position, censor_fn = censor_fn, breaks_fn = breaks_fn, ...), class = "continuous_axis")
}

#' Add simplified x10^n axis scale for ggplot
#'
#' Must be exported to work correctly. Not called directly by user
#' @param object New scale to add
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns Called when `axis_10x()` used
#' @export
ggplot_add.x10_axis <- function(object, plot, object_name) {
  x_or_y <- object$axis
  axis_fn <- match_fun(sprintf("scale_%s_continuous", x_or_y))
  limits <- object$limits
  limits <- limits %W% get_plot_data_limits(plot, axis = x_or_y)
  breaks <- object$breaks
  if (is.null(breaks) || is_waiver(breaks)) {
    breaks <- get_plot_axis_breaks(plot, x_or_y)
  }
  max_break <- max(breaks, na.rm = TRUE)
  if (max_break <= 0) {
    warning("Can't use scale_x10() function when maximum break is <= 0", call. = FALSE)
    labels <- object$labels
    if (is_waiver(labels)) {
      labels <- get_plot_axis_labels(plot, x_or_y)
      if (is_waiver(labels)) {
        labels <- axis_label_numeric
      }
    }
    title <- object$title %W% get_plot_axis_title(plot, x_or_y)
  } else {
    z <- log10(max_break)
    if (ceiling(z) >= object$max_digits) {
      z <- floor(z)
      labels <- axis_label_numeric(breaks/10^z)
      title <- object$title
      if (!is.null(title)) {
        if (is_waiver(title)) {
          plot_title <- get_plot_axis_title(plot, x_or_y)
          title <- if (is_waiver(plot_title) || is.null(plot_title)) "" else plot_title
        }
        title_suffix <- if (object$add_space) {
          if (object$show_parentheses) {
            sprintf("'(\u00D7'~'10'^'%s'*')'", z)
          } else {
            sprintf("'\u00D7'~'10'^'%s'", z)
          }
        } else {
          if (object$show_parentheses) {
            sprintf("'(\u00D710'^'%s'*')'", z)
          } else {
            sprintf("'\u00D710'^'%s'", z)
          }
        }
        #title <- str2expression(paste0(if (inherits(title, "character")) shQuote(title) else paste0(title), "~", title_suffix))
        title <- str2expression(paste0(if (is.character(title)) shQuote(title) else paste0(title), "~", title_suffix))
      }
    } else {
      labels <- object$labels
      labels <- labels %W% get_plot_axis_labels(plot, x_or_y)
      labels <- labels %W% axis_label_numeric
      title <- object$title %W% get_plot_axis_title(plot, x_or_y)
    }
  }
  suppress({
    plot + axis_fn(name = title, limits = range(limits, breaks, na.rm = TRUE), breaks = breaks, labels = labels, trans = scales::identity_trans(), oob = object$censor_fn, position = object$position, expand = c(object$expand_lower, 0, object$expand_upper, 0), guide = guide_clean_axis())
  })
}

#' Programmatically create simplified x10^n axis scale for ggplot
#'
#' @inheritParams scale_continuous
#' @param max_digits Maximum number of digits allowed for maximum break. Default is `3` (all breaks must be < 1000)
#' @param show_parentheses If `TRUE` (default), x10^n multiplier is wrapped in `"()"`
#' @param add_space If `TRUE` (default), space is added between multiplication symbol and 10^n
#' @returns Enter as` plot + scale_x10()`
#' @export
scale_x10 <- function(axis = "y", title = waiver(), limits = waiver(), breaks = waiver(), labels = waiver(), expand_lower = 0, expand_upper = 0, max_digits = 3, n_breaks = 4, position = NULL, censor_fn = rescale_none, breaks_fn = pretty, show_parentheses = TRUE, add_space = TRUE, ...) {
  structure(list(axis = axis, scale = scale, title = title, limits = limits, breaks = breaks, labels = labels, expand_lower = expand_lower, expand_upper = expand_upper, max_digits = max_digits, n_breaks = n_breaks, position = position, censor_fn = censor_fn, breaks_fn = breaks_fn, show_parentheses = show_parentheses, add_space = add_space, ...), class = "x10_axis")
}

#' Input to guide in scale functions to generate clean axis
#'
#' Needed to place significance annotation outside of plot limits
#' @inheritParams scale_continuous
#' @param breaks Axis breaks
#' @param expand_lower,expand_upper Expansion to add to ends of axis
#' @param plot_limits Limits of data for plotting all objects in plotting space
#' @param axis_limits Limits used to generate axis breaks
#' @export
scale_axis_clean <- function(
  axis = "y",
  scale = "regular",
  plot_limits,
  axis_limits,
  title = NULL,
  breaks = NULL,
  breaks_fn = pretty,
  n_breaks = 4,
  labels = NULL,
  expand_lower = 0,
  expand_upper = 0,
  position = NULL,
  censor_fn = rescale_none,
  ...) {
  scale <- match.arg(scale, choices = c("regular", "scientific", "log10", "log", "log2"))
  position <- position %||% if (axis == "y") "left" else "bottom"
  axis_fn <- match.fun(sprintf("scale_%s_continuous", axis))

  # Breaks
  breaks <- breaks %||% .create_axis_breaks(.limits = axis_limits, .scale = scale, .breaks_fn = breaks_fn, .n = n_breaks)

  # Labels
  labels <- labels %||% switch(scale, regular = axis_label_numeric, scientific = axis_label_x10, log2 = scales::trans_format("log2", scales::math_format(2^.x)), log10 = , log = scales::trans_format("log10", scales::math_format(10^.x)))

  # Trans
  trans <- switch(scale, log2 = scales::log2_trans(), log = , log10 = scales::log10_trans(), scales::identity_trans())

  # Limits
  plot_limits <- range(plot_limits, breaks, na.rm = TRUE)
  if (startsWith(scale, "log") && plot_limits[1L] <= 0) {
    plot_limits[1L] <- -Inf
  }

  suppress(axis_fn(name = title, limits = plot_limits, breaks = breaks, labels = labels, trans = trans, oob = censor_fn, position = position, expand = ggplot2::expansion(mult = c(expand_lower, expand_upper)), ...))
}

# Limits ------------------------------------------------------------------

#' Create axis limits for continuous variable (including log scale)
#'
#' @param breaks_fn Function used to generate breaks. Default is `axis_breaks_continuous()`
#' @returns Enter as `scale_y_continuous(limits = axis_limits())`
#' @export
axis_limits <- function(breaks_fn = axis_breaks_continuous()) {
  force(breaks_fn)
  function(x) {
    if (all(is.infinite(x))) return(x)
    breaks <- breaks_fn(x)
    breaks <- range(breaks)
    lower <- Min(c(breaks[1L], x[1L]))
    upper <- Max(c(breaks[2L], x[2L]))
    if (lower != upper) c(lower, upper) else x
  }
}

# Breaks ------------------------------------------------------------------

#' Create axis breaks for continuous variable
#'
#' @param n_breaks Desired number of axis breaks. Default is `4`
#' @param breaks_fn Function used to generate breaks. Enter as call using (). Default is `pretty.` Alternative is `scales::breaks_extended(n = n_breaks, only.loose = TRUE)`
#' @param min_breaks Minimum number of possible axis breaks. Default is `4`
#' @param max_breaks Maximum number of breaks allowed. Default is `6`
#' @param expand Length 2 numeric vector containing expansion arguments for lower and upper ends of axis respectively. Default is `0` for both
#' @param include_negative If `FALSE` (default), negative numbers are excluded from breaks
#' @returns Enter as `scale_y_continuous(breaks = axis_breaks_continuous())`
#' @export
axis_breaks_continuous <- function(n_breaks = 4, breaks_fn = pretty, min_breaks = 4, max_breaks = 6, expand = c(0, 0), include_negative = FALSE) {
  function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) return(numeric())
    # Back calculate limits to account for expand_y
    d <- diff(x)/(1 + expand[1L] + expand[2L])
    l <- x[1L] + d*expand[1L]
    u <- x[2L] - d*expand[2L]
    x <- c(l, u)

    if (is.null(breaks_fn)) {
      breaks <- scales::breaks_extended(n = n_breaks, only.loose = TRUE)(x)
    } else {
      breaks_fn <- match.fun(breaks_fn)
      breaks <- breaks_fn(x, n = n_breaks)
    }

    len_breaks <- length(breaks)
    if (!include_negative) {
      breaks <- breaks[breaks >= 0]
    }
    if (len_breaks > max_breaks && is_odd(len_breaks)) {
      breaks <- breaks[seq(1, len_breaks, by = 2)]
    }
    if (len_breaks == 3 && min_breaks > 3) {
      breaks <- seq(breaks[1], breaks[3], by = (breaks[3] - breaks[1])/4)
    }
    breaks
  }
}

# Labels ------------------------------------------------------------------

#' Labeling function for continuous axis
#'
#' @param x Numeric vector
#' @param max_leading_zeroes Maximum number of leading zeroes when max < 1. Default is `3`
#' @param max_trailing_zeroes Maximum number of leading zeroes when max > 1. Default is `4`
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Enter as `scale_y_continuous(labels = axis_label_numeric)`
#' @export
axis_label_numeric <- function(x, max_leading_zeroes = 3, max_trailing_zeroes = 4, unicode = TRUE) {
  x_max <- abs(Max(x))
  if (x_max < 10^(-1 - max_leading_zeroes) || x_max >= 10^max_trailing_zeroes) {
    axis_label_x10(x, unicode)
  } else {
    format(x, scientific = FALSE)
  }
}

#' Create base^x axis labels
#'
#' @param base Logarithm base. Default is `10`
#' @returns Function that returns an expression. Enter as `scale_(x|y)_continuous(labels = axis_label_log())`
#' @export
axis_label_log <- function(base = 10) {
  force(base)
  function(x) {
    z <- ifelse(
      is.na(x),
      NA,
      ifelse(
        x == 0,
        "0",
        paste0(ifelse(x > 0, "", "-"), base, "^", round(log(abs(x), base = base)))
      )
    )
    parse(text = z)
  }
}

#' Labeling function to generate scientific "multiplier x 10^exp" labels
#'
#' @param x Numeric vector containing position of axis breaks
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Enter as `scale_y_continuous(labels = axis_label_x10)`. Output includes multiplier prior to x sign
#' @export
axis_label_x10 <- function(x, unicode = TRUE) {
  if (unicode) {
    has_decimal <- any(grepl(pattern = "\\.", x = format(x, scientific = TRUE)))
    axis_labels <- function(j) {
      if (is.na(j)) {
        return("")
      } else if (j == 0) {
        return(0)
      }
      j <- unlist(strsplit(format(j, scientific = TRUE), "e"), use.names = FALSE)
      j1 <- j[1L]
      if (has_decimal) {
        if (!grepl(pattern = "\\.", x = j1)) {
          j1 <- paste0(j1, ".0")
        }
      }
      bquote(.(paste(j1, "\u00d7", "10"))^.(as.numeric(j[2])))
    }
    # do.call("expression", lapply(x, function(z) tryCatch(axis_labels(z), error = function(e) NA)))
    as.expression(lapply(x, axis_labels))
  } else {
    v1 <- format(x, scientific = TRUE)
    v1 <- gsub("0e\\+00","0", v1)
    v1 <- gsub("^(.*)e", "'\\1'e", v1)
    v1 <- gsub("e\\+","e", v1)
    v1 <- gsub("e", "%*%10^", v1)
    parse(text = v1)
  }
}

# Helper functions --------------------------------------------------------

#' Set minimum value for axis
#'
#' @param .min,.max Minimum and maximum values respectively
#' @param .breaks Numeric vector of axis breaks
#' @param .values Numeric vector containing raw data along axis of interest
#' @noRd
.set_axis_limits <-  function(.min, .max, .breaks, .values) {
  min_value <- if (!is.null(.min)) {
    .min
  } else if (!is.null(.breaks)) {
    Min(.breaks)
  } else {
    Min(.values)
  }
  max_value <- if (!is.null(.max)) {
    .max
  } else if (!is.null(.breaks)) {
    Max(.breaks)
  } else {
    Max(.values)
  }
  c(min_value, max_value)
}

#' Create axis breaks for continuous axis
#'
#' @param .limits Length 2 numeric vector with lower limit first and upper limit second. Input is usually limits of raw data
#' @param .scale Options: `"regular"` (default), `"scientific"`, `"log2"`, `"log10"`, `"log"`
#' @param .breaks_fn Breaks function. Input must take numeric length 2 numeric vector as input and argument called n. Only relevant when .scale is regular or scientific. Extended breaks are used if `NULL.` Default is `pretty`
#' @param .n Number of axis breaks. Default is `4`
#' @importFrom scales breaks_extended
#' @noRd
.create_axis_breaks <- function(.limits, .scale = "regular", .breaks_fn = pretty, .n = 4) {
  .breaks_fn <- .breaks_fn %||% scales::breaks_extended(only.loose = TRUE)
  if (.scale %in% c("regular", "scientific")) {
    breaks <- .breaks_fn(.limits, n = .n)
    len_breaks <- length(breaks)
    if (len_breaks > 6 && is_odd(len_breaks)) {
      breaks <- breaks[seq(1, len_breaks, by = 2)]
    }
    if (len_breaks == 3) {
      breaks <- seq(breaks[1], breaks[3], by = (breaks[3] - breaks[1])/4)
    }
  } else if (grepl("log", .scale, fixed = TRUE)) {
    base <- if (.scale == "log2") 2 else 10
    log_limits <- log(.limits, base = base)
    upper <- base^(ceiling(log_limits[2L]))
    lower <- base^(floor(log_limits[1L]))
    .limits <- c(upper, lower)
    breaks <- base^.breaks_fn(log(.limits, base = base), n = .n)
    breaks <- breaks[log(breaks, base = base) %% 1 == 0]
  }
  breaks
}

#' Clean y axis
#'
#' @inheritParams ggplot2::guide_axis
#' @return Enter as `guides(axis = guide_clean_axis())` or `scale_(x|y)_continuous(guide = guide_clean_axis())`
#' @noRd
guide_clean_axis <- function(title = waiver(), check.overlap = FALSE, angle = NULL, n.dodge = 1, order = 0, position = waiver()) {
  structure(
    list(
      title = title,
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,
      order = order,
      position = position,
      available_aes = c("x", "y"),
      name = "axis"),
    class = c("guide", "clean", "axis"))
}

#' Method for for guide_gengrob applied to "clean" object
#'
#' Must be exported to be used correctly. Not called directly by user
#' @param guide Guide
#' @param theme Theme
#' @returns Used in `guide_clean_axis` function
#' @export
guide_gengrob.clean <- function(guide, theme) {
  aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
  draw_axis_clean(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    check.overlap = guide$check.overlap,
    angle = guide$angle,
    n.dodge = guide$n.dodge)
}

#' Grob to draw clean axis
#'
#' Functionality from Charlotte Dawson's excellent package ggprism
#' @param break_positions position of ticks
#' @param break_labels labels at ticks
#' @param axis_position Axis position. Options: `"left"` (default for y axis), `"bottom"` (default for x axis), `"right"`, `"top"`
#' @param theme Complete theme
#' @param check.overlap If `FALSE` (default), overlapping labels not removed. If `TRUE`, overlapping labels are removed (priority is for first, last, and middle labels)
#' @param angle Angle for axis tick labels
#' @param n.dodge Number of rows (for y axis) or columns (for x axis) that labels should span when overlapping. Relevant when `check.overlap = TRUE`
#' @returns Called by `guide_gengrob.clean`
#' @importFrom grid unit unit.c grobWidth grobHeight gTree gList grobWidth grobHeight viewport
#' @importFrom gtable gtable_row gtable_col gtable_width gtable_height
#' @export
draw_axis_clean <- function(break_positions, break_labels, axis_position, theme, check.overlap = FALSE, angle = NULL, n.dodge = 1) {
  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"
  line_element_name <- sprintf("axis.line.%s", aesthetic)
  tick_element_name <- sprintf("axis.ticks.%s", aesthetic)
  tick_length_element_name <- sprintf("axis.ticks.length.%s", aesthetic)
  label_element_name <- sprintf("axis.text.%s", aesthetic)

  line_element <- ggplot2::calc_element(line_element_name, theme)
  tick_element <- ggplot2::calc_element(tick_element_name, theme)
  tick_length <- ggplot2::calc_element(tick_length_element_name, theme)
  label_element <- ggplot2::calc_element(label_element_name, theme)

  # Override rotation of axis tick labels
  if (inherits(label_element, "element_text")) {
    get_text_angle <- function(axis_position, angle) {
      if (is.null(angle)) {
        return(ggplot2::element_text(angle = NULL, hjust = NULL, vjust = NULL))
      }
      if (axis_position == "bottom") {
        hjust <- if (angle > 0) 1 else if (angle < 0) 0 else 0.5
        vjust <- if (abs(angle) == 90) 0.5 else 1
      } else if (axis_position == "left") {
        hjust <- if (abs(angle) == 90) 0.5 else 1
        vjust <- if (angle > 0) 0 else if (angle < 0) 1 else 0.5
      } else if (axis_position == "top") {
        hjust <- if (angle > 0) 0 else if (angle < 0) 1 else 0.5
        vjust <- if (abs(angle) == 90) 0.5 else 0
      } else if (axis_position == "right") {
        hjust <- if (abs(angle) == 90) 0.5 else 0
        vjust <- if (angle > 0) 1 else if (angle < 0) 0 else 0.5
      }
      ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust)
    }
    label_overrides <- get_text_angle(axis_position, angle)
    if (!is.null(label_overrides$angle)) {
      label_element$angle <- label_overrides$angle
    }
    if (!is.null(label_overrides$hjust)) {
      label_element$hjust <- label_overrides$hjust
    }
    if (!is.null(label_overrides$vjust)) {
      label_element$vjust <- label_overrides$vjust
    }
  }

  # Set parameters for x or y axis
  is_vertical <- axis_position %in% c("left", "right")

  if (is_vertical) {
    position_dim <- "y"
    non_position_dim <- "x"
    position_size <- "height"
    non_position_size <- "width"
    gtable_element <- gtable::gtable_row
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobWidth
  } else {
    position_dim <- "x"
    non_position_dim <- "y"
    position_size <- "width"
    non_position_size <- "height"
    gtable_element <- gtable::gtable_col
    measure_gtable <- gtable::gtable_height
    measure_labels_non_pos <- grid::grobHeight
  }

  # Set parameters for primary or secondary axis
  is_second <- axis_position %in% c("right", "top")
  if (is_second) {
    tick_direction <- 1
    non_position_panel <- grid::unit(0, "npc")
    tick_coordinate_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- grid::unit(1, "npc")
    tick_coordinate_order <- c(1, 2)
  }

  # conditionally set the gtable ordering
  labels_first_gtable <- axis_position %in% c("left", "top")

  # set common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
  axis_position_opposite <- opposite_positions[axis_position]
  names(axis_position_opposite) <- NULL
  # Line
  line_grob <- rlang::exec(
    ggplot2::element_grob, line_element,
    # to extend axis to lowest break replace 0 with min(break_positions)
    !!position_dim := grid::unit(c(0, max(break_positions)), "npc"),
    !!non_position_dim := grid::unit.c(non_position_panel, non_position_panel))

  if (n_breaks == 0L) {
    return(grid::gTree(children = grid::gList(line_grob), width = grid::grobWidth(line_grob), height = grid::grobHeight(line_grob), xmin = NULL, ymin = NULL, vp = NULL, cl = "absoluteGrob"))
  }

  # break_labels can be a list of language objects
  if (is.list(break_labels)) {
    break_labels <- if (any(vapply(break_labels, is.language, logical(1)))) {
      do.call(expression, break_labels)
    } else {
      unlist(break_labels)
    }
  }

  # calculate multiple rows/columns of labels
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)
  draw_axis_labels <- function(.break_positions, .break_labels, .label_element, .is_vertical, .check.overlap = FALSE) {
    if (.is_vertical) {
      position_dim <- "y"
      label_margin_name <- "margin_x"
    } else {
      position_dim <- "x"
      label_margin_name <- "margin_y"
    }
    n_breaks <- length(.break_positions)
    .break_positions <- grid::unit(.break_positions, "native")
    if (.check.overlap) {
      priority <- if (n_breaks == 0L) {
        numeric(0)
      } else {
        z <- function(x, y) {
          n <- y - x + 1
          if (n <= 2) return(numeric(0))
          mid <- x - 1 + (n + 1)%/%2
          c(mid, z(x, mid), z(mid, y))
        }
        c(1, n_breaks, z(1, n_breaks))
      }
      .break_labels <- .break_labels[priority]
      .break_positions <- .break_positions[priority]
    }
    labels_grob <- rlang::exec(ggplot2::element_grob, .label_element, `:=`(!!position_dim, .break_positions), `:=`(!!label_margin_name, TRUE), label = .break_labels, check.overlap = .check.overlap)
  }

  # Labels
  label_grobs <- lapply(dodge_indices, function(idx) {
    draw_axis_labels(.break_positions = break_positions[idx], .break_labels = break_labels[idx], .label_element = label_element, .is_vertical = is_vertical, .check.overlap = check.overlap)
  })

  # Ticks
  ticks_grob <- rlang::exec(ggplot2::element_grob, tick_element, !!position_dim := rep(unit(break_positions, "native"), each = 2), !!non_position_dim := rep(grid::unit.c(non_position_panel + (tick_direction*tick_length), non_position_panel)[tick_coordinate_order], times = n_breaks), id.lengths = rep(2, times = n_breaks))

  # gtable
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- do.call(grid::unit.c, lapply(label_grobs, measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- grid::unit.c(tick_length, label_dims)

  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  gt <- rlang::exec(gtable_element, name = "axis", grobs = grobs, !!non_position_sizes := grob_dims, !!position_size := unit(1, "npc"))

  # create viewport
  justvp <- rlang::exec(grid::viewport, !!non_position_dim := non_position_panel, !!non_position_size := measure_gtable(gt), just = axis_position_opposite)
  grid::gTree(children = grid::gList(line_grob, gt), width = gtable::gtable_width(gt), height = gtable::gtable_height(gt), xmin = NULL, ymin = NULL, vp = justvp, cl = "absoluteGrob")
}

#' log10 + 1 axis transformation
#'
#' Function for `coord_trans()` to prevent log(0).  ggplot(...) + coord_trans(y = trans_log10_1)
#' @export
trans_log10_1 <- scales::trans_new(
  name = "trans_log10_1",
  transform = function(x) log10(x + 1),
  inverse = function(x) (10^x) - 1)

#' log10 axis transformation that allows for 0s
#'
#' Function for `coord_trans()` to prevent log(0). ggplot(...) + coord_trans(y = trans_log10_nonzero)
#' @export
trans_log10_nonzero <- scales::trans_new(
  name = "trans_log10_nonzero",
  transform = function(x) ifelse(x == 0, 0, log10(x)),
  inverse = function(x) ifelse(x == 0, 0, 10^x))

#' Rotate x axis labels
#'
#' @param angle Angle for x axis labels. Default is `45`
#' @returns Add to ggplot object just as `theme()` is used
#' @export
rotate_x_labels <- function(angle = 45) {
  theme(axis.text.x = element_text(angle = angle, hjust = 1, vjust = 1))
}
