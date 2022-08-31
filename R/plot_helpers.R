# Scale functions ---------------------------------------------------------

#' Censor function
#'
#' Functionality from scales package
#' @param x Vector
#' @param ... Not used
#' @returns Input
#' @export
rescale_none <- function(x, ...) x

#' New fill scale
#'
#' Functionality from ggnewscale package
#' @returns Called directly by user
#' @export
scale_fill_new <- function() {
  #structure(ggplot2::standardise_aes_names("fill"), class = "new_color")
  structure("fill", class = "new_color")
}

#' New color scale
#'
#' Functionality from ggnewscale package
#' @rdname scale_fill_new
#' @export
scale_color_new <- function() {
  #structure(ggplot2::standardise_aes_names("colour"), class = "new_color")
  structure("colour", class = "new_color")
}

#' Add new color scale
#'
#' Must be exported to work correctly
#' @param object New scale to add
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns Not called directly by user
#' @export
ggplot_add.new_color <- function(object, plot, object_name) {
  if (is.null(plot$scales$get_scales(object))) {
    plot$scales <- ggplot2::ggplot_build(plot)$plot$scales
  }
  z <- gsub(pattern = "(_new)*", replacement = "", x = names(plot$mapping), fixed = FALSE)
  old_aes <- names(plot$mapping)[z %in% object]
  new_aes <- paste0(old_aes, "_new")
  names(plot$mapping)[names(plot$mapping) == old_aes] <- new_aes
  plot$layers <- lapply(plot$layers, function(x) {
    original_aes <- object
    new_layer <- ggplot2::ggproto(NULL, x)
    names_mapping <- names(new_layer$mapping)
    z <- gsub(pattern = "(_new)*", replacement = "", x = names(new_layer$mapping), fixed = FALSE)
    old_aes <- names_mapping[z %in% object]
    if (length(old_aes) == 0) {
      names_stat <- names(new_layer$stat$default_aes)
      z <- gsub(pattern = "(_new)*", replacement = "", x = names_stat, fixed = FALSE)
      old_aes <- names_stat[z %in% object]
      if (length(old_aes) == 0) {
        names_geom <- names(new_layer$geom$default_aes)
        z <- gsub(pattern = "(_new)*", replacement = "", x = names_geom, fixed = FALSE)
        old_aes <- names_geom[z %in% object]
      }
    }
    new_aes <- paste0(old_aes, "_new")
    old_geom <- new_layer$geom
    old_handle_na <- old_geom$handle_na
    change_name <- function(list, old, new) {
      if (is.null(list)) {
        NULL
      } else if (is.character(list)) {
        list[list %in% old] <- new
        list
      } else {
        name <- names(list)
        name[name %in% old] <- new
        names(list) <- name
        list
      }
    }
    new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom, handle_na = function(self, data, params) {
      names(data)[names(data) %in% new_aes] <- original_aes
      old_handle_na(data, params)
    })
    new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
    new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
    new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
    new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
    draw_key <- new_geom$draw_key
    new_draw_key <- function(data, params, size) {
      names(data)[names(data) == new_aes] <- original_aes
      draw_key(data, params, size)
    }
    new_geom$draw_key <- new_draw_key
    new_layer$geom <- new_geom
    old_stat <- new_layer$stat
    parent <- if (!is.null(old_stat$is_new)) old_stat$super() else ggplot2::ggproto(NULL, old_stat)
    new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1L]), parent, setup_data = function(self, data, scales, ...) {
      names(data)[names(data) %in% new_aes] <- original_aes
      data <- ggplot2::ggproto_parent(self$super(), self)$setup_data(data, scales, ...)
      names(data)[names(data) %in% original_aes] <- new_aes
      data
    }, handle_na = function(self, data, params) {
      names(data)[names(data) %in% new_aes] <- original_aes
      ggplot2::ggproto_parent(self$super(), self)$handle_na(data, params)
    }, is_new = TRUE)
    new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
    new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
    new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
    new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
    new_layer$stat <- new_stat
    new_layer$mapping <- change_name(new_layer$mapping, old_aes, new_aes)
    new_layer$aes_params <- change_name(new_layer$aes_params, old_aes, new_aes)
    new_layer
  })
  plot$scales$scales <- lapply(plot$scales$scales, function(x) {
    z <- x$aesthetics
    old_aes <- gsub(pattern = "(_new)*", replacement = "", x = z, fixed = FALSE)
    old_aes <- z[old_aes %in% object]
    if (length(old_aes) != 0) {
      z <- paste0(old_aes, "_new")
      x$aesthetics[x$aesthetics %in% old_aes] <- z
      no_guide <- x$guide == "none" || !(is.logical(x$guide) && length(x$guide) == 1L && !is.na(x$guide) && !x$guide)
      if (!no_guide) {
        if (is.character(x$guide)) {
          x$guide <- get(paste0("guide_", x$guide), mode = "function")()
        }
        x$guide$available_aes[x$guide$available_aes %in% old_aes] <- z
      }
    }
    x
  })
  z <- gsub(pattern = "(_new)*", replacement = "", x = names(plot$labels), fixed = FALSE)
  old_aes <-  names(plot$labels)[z %in% object]
  names(plot$labels)[names(plot$labels) %in% old_aes] <- paste0(old_aes, "_new")
  plot
}

# Data formatting ---------------------------------------------------------

#' Create x and y variables for plotting
#'
#' @param .df Data frame
#' @param .formula y ~ x. Prioritized over `.y` and `.x` if .formula and `.y` and `.x` are provided
#' @param .y,.x Variables forming y and x axis respectively. Enter as quoted or unquoted variable names. Only relevant if formula is missing
#' @param .vars_remove_na Variables other than `.x` and `.y` from which missing values should be removed. Enter as character vector
#' @returns Input data frame with new columns "x" and "y". Rows with missing values for "x", "y", and `.vars_remove_na` are removed
#' @noRd
.create_plot_df <- function(.df, .formula = NULL, .y = NULL, .x = NULL, .vars_remove_na = NULL) {
  vars <- formula2vars(formula = .formula, x = .x, y = .y)
  .df$y <- .df[[vars$y]]
  .df$x <- .df[[vars$x]]
  .vars_remove_na <- Intersect(c("x", "y", .vars_remove_na), names(.df))
  .df[complete.cases(.df[, .vars_remove_na]), ]
}

#' Create a categorical vector to be entered as column in data frame used for plotting
#'
#' @param df Data frame
#' @param var Variable to create or convert to factor. Enter as quoted variable name
#' @param if_null Character vector to return if `var` does not refer to a column in `df`
#' @param as_fct If `TRUE` (default), output is coerced to a factor vector
#' @param levels Levels to use in newly generated factor variable. Only relevant when `as_fct = TRUE`
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default. Only relevant when `as_fct = TRUE`
#' @param droplevels If `TRUE` (default) and input is a factor, unused levels are dropped
#' @returns Character or factor vector with length equal to `nrow(df)`
#' @noRd
.new_cat_var <- function(df, var = NULL, if_null = "a", as_fct = TRUE, levels = NULL, reverse = FALSE, droplevels = TRUE) {
  var <- var %||% "1"
  vals <- .subset2(df, var) %||% if_null
  if (as_fct) {
    if (is.null(levels)) {
      levels <- create_levels(vals, reverse = reverse, droplevels = droplevels)
    } else if (reverse) {
      levels <- Rev(levels)
    }
    factor(vals, levels = levels)
  } else {
    vals
  }
}

# Other -------------------------------------------------------------------

#' Golden ratio
#'
.phi <- 0.5 + sqrt(5)/2

#' Parse expressions safely
#'
#' @param x Character vector
#' @returns Expression vector
#' @noRd
parse_safe <- function(x) {
  stopifnot(is.character(x))
  out <- vector(mode = "expression", length = length(x))
  for (i in seq_along(x)) {
    expr <- parse(text = x[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

#' Automatically determine whether log transformation is needed
#'
#' @param x Numeric vector of y values
#' @param breaks_fn Function used to generate breaks. Default is `pretty`
#' @param n Number of desired breaks. Default is `4`
#' @param min_log_breaks Minimum number of breaks tolerable on log scale. Default is `4`
#' @returns Logical vector of length 1
#' @noRd
prefer_log <- function(x, breaks_fn = pretty, n = 4, min_log_breaks = 4) {
  !length(.create_axis_breaks(.limits = Range(x), .scale = "log", .n = n)) < min_log_breaks
}

#' Check a vector for zero range
#'
#' Functionality from `scales::zero_range()`
#' @param rng Range of numeric vector. Must have length of 1 or 2
#' @param tol Tolerance for 0
#' @returns Logical of length 1
#' @noRd
has_zero_range <- function(rng, tol = 1000*.Machine$double.eps) {
  if (length(rng) == 1) return(TRUE)
  if (anyNA(rng)) return(NA)
  if (rng[1] == rng[2]) return(TRUE)
  if (all(is.infinite(rng))) return(FALSE)
  m <- min(abs(rng))
  if (m == 0) return(FALSE)
  abs((rng[1] - rng[2])/m) < tol
}
