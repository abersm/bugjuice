# Create factor -----------------------------------------------------------

#' Create reasonable levels for factor regardless of input variable class
#'
#' @param x Variable to converted to factor
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default
#' @param droplevels If `TRUE` (default) and `x` is a factor, unused levels are dropped
#' @returns Character vector containing factor levels. Enter as `factor(x, levels = create_levels(x))`
#' @export
create_levels <- function(x, reverse = FALSE, droplevels = TRUE) {
  if (is.factor(x)) {
    levels <- attr(x, "levels")
    if (droplevels) {
      x <- factor(x, exclude = if (anyNA(levels)) NULL else NA)
      levels <- attr(x, "levels")
    }
  } else if (is.logical(x)) {
    levels <- c(TRUE, FALSE)
  } else if (is.numeric(x)) {
    levels <- sort.int(unique.default(x), method = "quick")
  } else {
    levels <- sort.int(unique.default(x), method = "quick")
  }
  if (reverse) {
    Rev(levels)
  } else {
    levels
  }
}

#' Convert severity to a factor variable
#'
#' @param x Character vector
#' @param levels Severity levels from least to most severe. Enter as character vector
#' @export
fct_severity <- function(x, levels = NULL) {
  if (is.null(levels)) {
    x_unique <- unique.default(x)
    levels <- tolower(x_unique)
    levels <- dplyr::case_when(
      levels == "mild" ~ 1L,
      grepl("mild", levels, fixed = TRUE) ~ 2L,
      levels == "moderate" ~ 3L,
      grepl("moderate|medium", levels) ~ 4L,
      levels == "severe" ~ 5L,
      grepl("severe|very", levels) ~ 6L,
      levels == "critical" ~ 7L,
      levels %in% c("deceased", "death", "dead") ~ 8L,
      !is.na(levels) ~ 9L,
      TRUE ~ 10L)
    names(levels) <- x_unique
    names(sort.int(levels))
  }
  factor(x, levels = levels)
}

# Type conversion ---------------------------------------------------------

#' Convert categorical variable to factor encoded as integer
#'
#' Similar to as.integer(factor(x)) but faster
#' @param x Factor, character, numeric, or logical vector
#' @param levels Order of levels in `x`
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default
#' @returns Integer version of `x` (lowest level is 1, highest value is equal to the number of levels)
as_numeric_factor <- function(x, levels = NULL, reverse = FALSE) {
  x_levels <- levels %||% create_levels(x)
  if (reverse) {
    x_levels <- Rev(x_levels)
  }
  match(x, x_levels, incomparables = NA_integer_)
}

#' Alias for `as_numeric_factor()`
#'
#' @rdname as_numeric_factor
#' @export
fct_to_num <- as_numeric_factor

#' Convert vector to factor
#'
#' @rdname create_levels
#' @export
as_fct <- function(x, reverse = FALSE, droplevels = FALSE) {
  factor(x, levels = create_levels(x, reverse = reverse, droplevels = droplevels))
}

# Edit levels -------------------------------------------------------------

#' Reorder levels of a factor
#'
#' Functionality from forcats package
#' @param .x Factor vector to be reordered
#' @param .reorder_by Vector used to determine new order of `.x` levels. Levels of `.x` will be determined according to `.fn(.reorder_by)` calculation
#' @param .fn Function applied to reorder_by to determine order of `.x` levels. Default is `Mean`
#' @param ... Arguments passed to `.fn`
#' @param .increasing If `TRUE` (default), levels of `.x` will be in ascending order of `.fn(.reorder_by)` calculation. If `FALSE`, levels of `.x` will be in descending order of `.fn(.reorder_by)` calculation
#' @returns Factor with updated levels according to `.fn(.reorder_by)` calculation and increasing argument
#' @export
fct_reorder <- function(.x, .reorder_by, .fn = Mean, ..., .increasing = TRUE) {
  if (!inherits(.x, "factor")) {
    .x <- factor(.x)
  }
  x_updated <- tapply(.reorder_by, .x, .fn, ...)
  x_updated <- factor(.x, levels = attr(.x, "levels")[order(x_updated, decreasing = !.increasing)], exclude = NULL)
  attributes(x_updated) <- update_list(attributes(.x), attributes(x_updated))
  x_updated
}

#' Order levels of a factor by their order of appearance
#'
#' @rdname fct_reorder
#' @export
fct_by_input_order <- function(.x) {
  if (is.character(.x)) {
    .x <- factor(.x)
  }
  idx <- as.integer(.x)[!duplicated(.x)]
  idx <- idx[!is.na(idx)]
  new_f <- factor(.x, levels = attr(.x, "levels")[idx], exclude = NULL)
  attributes(new_f) <- update_list(attributes(.x), attributes(new_f))
  new_f
}

#' Order levels of a factor by their relative frequency (most to least common)
#'
#' @rdname fct_reorder
#' @export
fct_by_count <- function(.x, .increasing = TRUE) {
  if (is.character(.x)) {
    .x <- factor(.x)
  }
  idx <- order(table(.x), decreasing = !.increasing)
  new_f <- factor(.x, levels = attr(.x, "levels")[idx], exclude = NULL)
  attributes(new_f) <- update_list(attributes(.x), attributes(new_f))
  new_f
}

#' Order levels of a factor from least to most common
#'
#' @rdname fct_reorder
#' @export
fct_increasing <- function(.x) fct_by_count(.x, .increasing = TRUE)

#' Order levels of a factor from most to least common
#'
#' @rdname fct_reorder
#' @export
fct_decreasing <-function(.x) fct_by_count(.x, .increasing = FALSE)

# Info --------------------------------------------------------------------

#' Determine whether a variable is likely a categorical variable
#'
#' @param x Vector
#' @param max_n_unique If number of `n_unique(x) <= max_n_unique`, variable is considered a factor
#' @returns If `x` is likely a categorical variable, `TRUE.` Otherwise, `FALSE`
#' @export
is_categorical <- function(x, max_n_unique = 5L) {
  inherits(x, c("factor", "ordered", "logical", "character", "integer", "labelled")) || n_unique(x) <= max_n_unique
}
