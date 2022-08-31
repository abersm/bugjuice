#' Determine whether integer is even
#'
#' @param x Integer vector
#' @return Logical vector with length equal to input. If integer is even, output is `TRUE.` If integer is odd, output is `FALSE`
#' @export
is_even <- function(x) x %% 2 == 0

#' Determine whether integer is odd
#'
#' @rdname is_even
#' @return Logical vector with length equal to input. If integer is odd, output is `TRUE.` If integer is even, output is `FALSE`
#' @export
is_odd <- function(x) x %% 2 != 0

#' Determine whether input value is zero
#'
#' @param x Numeric vector
#' @return Logical vector with length equal to input. If `x == 0`, output is `TRUE.` If `x != 0`, output is `FALSE.` If `is.na(x)`, output is `NA`
#' @export
is_zero <- function(x) x == 0

#' Determine whether any values in a vector are equal to zero
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values removed
#' @return Length 1 logical vector
#' @export
any_zero <- function(x, na.rm = TRUE) any(x == 0, na.rm = na.rm)

#' Determine whether a vector is a binary variable
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are removed
#' @return Logical vector of length 1
#' @export
is_binary <- function(x, na.rm = TRUE) n_unique(x, na.rm = na.rm) == 2L

#' Determine whether a vector is not a binary variable
#'
#' @rdname is_binary
#' @export
is_not_binary <- function(x, na.rm = TRUE) !is_binary(x, na.rm = na.rm)

#' Determine whether a vector consists exclusively of 0 and 1
#'
#' @rdname is_binary
#' @export
is_binary_01 <- function(x, na.rm = TRUE) {
  allowed_values <- if (na.rm) c(0, 1, NA_real_) else c(0, 1)
  if (is_not_binary(x, na.rm)) return(FALSE)
  if (inherits(x, "factor") && all(attr(x, "levels") %in% allowed_values)) return(TRUE)
  x <- suppressWarnings(as.numeric(x))
  is_binary(x, na.rm) && all(x %in% allowed_values)
}

#' Convert binary variables to integer coded as 0 or 1
#'
#' @param x Vector
#' @returns Integer vector coded as 0 or 1 with length equal to input
#' @export
as_binary_01 <- function(x) {
  if (!is_binary(x)) stop("Input to as_binary_01() is not a binary variable", call. = FALSE)
  x_levels <- levels(x) %||% create_levels(x)
  match(x, x_levels, incomparables = NA_integer_) - 1L
}

#' Determine whether numeric values are nearly equivalent to integers
#'
#' @rdname is_zero
#' @returns Logical vector with length equal to input
#' @export
is_integerish <- function(x) floor(x) == x

#' Determine whether a numeric vector is continuous
#'
#' @rdname is_zero
#' @export
is_continuous <- function(x) is.numeric(x) && !is.integer(x)

#' Alias for anyNA
#'
#' @rdname is_zero
#' @export
any_na <- function(x) anyNA(x)

#' Check if vector contains only missing values
#'
#' @rdname is_zero
#' @export
all_na <- function(x) all(is.na(x))

#' Determine whether values are included in a specified range
#'
#' @param x Numeric vector
#' @param min,max Lower and upper bound of values respectively
#' @param include_min,include_max If `TRUE`, include values equal to min or max, respectively. Both `TRUE` by default
#' @returns Logical with length and order same as input. `NA` values in `x` will return `NA` in output
#' @export
is_between <- function(x, min, max, include_min = TRUE, include_max = TRUE) {
  # findInterval(x, c(min, max)) == 1L
  above_min <- if (include_min) x >= min else x > min
  below_max <- if (include_max) x <= max else x < max
  above_min & below_max
}

#' Determine number of unique values for vector
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values removed
#' @returns Length 1 integer vector
#' @export
n_unique <- function(x, na.rm = TRUE) {
  x <- if (inherits(x, "factor")) attr(x, "levels") else unique.default(x)
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  length(x)
}

#' Counts for each unique value
#'
#' @param x Vector or data frame
#' @param ... Columns in `x`. Enter using tidyselect syntax. Ignored if `x` is a vector
#' @returns If input is a vector, out is a named integer vector containing number of observations per level for a categorical variable. Name of values refers to the level. Listed in decreasing order of group size with NAs listed last. If input is a data frame, output is a data frame containing columns for variable, group (unique value for variable), Freq (number of rows in `x` in which variable = group). Similar to output from `dplyr::count`
#' @export
n_per_group <- function(x, ...) {
  if (is.data.frame(x)) {
    if (n_dots(...) > 0L) {
      x <- dplyr::select(x, ...)
    }
    purrr::map2_df(x, names(x), function(.x, .y) {
      x_unique <- if (anyNA(.x)) {
        idx_na <- is.na(.x)
        .x <- .x[!idx_na]
        x_unique <- unique.default(.x)
        out <- tabulate(match(.x, x_unique))
        names(out) <- x_unique
        c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
      } else {
        x_unique <- unique.default(.x)
        out <- tabulate(match(.x, x_unique))
        names(out) <- x_unique
        sort.int(out, method = "quick", decreasing = TRUE)
      }
      list(variable = .y, group = names(x_unique), Freq = as.integer(x_unique))
    })
  } else {
    if (anyNA(x)) {
      idx_na <- is.na(x)
      x <- x[!idx_na]
      x_unique <- unique.default(x)
      out <- tabulate(match(x, x_unique))
      names(out) <- x_unique
      c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
    } else {
      x_unique <- unique.default(x)
      out <- tabulate(match(x, x_unique))
      names(out) <- x_unique
      sort.int(out, method = "quick", decreasing = TRUE)
    }
  }
}

#' Mode of a vector
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are removed prior to determining mode
#' @returns Value(s) in vector that occur most frequently. Class matches input class
#' @export
Mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_unique <- unique.default(x)
  x_unique[which.max(tabulate(match(x, x_unique)))]
}

#' Count the number of times a value appears in a vector
#'
#' @param x Vector
#' @param ... Value(s) to count occurrence in `x`
#' @returns Number of times value appears in `x`
#' @export
count_any_of <- function(x, ...) sum(x %in% c(...), na.rm = TRUE)

#' Count the number of values in a vector other than a predefined subset
#'
#' @rdname count_any_of
#' @param ... Value(s) to exclude from `x`
#' @returns Number of times values other than those included in values argument appear in `x`
#' @export
count_none_of <- function(x, ...) sum(x %!in% c(...), na.rm = TRUE)

#' Determine whether vector contains any duplicate values
#'
#' @param x Vector
#' @returns Length 1 logical
#' @export
any_dupes <- function(x) anyDuplicated(x) > 0L

#' Determine whether all values in a vector are unique
#'
#' @rdname any_dupes
#' @export
all_unique <- function(x) anyDuplicated(x) == 0L

#' Include the first occurrence of the subsequently duplicated value in duplicated output
#'
#' @rdname dupe_vals
#' @returns Logical vector in which the index of any value that is duplicated is `TRUE.` Unique values are `FALSE`
#' @export
duplicated_incl_first <- function(x) {
  duplicated.default(x, fromLast = FALSE) | duplicated.default(x, fromLast = TRUE)
}

#' Extract duplicated entries in a vector
#'
#' @param x Vector
#' @returns Values in `x` that are duplicated
#' @export
dupe_vals <- function(x) unique.default(x[duplicated(x)])

# Trend over time ---------------------------------------------------------

#' Determine number/percent of increasing or decreasing between adjacent values
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values are removed
#' @returns List containing n_deltas, n_plateaus, n_increase, n_decreases
#' @export
trend_summary <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  list(
    n_deltas = n - 1L,
    n_plateaus = sum(x_diff == 0),
    n_increases = sum(x_diff > 0),
    n_decreases = sum(x_diff < 0)
  )
}

#' Determine if a variable is constant (all values identical)
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are removed
#' @returns Length 1 logical vector
#' @export
is_constant <- function(x, na.rm = TRUE) n_unique(x, na.rm = na.rm) == 1L

#' Determine whether a numeric vector is only increasing (increases monotonically)
#'
#' @param x Numeric vector
#' @param incl_plateau If `TRUE` (default), output can still be `TRUE` even if `x` contains instances in which adjacent values. If `FALSE`, values must be strictly monotonically increasing
#' @param na.rm If `TRUE` (default), missing values are removed
#' @returns Length 1 logical. `TRUE` if each successive value is greater than the previous value. If `incl_plateau` is `TRUE`, output can still be `TRUE` even if `x` contains instances in which adjacent values
#' @export
only_increasing <- function(x, incl_plateau = TRUE, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  if (incl_plateau) all(x_diff >= 0L, na.rm = TRUE) else all(x_diff > 0L, na.rm = TRUE)
}

#' Determine whether a numeric vector contains any increases
#'
#' @rdname only_increasing
#' @returns Length 1 logical. `TRUE` if any occurrences of a value greater than the previous value
#' @export
any_increasing <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  any(x_diff > 0L, na.rm = TRUE)
}

#' Determine whether a numeric vector is only decreasing (decreases monotonically)
#'
#' @param x Numeric vector
#' @param incl_plateau If `TRUE` (default), output can still be `TRUE` even if `x` contains instances in which adjacent values. If `FALSE`, values must be strictly monotonically decreasing
#' @param na.rm If `TRUE` (default), missing values are removed
#' @returns Logical. `TRUE` if each successive value is less than the previous value. If `incl_plateau` is `TRUE`, output can still be `TRUE` even if `x` contains instances in which adjacent values
#' @export
only_decreasing <- function(x, incl_plateau = TRUE, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  if (incl_plateau) all(x_diff <= 0L, na.rm = TRUE) else all(x_diff < 0L, na.rm = TRUE)
}

#' Determine whether a numeric vector contains any decreases
#'
#' @rdname only_decreasing
#' @returns Length 1 logical. `TRUE` if any occurrences of a value less than the previous value
#' @export
any_decreasing <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  any(x_diff < 0L, na.rm = TRUE)
}

#' Extract a subset of a vector, list, or data frame, either by position or names
#'
#' @param x Vector, list, or data frame
#' @param idx Either a numeric vector of positions or character vector of names to extract
#' @param depth Number of brackets. Options: `1`, `2`. Default is `1`
#' @returns Subset of `x`
#' @export
extract <- function(x, idx = 1, depth = 1) {
  if (depth == 1 || length(idx) > 1) .subset(x, idx) else .subset2(x, idx)
}

#' Reverse a vector
#'
#' Equivalent to `rev.default`
#' @param x Vector
#' @returns Vector with length and class identical to input
#' @export
Rev <- function(x) {
  n <- length(x)
  if (n == 0L) x else x[n:1L]
}
