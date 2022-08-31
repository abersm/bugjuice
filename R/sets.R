#' Elements in x but not y
#'
#' Similar to `base::setdiff` but doesn't convert `x` and `y` to vectors
#' @param x,y Vectors
#' @export
Setdiff <- function(x, y) unique.default(x[match(x, y, 0L) == 0L])

#' Elements in x and y
#'
#' Similar to `base::intersect` but doesn't convert `x` and `y` to vectors
#' @inheritParams Setdiff
#' @export
Intersect <- function(x, y) unique.default(y[match(x, y, 0L)])

#' Setdiff
#'
#' Equivalent to sequentially running `Setdiff` on > 2 vectors
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @export
setdiff_all <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  Reduce(Setdiff, lists)
}

#' Elements common to all input vectors
#'
#' Equivalent to sequentially running `Intersect` on > 2 vectors
#' @param ... Vectors or list of vectors
#' @export
intersect_all <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  Reduce(Intersect, lists)
}

#' Interleave vectors or lists
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param coerce_fn Function applied to each input vector prior to interleaving. Default is as.character
#' @export
interleave <- function(..., coerce_fn = as.character) {
  coerce_fn <- match_fun(coerce_fn)
  z <- lapply(list(...), coerce_fn)
  n_args <- length(z)
  if (n_args < 2) return(c(...))
  max_len <- max(lengths(z), na.rm = TRUE)
  z <- lapply(z, rep, length.out = max_len)
  elements <- unlist(z)
  positions <- as.integer(matrix(seq_along(elements), nrow = n_args, byrow = TRUE))
  elements[positions]
}

#' Recursively determine whether all inputs are identical
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @export
all_identical <- function(...) .lapply_predicate(..., .fn = identical)

#' Recursively determine whether all inputs are equal
#'
#' @rdname all_identical
#' @export
all_equal <- function(...) .lapply_predicate(..., .fn = all.equal)

#' Helper function to generate predicate functions that accept > 2 inputs
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param .fn Predicate function. Must take 2 objects as input to be compared
#' @returns Length 1 logical
#' @noRd
.lapply_predicate <- function(..., .fn) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  z <- lists[[1L]]
  for (i in seq_along(lists)[-1L]) {
    if (!.fn(.subset2(lists, i), z)) return(FALSE)
  }
  TRUE
}
