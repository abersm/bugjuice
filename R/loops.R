# Silent version of map functions -----------------------------------------

#' Silent version of lapply/map
#'
#' @param .x Input to mapping function
#' @param .f Function to apply to each element of `.x`
#' @param ... Arguments passed to `.f`
#' @param .otherwise Value to be returned if output of `.f` is an error. Default is `NULL`
#' @param .remove_null If `TRUE`, `NULL` values are removed from output
#' @export
try_map <- function(.x, .f, ..., .otherwise = NULL, .remove_null = FALSE) {
  out <- if (is.function(.f)) {
    lapply(.x, function(x) tryCatch(.f(x, ...), error = function(e) .otherwise))
  } else {
    lapply(.x, function(x) tryCatch(do.call(`[[`, list(x, .f, ...)), error = function(e) .otherwise))
  }
  if (.remove_null) remove_null(out) else out
}

#' try_map that suppresses warnings
#'
#' @inheritParams try_map
#' @export
try_map_silent <- function(.x, .f, ..., .otherwise = NULL, .remove_null = FALSE) {
  out <- if (is.function(.f)) {
    lapply(.x, function(x) tryCatch(suppressWarnings(.f(x, ...)), error = function(e) .otherwise))
  } else {
    lapply(.x, function(x) tryCatch(suppressWarnings(do.call(`[[`, list(x, .f, ...))), error = function(e) .otherwise))
  }
  if (.remove_null) remove_null(out) else out
}

#' Silent version of vapply
#'
#' @param X,FUN,...,FUN.VALUE,USE.NAMES Arguments passed to vapply
#' @param .otherwise Value to be returned if output of `.f` is an error
#' @noRd
.try_vapply <- function(X, FUN, ..., FUN.VALUE, USE.NAMES, .otherwise) {
  out <- if (is.function(FUN)) {
    vapply(X = X, FUN = function(x) tryCatch(FUN(x, ...), error = function(e) .otherwise), FUN.VALUE = FUN.VALUE, USE.NAMES = FALSE)
  } else {
    vapply(X = X, FUN = function(x) tryCatch(do.call(`[[`, list(x, FUN, ...)), error = function(e) .otherwise), FUN.VALUE = FUN.VALUE, USE.NAMES = FALSE)
  }
  if (.use_names) {
    names(out) <- names(X)
  }
  out
}

#' Silent version of vapply for character output (or map_chr)
#'
#' @inheritParams try_map
#' @param .use_names If `TRUE`, names of `.x` are applied to names of output
#' @export
try_map_chr <- function(.x, .f, ..., .otherwise = NA_character_, .use_names = FALSE) {
  .try_vapply(X = .x, FUN = .f, ..., FUN.VALUE = character(1), USE.NAMES = .use_names, .otherwise = .otherwise)
}

#' Silent version of vapply for numeric output (or map_dbl)
#'
#' @inheritParams try_map_chr
#' @export
try_map_dbl <- function(.x, .f, ..., .otherwise = NA_real_, .use_names = FALSE) {
  .try_vapply(X = .x, FUN = .f, ..., FUN.VALUE = numeric(1), USE.NAMES = .use_names, .otherwise = .otherwise)
}

#' Silent version of vapply for integer output (or map_int)
#'
#' @inheritParams try_map_chr
#' @export
try_map_int <- function(.x, .f, ..., .otherwise = NA_integer_, .use_names = FALSE) {
  .try_vapply(X = .x, FUN = .f, ..., FUN.VALUE = integer(1), USE.NAMES = .use_names, .otherwise = .otherwise)
}

#' Silent version of vapply for logical output (or map_lgl)
#'
#' @inheritParams try_map_chr
#' @export
try_map_lgl <- function(.x, .f, ..., .otherwise = NA_integer_, .use_names = FALSE) {
  .try_vapply(X = .x, FUN = .f, ..., FUN.VALUE = logical(1), USE.NAMES = .use_names, .otherwise = .otherwise)
}

#' Silent version of map_dfr
#'
#' @inheritParams try_map
#' @param .prefix Prefix to use for new column names. Default is `"V"`. Enter as quoted prefix
#' @export
try_map_dfr <- function(.x, .f, ..., .otherwise = NULL, .prefix = "V") {
  out <- lapply(.x, function(x) tryCatch(.f(x, ...), error = function(e) .otherwise))
  list_to_df_rowwise(remove_null(out), prefix = .prefix)
}

#' Silent version of map_dfc
#'
#' @inheritParams try_map
#' @export
try_map_dfc <- function(.x, .f, ..., .otherwise = NULL) {
  out <- lapply(.x, function(x) tryCatch(.f(x, ...), error = function(e) .otherwise))
  list_to_df(out)
}

# Silent version of map2 functions ----------------------------------------

#' Silent version of mapply/map2
#'
#' @param .x,.y Input to mapping function
#' @param .f Function to apply to each element of `.x`
#' @param ... Arguments passed to `.f`
#' @param .otherwise Value to be returned if output of `.f` is an error. Default is `NULL`
#' @param .remove_null If `TRUE`, `NULL` values are removed from output
#' @param .use_names If `TRUE`, output can be named
#' @export
try_map2 <- function(.x, .y, .f, ..., .otherwise = NULL, .remove_null = FALSE, .use_names = FALSE) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = .use_names)
  if (.remove_null) remove_null(out) else out
}

#' Silent version of map2_chr
#'
#' @inheritParams try_map2
#' @export
try_map2_chr <- function(.x, .y, .f, ..., .otherwise = NA_character_) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  as.vector(out, "character")
}

#' Silent version of map2_dbl
#'
#' @inheritParams try_map2_chr
#' @export
try_map2_dbl <- function(.x, .y, .f, ..., .otherwise = NA_real_) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  as.vector(out, "double")
}

#' Silent version of map2_int
#'
#' @inheritParams try_map2_chr
#' @export
try_map2_int <- function(.x, .y, .f, ..., .otherwise = NA_integer_) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  as.vector(out, "integer")
}

#' Silent version of map2_lgl
#'
#' @inheritParams try_map2_chr
#' @export
try_map2_lgl <- function(.x, .y, .f, ..., .otherwise = NA) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  as.vector(out, "logical")
}

#' Silent version of map2_dfr
#'
#' @inheritParams try_map2
#' @param .prefix Prefix for column names. Enter as quoted prefix. Default is `"V"`
#' @export
try_map2_dfr <- function(.x, .y, .f, ..., .otherwise = NULL, .prefix = "V") {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  list_to_df_rowwise(remove_null(out), prefix = .prefix)
}

#' Silent version of map2_dfc
#'
#' @inheritParams try_map2
#' @export
try_map2_dfc <- function(.x, .y, .f, ..., .otherwise = NULL) {
  out <- mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  list_to_df(out)
}

# Silent version of walk functions ----------------------------------------

#' Silent version of walk
#'
#' @inheritParams try_map
#' @export
try_walk <- function(.x, .f, ..., .otherwise = NULL) {
  lapply(.x, function(x) tryCatch(.f(x, ...), error = function(e) .otherwise))
  invisible(.x)
}

#' Silent version of walk2
#'
#' @inheritParams try_map2
#' @export
try_walk2 <- function(.x, .y, .f, ..., .otherwise = NULL) {
  mapply(function(x, y) tryCatch(.f(x, y, ...), error = function(e) .otherwise), x = .x, y = .y, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(.x)
}

#' Element-wise function
#'
#' @param .list List of vectors passed to `.f` to be evaluated element-wise
#' @param .f Function to be applied to input vectors
#' @export
elementwise <- function(.list, .f) {
  list_lengths <- lengths(.list)
  unique_lengths <- unique.default(list_lengths)
  n_unique_lengths <- length(unique_lengths)
  if (n_unique_lengths > 1L) {
    if (n_unique_lengths == 2L && any(unique_lengths == 1L)) {
      unique_lengths <- max(unique_lengths)
      .list <- lapply(.list, rep, length.out = unique_lengths)
    } else {
      stop("Inputs to '.list' argument of elementwise() must consist of vectors, each with the same length", call. = FALSE)
    }
  }
  transposed_list <- purrr::transpose(.list)
  #transposed_list <- do.call(Map, c(c, .list, USE.NAMES = FALSE))
  lapply(transposed_list, function(x) do.call(.f, x))
}
