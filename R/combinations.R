#' Crossing for 2 or more vectors
#'
#' Functionality from `base::expand.grid`
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param .prefix_colnames Prefix for columns names. Default is `"V"`
#' @returns Data frame with 1 column per vector entered into `...` and each row containing unique combinations of 1 element per vector
#' @export
crossings <- function(..., .prefix_colnames = "V") {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  n_lists <- length(lists)
  out <- vector("list", n_lists)
  idx <- seq_len(n_lists)
  names(out) <- paste0(.prefix_colnames, idx)
  h <- 1L
  z <- lengths(lists)
  n_total <- prod(z)
  if (n_total == 0L) {
    for (i in idx) {
      out[[i]] <- .subset(.subset2(lists, i), FALSE)
    }
  } else {
    for (i in idx) {
      l <- .subset2(lists, i)
      n_l <- length(l)
      n_total <- n_total/n_l
      l <- l[rep.int(rep.int(seq_len(n_l), rep.int(h, n_l)), n_total)]
      out[[i]] <- l
      h <- h*n_l
    }
  }
  structure(out, class = "data.frame", row.names = seq_len(prod(z)))
}

#' List of combinations
#'
#' Functionality from `utils::combn`
#' @param x Vector. Number of elements must be > `n`
#' @param n Number of elements per group. Must be > 0. Default is `2` (pairs of elements in `x`)
#' @param na.rm If TRUE (default), missing values are removed from x prior to generating combinations
#' @returns List of vectors (each of length `n`) containing unique `n` element combos of values in `x`
#' @export
combos_list <- function(x, n = 2, na.rm = TRUE)  {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_len <- length(x)
  if (x_len < n) {
    message(sprintf("Input 'x' to combos_list() must have length > 'n'. Currently, length of 'x' and 'n' are %s and %, respectively", x_len, n))
    return(NULL)
  }
  e <- 0
  h <- n
  a <- seq_len(n)
  r <- x[a]
  out <- vector("list", choose(n = x_len, k = n))
  out[[1L]] <- r
  i <- 2L
  z <- x_len - n + 1L
  while (a[1L] != z) {
    if (e < x_len - h) {
      h <- 1L
      e <- a[n]
      j <- 1L
    } else {
      e <- a[n - h]
      h <- h + 1L
      j <- 1L:h
    }
    a[n - h + j] <- e + j
    r <- x[a]
    out[[i]] <- r
    i <- i + 1L
  }
  out
}

#' Extract from vector a list of length 2 vectors containing pairwise combinations
#'
#' @param x Vector with elements to be combined pairwise
#' @param na.rm If `TRUE` (default), missing values are removed from x prior to generating combinations
#' @returns List of length 2 vectors
#' @export
pairs_list <- function(x, na.rm = TRUE) combos_list(x = x, n = 2, na.rm = na.rm)

#' Generate all combinations of any number of input vectors
#'
#' @param ... Enter 1 or more vectors
#' @param n Number of elements from `x` per combination. Default is `2`. Only relevant input is a single vector
#' @param col_names Character vector containing names of columns to be generated
#' @param na.rm If `TRUE` (default), missing values are removed from vectors prior to generating combinations. Only relevant if 1 vector is supplied
#' @returns Data frame of combinations. If 1 vector, output is a data frame with 1 row for each unique combination of elements taken n at a time. Pairs of elements not repeated (i.e. if a row of output is a data frame contains a in V1 and B in V2 there will be no row with b in V1 and a in V2) entered. If > 1 vector entered, output consists of a data frame containing 1 column for each input vector and 1 row for each possible combination of values across vectors
#' @export
combos <- function(..., n = 2, col_names = paste0("V", seq_len(n)), na.rm = TRUE) {
  if (n_dots(...) > 1 || is.list(c(...))) {
    crossings(...)
  } else {
    .combos_1_vec_as_df(..., n = n, col_names = col_names, na.rm = na.rm)
  }
}

#' Pairwise combinations of elements from 1 vector
#'
#' Wrapper around combos with n = 2
#' @param x Vector with elements to be combined pairwise
#' @param col_names Character vector containing names of columns to be generated
#' @param na.rm If `TRUE` (default), missing values are removed from `x` prior to generating combinations
#' @returns Data frame with 2 columns that contains unique pairs of elements in each row
#' @export
pairs <- function(x, col_names = c("V1", "V2"), na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  n1 <- n - 1
  k <- n*n1/2
  idx <- seq_len(n1)
  y <- array(rep(idx, rev(idx)), dim = c(k, 2))
  for (i in idx) {
    ni <- n - i
    z <- k - (ni + 1)*ni/2
    y[(z + 1):(z + ni), 2] <- (i + 1):n
  }
  df <- matrix_to_df(array(x[y], dim = c(k, 2)))
  names(df) <- col_names
  df
}

#' Combine multiple vectors by matching positions into list of element-wise combinations
#'
#' Matching elements from each vector paired (element 1 from vector 1 paired with element 1 from vector 2)
#' @param ... Vectors. All must have same length
#' @returns List of vectors (each with length equal to the number of vectors entered) containing element-wise combinations (i.e. 1st element in each vector in 1st element of output list, 2nd element in each vector in 2nd element of output list, etc.)
#' @export
matched_combos <- function(...) {
  terms_combined <- list(...)
  purrr::pmap(terms_combined, c)
}

#' Alias for matched_combos
#'
#' @rdname matched_combos
#' @export
matched_pairs <- matched_combos

# Combination helpers -----------------------------------------------------

#' Combination of elements in a vector taken n at time
#'
#' @param x Vector
#' @param n Number of elements from x per combination. Default is `2`
#' @param col_names Character vector containing names of columns to be generated
#' @param na.rm If `TRUE` (default), missing values are removed from x prior to generating combinations
#' @returns Data frame with 1 row for each unique combination of elements taken n at a time. Pairs of elements not repeated (i.e. if a row of output data frame contains a in V1 and B in V2 there will be no row with b in V1 and a in V2)
#' @noRd
.combos_1_vec_as_df <- function(x, n = 2, na.rm = TRUE, col_names = paste0("V", seq_len(n))) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_len <- length(x)
  if (x_len < n) {
    message(sprintf("Input 'x' to combos_list() must have length > 'n'. Currently, length of 'x' is %s and 'n = %s'", x_len, n))
    return(NULL)
  }
  e <- 0
  h <- n
  a <- col_idx <- seq_len(n)
  r <- x[a]
  n_rows <- choose(n = x_len, k = n)
  m <- matrix(r, nrow = length(r), ncol = n_rows)
  i <- 2L
  z <- x_len - n + 1L
  while (a[1L] != z) {
    if (e < x_len - h) {
      h <- 1L
      e <- a[n]
      j <- 1L
    } else {
      e <- a[n - h]
      h <- h + 1L
      j <- 1L:h
    }
    a[n - h + j] <- e + j
    r <- x[a]
    m[, i] <- r
    i <- i + 1L
  }
  # dim(m) <- c(n, n_rows)
  m <- t.default(m)
  df <- vector("list", n)
  for (i in col_idx) {
    df[[i]] <- m[, i]
  }
  class(df) <- "data.frame"
  attr(df, "row.names") <- c(NA_integer_, -n_rows)
  names(df) <- col_names
  df
}

#' Number of combinations
#'
#' Order does not matter (i.e. a, b is the same as b, a)
#' @param n Number of options
#' @param k Number of items selected (note: `n` must be >= `k`)
#' @param replacement If `TRUE` (default), combinations with replacement is calculated. If `FALSE`, combinations without replacement is calculated. Example of replacement: if creating all combinations of 2 letters, "AA" is allowed (i.e. "A" can be selected twice)
#' @returns Number of possible combinations
#' @export
n_combos <- function(n, k, replacement = TRUE) {
  if (replacement) {
    factorial(n + k - 1)/(factorial(k)*factorial(n - 1))
  } else {
    choose(n, k)
  }
}

#' Number of permutations
#'
#' Order matters
#' @param n Number of options
#' @param k Number of items selected (note: `n` >= `k`)
#' @param replacement If `TRUE` (default), combinations with replacement is calculated. If `FALSE`, combinations without replacement is calculated. Example of replacement: if creating all combinations of 2 letters, "AA" is allowed (i.e. "A" can be selected twice)
#' @returns Number of permutations
#' @export
n_permutations <- function(n, k = n, replacement = FALSE) {
  if (replacement) {
    n^k
  } else {
    factorial(n)/factorial(n - k)
  }
}
