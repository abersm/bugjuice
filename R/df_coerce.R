# Other class to data frame -----------------------------------------------

#' Convert vectors to data.frame
#'
#' @param ... Vectors or list of vectors. Can have different lengths (lengths do not need to share a common multiple which is in contrast to vector to data frame conversion by data.frame() function)
#' @param prefix Prefix for newly created column names. Default is `"V"`
#' @returns Data frame. Each input vector forms a column in output data frame. If vectors do not share the same length, shorter vectors are recycled to match the length of the longest input vector (see examples)
#' @export
vec_to_df <- function(..., prefix = "V") {
  x <- if (is.list(x <- c(...))) x else list(...)
  z <- lengths(x)
  if (any(zero_idx <- z == 0)) {
    x[zero_idx] <- NULL
    z <- z[z != 0]
  }
  n <- max(z)
  if (any(idx <- z != n)) {
    for (i in which(idx)) {
      vals <- .subset2(x, i)
      x[[i]] <- rep_len(vals, length.out = n)
    }
  }
  names(x) <- .force_names(x, prefix = prefix)
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)
  x
}

#' Convert a list to a data frame
#'
#' Opposite of `as.list` applied to a data frame
#' @param x List of vectors. All elements of list (each vector) must have same length or must share a common multiple (see examples for details). To convert a list of vectors to rows of a data frame, use `list_to_df_rowwise()`
#' @returns Data frame with elements of input list (vectors) forming columns of output data frame
#' @export
list_to_df <- function(x) {
  x <- remove_null(x)
  if (length(x) == 0L) return(NULL)
  names(x) <- .force_names(x)
  z <- lengths(x)
  n <- max(z)
  if (any(idx <- z != n)) {
    for (i in which(idx)) {
      x[[i]] <- rep_len(.subset2(x, i), length.out = n)
    }
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)
  x
}

#' Convert a list of vectors to rows of a data frame
#'
#' Similar to `Reduce(rbind, list)`, but faster
#' @param ... Vectors or list of vectors used to create rows in output data frame. If the lengths of list components are not equal, shorter elements will be recycled to the length of the longest element (i.e. for a single row, values will be recycled across the leftover columns)
#' @param rownames_to_col If `TRUE`, rownames of `x` are used to create a new column in output data frame. Default is `FALSE`
#' @param colname_rownames Name of column in output data frame containing rownames of `x`. Only relevant when `rownames_to_col = TRUE`. Default is `"rowname"`
#' @param prefix Prefix for newly created column names. Default is `"V"`
#' @returns Data frame with elements of input combined to form rows of data frame
#' @export
list_to_df_rowwise <- function(..., rownames_to_col = FALSE, colname_rownames = "rowname", prefix = "V") {
  x <- if (is.list(x <- c(...))) x else list(...)
  matrix_to_df(do.call(rbind, x), rownames_to_col = rownames_to_col, prefix = prefix, colname_rownames = colname_rownames)
}

#' Convert a matrix to a data frame
#'
#' @param x Matrix
#' @param rownames_to_col If `TRUE`, rownames of `x` are used to create a new column in output data frame. Default is `FALSE`
#' @param colname_rownames Name of column in output data frame containing rownames of `x`. Only relevant when `rownames_to_col = TRUE`. Default is `"rowname"`
#' @param prefix Prefix for newly created column names. Default is `"V"`
#' @export
matrix_to_df <- function(x, rownames_to_col = FALSE, colname_rownames = "rowname", prefix = "V") {
  # Dimensions of input matrix
  dims <- dim(x)
  n_rows <- dims[[1L]]
  n_cols <- dims[[2L]]
  col_idx <- seq_len(n_cols)

  # Row/column names of input matrix
  x_names <- dimnames(x)
  col_names <- x_names[[2L]]
  if (is.null(col_names)) {
    col_names <- paste0(prefix, col_idx)
  } else {
    # Identify any columns that lack names
    empty_col_names <- !nzchar(col_names)
    if (any(empty_col_names)) {
      # Create new column names with a numeric component that reflects column number in input matrix
      col_names[empty_col_names] <- paste0(prefix, col_idx)[empty_col_names]
    }
  }

  # Build data frame
  df <- vector("list", n_cols)
  for (i in col_idx) {
    df[[i]] <- x[, i]
  }
  class(df) <- "data.frame"
  attr(df, "row.names") <- c(NA_integer_, -n_rows)
  names(df) <- col_names
  if (rownames_to_col && !is.null(row_names <- x_names[[1L]])) {
    df[[colname_rownames]] <- row_names
    df[c(colname_rownames, col_names)]
  } else {
    df
  }
}

# Data frame to other class -----------------------------------------------

#' Convert data frame to matrix
#'
#' Functionality from base function data.matrix
#' @param df Data frame
#' @export
df_to_matrix <- function(df) {
  d <- dim(df)
  n_col <- d[2L]
  n_row <- d[1L]
  idx_col <- seq_len(n_col)
  for (i in idx_col) {
    xi <- .subset2(df, i)
    if (is.numeric(xi)) {
      next
    }
    if (inherits(xi, c("factor", "character", "logical"))) {
      df[[i]] <- as_numeric_factor(xi)
      next
    }
    df[[i]] <- as.numeric(xi)
  }
  x <- matrix(if (all(vapply(df, is.integer, logical(1)))) NA_integer_ else NA_real_, nrow = n_row, ncol = n_col, dimnames = list(NULL, names(df)))
  for (i in idx_col) {
    x[, i] <- .subset2(df, i)
  }
  x
}

#' Convert data frame to list of columns
#'
#' @param df Data frame
#' @export
df_to_list <- function(df) as.list(df)

#' Convert data frame to list of rows
#'
#' @param df Data frame
#' @returns List of length `nrow(df)` with each element containing a data frame with 1 row
#' @export
df_to_list_rowwise <- function(df) lapply(seq_nrow(df), function(i) df[i, ])

#' Convert 2 columns of data frame to a named vector
#'
#' @param df Data frame
#' @param values Column in `df` to create values in output vector. Enter as quoted or unquoted column name
#' @param names Column in `df` to create names in output vector. Enter as quoted or unquoted column name
#' @returns Named vector
#' @export
df_to_named_vector <- function(df, values, names) {
  x <- .subset2(df, get_input(values))
  names(x) <- .subset2(df, get_input(names))
  x
}

#' Convert named vector to data frame with 2 columns (1 column for values, 1 column for named)
#'
#' @param x Named vector
#' @returns Data frame with columns "names", "values"
#' @export
named_vector_to_df <- function(x) vec_to_df(names = names(x), values = unname(x))
