#' Create 2 x 2 table for a broad range of input types
#'
#' @param x Data frame of counts, matrix, table, or length 4 numeric vector of counts. Enter by column (i.e., row 1/col 1, then row 2/col 1, then row 1/col 2, then row 2/col 2)
#' @param ... Arguments passed to class-specific `tab_2_by_2` function
#' @returns 2 x 2 table
#' @export
tab_2_by_2 <- function(x, ...) UseMethod("tab_2_by_2")

#' tab_2_by_2 - default method
#'
#' @rdname tab_2_by_2
#' @export
tab_2_by_2.default <- function(x, ...) {
  stop(paste("Not sure how to handle input to tab_2_by_2() of class", paste0(class(x), collapse = ", ")), call. = FALSE)
}

#' tab_2_by_2 - numeric
#'
#' @param x Numeric vector
#' @param ... Counts or arguments passed to table
#' @export
tab_2_by_2.numeric <- function(x, ...) {
  x_counts <- c(x, ...)
  if (length(x) != 4L) {
    x <- table(x, ...)
    dims <- dim(x)
    if (!all(dims == 2L)) {
      stop(sprintf("Numeric vector input to tab_2_by_2() must either be a length 4 numeric vector or 2 binary vectors that generate a 2 x 2 table.\nCurrent input has length %s and table(x, ...) generates a %s x %s table", length(x_counts), dims[1L], dims[2L]), call. = FALSE)
    }
    return(x)
  }
  as.table(matrix(x_counts, nrow = 2))
}

#' tab_2_by_2 - logical
#'
#' @param x Logical vector
#' @param ... Arguments passed to table
#' @export
tab_2_by_2.logical <- function(x, ...) {
  x <- table(x, ...)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    stop(sprintf("Logical vector input to tab_2_by_2() must generate a 2 x 2 table.\ntable(x, ...) applied to current input generates a %s x %s table", dims[1L], dims[2L]), call. = FALSE)
  }
  x
}

#' tab_2_by_2 - character
#'
#' @param x Character vector
#' @param ... Arguments passed to table
#' @export
tab_2_by_2.character <- function(x, ...) {
  x <- table(x, ...)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    stop(sprintf("Character vector input to tab_2_by_2() must create a 2 x 2 table, not a %s x %s table", dims[1L], dims[2L]), call. = FALSE)
  }
  x
}

#' tab_2_by_2 - factor vector
#'
#' @rdname tab_2_by_2.character
#' @export
tab_2_by_2.factor <- tab_2_by_2.character

#' tab_2_by_2 - matrix
#'
#' @param x 2 x 2 matrix
#' @param ... Not used
#' @export
tab_2_by_2.matrix <- function(x, ...) {
  dims <- dim(x)
  if (!all(dims == 2L)) {
    stop(sprintf("Matrix input to tab_2_by_2() must be 2 x 2, not %s x %s", dims[1L], dims[2L]), call. = FALSE)
  }
  as.table(x)
}

#' tab_2_by_2 - table
#'
#' @param x 2 x 2 table
#' @param ... Not used
#' @export
tab_2_by_2.table <- function(x, ...) {
  dims <- dim(x)
  if (!all(dims == 2L)) {
    stop(sprintf("Table input to tab_2_by_2() must be 2 x 2, not %s x %s", dims[1L], dims[2L]), call. = FALSE)
  }
  x
}

#' tab_2_by_2 - data.frame
#'
#' @param x Data frame of counts
#' @param ... Columns to use in `x`. Enter as formula (y ~ x or ~ y + x. outcome ~ predictor), comma separated list of quoted or unquoted column names not wrapped in `c()`, or tidyselect syntax
#' @export
tab_2_by_2.data.frame <- function(x, ...) {
  # Attempt to create 2 x 2 table using xtab
  x_xtab <- tryCatch(xtab(x, ...), error = function(e) NULL)
  if (!is.null(x_xtab)) {
    dims <- dim(x_xtab)
    if (all(dims == 2L)) return(x_xtab)
  }

  # If x is the output of count() or tab_to_counts()
  # Remove NA if x is the output of count()
  x_counts <- x[complete.cases(x), ]
  dims <- dim(x_counts)
  # If x contains a column of counts, number of rows should be 4
  if (dims[1L] == 4L) {
    if (dims[2L] == 1L) {
      # If x contains 1 column, create table of counts
      as.table(matrix(unlist(x_counts), nrow = 2))
    } else if (dims[2L] %in% c(2, 3)) {
      # If x contains 2 or 3 columns, find column containing counts (i.e. non-binary numeric variable)
      counts_var <- vars_which(x_counts, function(y) is_not_binary(y) && is.numeric(y))
      # Ensure x_count has appropriate row order
      if (length(counts_var) == 1L) {
        # x_counts has only 1 column that possibly contains count data
        group_vars <- Setdiff(names(x_counts), counts_var)
        # Arrange rows in order of other columns
        row_order <- lapply(group_vars, function(y) x_counts[[y]])
        x_counts <- x_counts[do.call("order", row_order), , drop = FALSE]
        if (length(group_vars) == 2L) {
          # If 2 grouping variables in x_counts, create dimnames
          tab_rows <- unique.default(x_counts[group_vars[1L]][[1L]])
          tab_cols <- unique.default(x_counts[group_vars[2L]][[1L]])
          dim_names <- list(tab_rows, tab_cols)
          names(dim_names) <- group_vars
        } else {
          # If > 2 grouping variables in x_counts, don't attempt to set dimnames
          dim_names <- list(NULL)
        }
        as.table(matrix(unlist(x_counts[[counts_var]]), nrow = 2, byrow = TRUE, dimnames = dim_names))
      } else {
        # Send message if x_counts contains multiple columns that possibly contains count data
        message("Data frame input to tab_2_by_2() has dimensions of ", paste(dims, collapse = " x "), "\nUnclear which column contains count data")
      }
    } else {
      stop("Not able to handle data frame input to tab_2_by_2() with dimensions of  ", paste(dims, collapse = " x "), call. = FALSE)
    }
  } else {
    # If x is not a data frame of counts, determine which columns were selected in dots
    dots_class <- vapply(dots_as_unquoted(...), class, character(1))
    vars <- if (dots_class[1L] == "call") all.vars(...) else names(dplyr::select(x, ...))
    n_vars <- length(vars)
    if (n_vars == 2L) {
      x[complete.cases(x[, vars, drop = FALSE]), ]
    } else if (n_vars > 2L) {
      stop("Data frame input to tab_2_by_2() and , ",  n_vars, " columns selected in ...", "\nColumns selected: ", paste0(vars, collapse = ", "), call. = FALSE)
    } else {
      stop("Data frame input to tab_2_by_2() and , ",  n_vars, " columns selected in ...", if (n_vars == 1L) paste0("\nColumn selected: ", vars), call. = FALSE)
    }
  }
}

#' tab_2_by_2 - glm
#'
#' @param x glm object
#' @param ... Not used
#' @export
tab_2_by_2.glm <- function(x, ...) {
  x <- xtab(x$model)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    stop(sprintf("Input to tab_2_by_2 is a glm\nTable generated by predictor and outcome variables is &s, not 2 x 2", paste0(dims, collapse = " x ")), call. = FALSE)
  }
  x
}
