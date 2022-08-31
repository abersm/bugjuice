# Contingency tables ------------------------------------------------------

#' Contingency table
#'
#' @param df Data frame
#' @param ... Variables to create table. Enter as character vector, comma separated list of unquoted or unquoted column names, or formula
#' @param incl_na If `FALSE` (default), NA values are not included in table. If `TRUE`, NA is included in table
#' @export
xtab <- function(df, ..., incl_na = FALSE) {
  dots <- dots_as_quoted(...)
  n <- n_dots(...)
  vars <- if (n == 0L) {
    names(df)
  } else if (n == 1L) {
    if (startsWith(dots[[1L]], "~") && is.list(vars <- c(...))) {
      all.vars(as_formula(as.character(vars)))
    } else {
      tryCatch({
        vars <- c(...)
        if (is.character(vars) && all(vars %in% names(df))) vars else dots
      }, error = function(e) dots)
    }
  } else {
    dots
  }
  df <- lapply(df[vars], function(x) {
    if (!inherits(x, "factor")) {
      x <- factor(x, exclude = if (!incl_na) c(NA, NaN))
    }
    if (incl_na && anyNA(x)) {
      x_levels <- attr(x, "levels")
      if (!anyNA(x_levels)) {
        x_levels <- c(x_levels, NA)
      }
      x <- factor(x, levels = x_levels, exclude = NULL)
    }
    x
  })
  table(df, dnn = names(df))
}

#' Alias for xtab
#'
#' @rdname xtab
#' @export
tab <- xtab

#' Cross table with or without row/column totals
#'
#' @param df Data frame
#' @param ... Columns in `df` used to generate contingency table. Enter using tidyselect syntax
#' @param total If `FALSE` (Default), no columns or rows for totals in output table. If `TRUE`, row and column totals are included in output table
#' @param incl_na If `TRUE` (default), missing values are included in output
#' @param title_row_totals Title of column containing row totals. Default is `"Total"`
#' @param title_col_totals Title of row containing column totals. Default is `"Total"`
#' @returns Cross table +/- rows and columns for totals
#' @export
x_table <- function(df, ..., total = FALSE, incl_na = TRUE, title_row_totals = "Total", title_col_totals = "Total") {
  vars <- names(dplyr::select(df, ...))
  df <- xtab(df = df, vars, incl_na = incl_na)
  if (total) {
    add_totals(df, title_row_totals = title_row_totals, title_col_totals = title_col_totals)
  } else {
    df
  }
}

#' Add column and row totals to table or matrix
#'
#' @param x Table or array/matrix
#' @param title_row_totals Title of column containing row totals. Default is `"Total"`
#' @param title_col_totals Title of row containing column totals. Default is `"Total"`
#' @returns Matrix containing row and column sums as well as raw numbers in `x`. Number in bottom right hand corner of matrix contains overall total
#' @export
add_totals <- function(x, title_row_totals = "Total", title_col_totals = "Total") {
  dims <- dim(x)
  n_rows <- dims[1L]
  n_cols <- dims[2L]
  other_dims <- dims[-c(1, 2)]
  z <- matrix(x, nrow = n_rows)
  z <- rbind(z, .colSums(z, n_rows, n_cols))
  z <- matrix(t.default(z), nrow = n_cols)
  z <- rbind(z, .colSums(z, n_cols, n_rows + 1L))
  z <- t.default(matrix(t.default(z), nrow = prod(other_dims)))
  z <- array(z, c(dims + 1, other_dims))
  rownames(z) <- c(rownames(x, do.NULL = FALSE), title_col_totals)
  colnames(z) <- c(colnames(x, do.NULL = FALSE), title_row_totals)
  x_names <- names(dimnames(x))
  if (!is.null(x_names)) {
    names(dimnames(z)) <- x_names
  }
  z
}

#' Convert counts in contingency table to column proportions
#'
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @returns Table containing column proportions (i.e., column proportions sum to 1)
#' @export
col_prop <- function(x, ..., incl_na = FALSE) {
  x <- if (inherits(x, "data.frame")) {
    xtab(df = x, names(dplyr::select(x, ...)), incl_na = incl_na)
  } else {
    as.table(x)
  }
  sweep(x, 2, marginSums(x, 2), "/", check.margin = FALSE)
}

#' Convert counts in contingency table to row proportions
#'
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `TRUE` (default), missing values are included in output
#' @returns Table containing row proportions (i.e., row proportions sum to 1)
#' @export
row_prop <- function(x, ..., incl_na = FALSE) {
  x <- if (inherits(x, "data.frame")) {
    xtab(df = x, names(dplyr::select(x, ...)), incl_na = incl_na)
  } else {
    as.table(x)
  }
  sweep(x, 1, marginSums(x, 1), "/", check.margin = FALSE)
}

#' Convert counts in contingency table to column percentages
#'
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @param digits Number of digits to include after decimal. Default is `1`
#' @returns Table containing column percentages (i.e., column percentages sum to 100)
#' @export
col_perc <- function(x, ..., incl_na = FALSE, digits = 1) {
  x <- col_prop(x, ..., incl_na = incl_na)
  round_up(x*100, digits = digits)
}

#' Convert counts in contingency table to row percentages
#'
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @param digits Number of digits to include after decimal. Default is `1`
#' @returns Table containing row percentages (i.e., row percentages sum to 100)
#' @export
row_perc <- function(x, ..., incl_na = FALSE, digits = 1) {
  x <- row_prop(x, ..., incl_na = incl_na)
  round_up(x*100, digits = digits)
}

#' Create data frame with counts column
#'
#' Output similar to `dplyr::count()` output when applied to df but with different row order
#' @param x Table or matrix with dimnames or data frame
#' @param ... Columns used to generate table. Only relevant if `x` is a data frame
#' @param colname_counts Column name for counts. Enter as quoted column name. Default is `"Freq"`
#' @returns Data frame containing column with counts
#' @export
tab_to_counts <- function(x, ..., colname_counts = "Freq") {
  if (inherits(x, "data.frame")) {
    x <- xtab(x, ...)
  }
  x_dimnames <- dimnames(x)
  if (is.null(x_dimnames)) {
    stop("Input 'x' to tab_to_counts() must contain dimnames", call. = FALSE)
  }
  if (is.null(names(x_dimnames))) {
    names(dimnames(x)) <- paste0("V", seq_along(x_dimnames))
  }
  df <- crossings(x_dimnames)
  df_names <- names(dimnames(x))
  idx <- df_names == "" | grepl("^\\.+|^[0-9]", df_names)
  df_names[idx] <- paste0("V", seq_len(sum(idx)))
  names(df) <- df_names
  df[[colname_counts]] <- c(x)
  df
}

#' Alias for `tab_to_counts`
#'
#' @rdname tab_to_counts
#' @export
counts <- tab_to_counts

#' Convert contingency table into raw data
#'
#' @param x Contingency table or matrix containing dimnames or a data frame with 1 column for counts and other columns containing grouping variables (1 row per group). Data frame input is should be formatted as the output of `dplyr::count`
#' @param colname_counts Column in `x` containing counts. Only relevant when `x` is a data frame containing counts. Default is `"Freq"`
#' @returns Raw data with 1 row per individual and 1 column for each variable
#' @export
tab_to_data <- function(x, colname_counts = "Freq") {
  # Create data frame with columns for grouping variables and column containing counts called "Freq"
  if (inherits(x, "table")) {
    # x is a table
    x_dimnames <- dimnames(x)
    if (is.null(names(x_dimnames))) {
      names(dimnames(x)) <- paste0("V", seq_along(x_dimnames))
    }
    x <- tab_to_counts(x, colname_counts = "Freq")
    x_names <- names(x)
    idx <- x_names == "" | grepl("^\\.+|^[0-9]", x_names)
    names(x)[idx] <- paste0("V", seq_len(sum(idx)))
  } else if (is.matrix(x)) {
    x <- tab_to_counts(x, colname_counts = "Freq")
    x_names <- names(x)
    idx <- x_names == "" | grepl("^\\.+|^[0-9]", x_names)
    names(x)[idx] <- paste0("V", seq_len(sum(idx)))
  } else {
    # x is a data frame
    x_names <- names(x)
    if (!any(x_names == colname_counts)) {
      stop(sprintf("'%s' is not a column name in input to tab_to_data", colname_counts), call. = FALSE)
    }
    names(x)[x_names == colname_counts] <- "Freq"
  }
  # Remove rows with 0 counts
  x <- x[x$Freq != 0, ]

  # Use counts in "Freq" column of x to generate appropriate number of rows
  x <- x[rep(seq_len(nrow(x)), x$Freq), Setdiff(names(x), "Freq")]
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}
