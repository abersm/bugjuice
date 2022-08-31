#' Get names of variables based on predicate function applied to column values
#'
#' @param .df Data frame
#' @param .predicate_fn Predicate function that will be applied to column values to select variable names. Must evaluate to logical (not `NULL`) and should not be vectorized (i.e. for vector input, single value should be returned)
#' @param ... Arguments passed to `.predicate_fn`
#' @param .invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @returns Character vector containing variable names that result in `TRUE` when `.predicate_fn` is applied to columns in `df`
#' @export
vars_which <- function(.df, .predicate_fn, ..., .invert = FALSE) {
  vars <- vapply(.df, .predicate_fn, logical(1), ..., USE.NAMES = TRUE)
  if (.invert) {
    vars <- !vars
  }
  names(vars)[vars]
}

#' Get names of variables based on predicate function applied to variable names
#'
#' @param .df Data frame
#' @param .predicate_fn Predicate function that will be applied to column names to select variable names. Must evaluate to logical (not `NULL`) and should not be vectorized (i.e. for vector input, single value should be returned)
#' @param ... Arguments passed to `predicate_fn`
#' @param .invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @returns Character vector containing variable names that result in `TRUE` when `predicate_fn` is applied to column names of `df`
#' @export
varnames_which <- function(.df, .predicate_fn, ..., .invert = FALSE) {
  df_names <- names(.df)
  z <- .predicate_fn(df_names, ...)
  names(z) <- df_names
  if (.invert) {
    z <- !z
  }
  df_names[z]
}

#' Get names of binary variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_binary <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_binary, .invert = invert)

#' Get names of binary variables coded as 0 or 1
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_binary_01 <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_binary_01, .invert = invert)

#' Get names of categorical variables
#'
#' @param df Data frame
#' @param max_n_unique Maximum number of unique values, below which variable assumed to be categorical
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_cat <- function(df, max_n_unique = 10, invert = FALSE) vars_which(df, .predicate_fn = is_categorical, max_n_unique = max_n_unique, .invert = invert)

#' Get names of factor variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_fct <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.factor, .invert = invert)

#' Get names of character variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_chr <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.character, .invert = invert)

#' Get names of numeric variables
#'
#' @param df Data frame
#' @param incl_integer If `TRUE` (default), integer variables are included
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_dbl <- function(df, incl_integer = TRUE, invert = FALSE) {
  if (incl_integer) {
    vars_which(df, .predicate_fn = is.numeric, .invert = invert)
  } else {
    vars_which(df, .predicate_fn = function(x) is.numeric(x) && !is.integer(x) && !all(is_integerish(x)), .invert = invert)
  }
}

#' Alias for vars_dbl
#'
#' @rdname vars_dbl
#' @export
vars_numeric <- vars_dbl

#' Get names of continuous variables
#'
#' @rdname vars_dbl
#' @export
vars_continuous <- function(df, invert = FALSE) vars_dbl(df, incl_integer = FALSE, invert = invert)

#' Get names of integer variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_int <- function(df, invert = FALSE) {
  vars_which(df, .predicate_fn = function(x) is.integer(x) || all(is_integerish(x)), .invert = invert)
}

#' Get names of variables which are coercible to a numeric
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_can_be_numeric <- function(df, invert = FALSE) vars_which(df, .predicate_fn = function(x) all(can_be_numeric(x)), .invert = invert)

#' Get names of logical variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_lgl <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.logical, .invert = invert)

#' Get names of date variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_date <- function(df, invert = FALSE) vars_which(df, .predicate_fn = function(x) inherits(x, c("Date", "POSIXct")), .invert = invert)

#' Get names of id variables
#'
#' @param df Data frame
#' @param na.rm If `TRUE`, missing values are not considered
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_id <- function(df, na.rm = FALSE, invert = FALSE) {
  fn <- if (na.rm) {
    function(x) {
      x <- x[!is.na(x)]
      length(x) == length(unique.default(x))
    }
  } else {
    n <- Nrow(df)
    function(x) length(unique.default(x)) == n
  }
  vars_which(df, .predicate_fn = fn, .invert = invert)
}

#' Get names of constant (zero-variance) variables
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_constant <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_constant, .invert = invert)

#' Get names of variables that contain no missing values
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_no_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = function(x) !anyNA(x), .invert = invert)

#' Get names of variables that contain one or more missing values
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_any_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = anyNA, .invert = invert)

#' Get names of variables that contain only missing values
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
vars_all_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = all_na, .invert = invert)

#' Class of each variable
#'
#' @param df Data frame
#' @param col Column(s) to evaluate. If `NULL` (default), all columns included. Enter as character or integer vector
#' @returns Data frame containing columns for var_name, col_number, class
#' @export
var_class <- function(df, col = NULL) {
  idx <- seq_along(df)
  names(idx) <- names(df)
  col <- col %||% idx
  col <- idx[col]
  df <- df[col]
  var_name <- names(col)
  names(col) <- NULL
  df <- vec_to_df(var = var_name, col_number = col, class = vapply(df, class, character(1), USE.NAMES = FALSE))
  df[order(df$class), , drop = FALSE]
}

# Counts ------------------------------------------------------------------

#' Number of unique values for variables of data frame
#'
#' @param df Data frame
#' @param na.rm If `TRUE` (default), missing values are not considered a unique value
#' @returns Named integer vector of unique values for each variable
#' @export
n_unique_per_var <- function(df, na.rm = TRUE) {
  vapply(df, n_unique, FUN.VALUE = integer(1), na.rm = na.rm, USE.NAMES = TRUE)
}

# Duplicates --------------------------------------------------------------

#' Identify duplicate values in a vector or rows in a data frame
#'
#' @param x Data frame or vector
#' @param ... Arguments passed to class-specific `get_dupes` function
#' @export
get_dupes <- function(x, ...) UseMethod("get_dupes")

#' get_dupes - default method
#'
#' @param x Vector
#' @param ... Not used
#' @returns Data frame with columns duplicated_value, n_dupes, positions
#' @export
get_dupes.default <- function(x, ...) {
  dupe_values <- x[duplicated.default(x)]
  list_to_df_rowwise(lapply(unique.default(dupe_values), function(y) {
    idx <- x == y
    c(duplicated_value = y, n_dupes = length(x[idx]), positons = paste(which(idx), collapse = ", "))
  }))
}

#' get_dupes - data frame
#'
#' Searches for duplicate rows
#' @param x Data frame
#' @param ... Columns to search for duplicates. Default uses all columns. Enter using tidyselect syntax
#' @returns Original data frame with new column (dupe_count) containing the number of duplicate rows
#' @export
get_dupes.data.frame <- function(x, ...) {
  vars <- if (n_dots(...) == 0) names(x) else names(dplyr::select(x, ...))
  var_names <- rlang::syms(vars)
  dupes <- dplyr::group_by(x, !!!var_names)
  dupes <- dplyr::mutate(dupes, dupe_count = 1:dplyr::n())
  dupes <- dplyr::ungroup(dupes)
  dupes <- dupes[dupes$dupe_count > 1, ]
  if (nrow(dupes) == 0) {
    message(sprintf("No duplicate combinations found\nThe following variables were included in search:\n%s", paste(vars, collapse = ", ")))
    return(NULL)
  }
  dupes <- dupes[c(vars, "dupe_count", Setdiff(names(dupes), c(vars, "dupe_count")))]
  dupes <- dupes[do.call("order", dupes[vars]), ]
  dupes
}

#' get_dupes - grouped_df
#'
#' Searches for duplicate rows
#' @inheritParams get_dupes.data.frame
#' @returns Original data frame with new column (dupe_count) containing the number of duplicate rows
#' @export
get_dupes.grouped_df <- function(x, ...) {
  grouping_vars <- dplyr::group_vars(x)
  x <- get_dupes.data.frame(dplyr::ungroup(x), ...)
  if (length(x) == 0L) return(NULL)
  dplyr::group_by(x, !!!rlang::syms(grouping_vars))
}

#' Identify duplicate columns in data frame
#'
#' @param df Data frame
#' @param ignore_class If `TRUE`, all columns are converted to characters prior to analysis
#' @returns Named character vector of duplicated columns in `df` in which values represent column numbers and names represent column names
#' @export
dupe_cols <- function(df, ignore_class = FALSE) {
  if (ignore_class) {
    df <- lapply(df, as.character)
  }
  df_names <- names(df)
  dupe_col_idx <- duplicated.default(df)
  if (sum(dupe_col_idx) == 0L) {
    message("No duplicate columns present in data")
    return(NULL)
  }
  dupes <- df_names[dupe_col_idx]
  others <- Setdiff(df_names, dupes)
  df_others <- df[others]
  out <- lapply(dupes, function(x) {
    x_values <- .subset2(df, x)
    cols <- others[vapply(df_others, function(y) identical(x_values, y), logical(1), USE.NAMES = FALSE)]
    c(col = x, dupe_cols = paste(cols, collapse = ", "))
  })
  list_to_df_rowwise(out)
}

# Other -------------------------------------------------------------------

#' Faster version of nrow
#'
#' @param x Data frame (not for matrices)
#' @returns Length 1 integer
#' @noRd
Nrow <- function(x) .row_names_info(x, type = 2L)

#' Row indices
#'
#' @param x Data frame
#' @returns Integer vector from 1 to `nrow(x)`
#' @noRd
seq_nrow <- function(x) seq_len(Nrow(x))

#' Determine whether column names are present in data frame
#'
#' Faster than testing x %in% names(df). Same speed as x %in% df_names
#' @param df Data frame
#' @param x Possible column name. Must be a length 1 character vector
#' @returns Logical vector of length 1
#' @noRd
is_column <- function(df, x) !is.null(.subset2(df, x))
