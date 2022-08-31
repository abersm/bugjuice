# Subset columns ----------------------------------------------------------

#' Select columns using tidyselect syntax
#'
#' Similar to `dplyr::select` but includes all columns when `...` are empty
#' @param df Data frame
#' @param ... Columns to select. Enter using tidyselect syntax
#' @returns Same output as `dplyr::select()` except that all columns are selected when dots are empty
#' @export
Select <- function(df, ...) if (n_dots(...) == 0L) df else dplyr::select(df, ...)

#' Remove columns using tidyselect syntax
#'
#' @rdname Select
#' @export
Unselect <- function(df, ...) dplyr::select(df, -c(...))

# Subset columns based on predicate ---------------------------------------

#' Get a subset of data frame columns according to a predicate
#'
#' @param .df Data frame
#' @param .predicate_fn Predicate function to be applied to each column in `.df`. Must evaluate to a length 1 logical (not `NULL`), thus non-vectorized functions only
#' @param ... Arguments passed to `.predicate_fn`
#' @param .invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @returns Data frame with columns meeting predicate criterion
#' @export
select_which <- function(.df, .predicate_fn, ..., .invert = FALSE) {
  vars <- vapply(.df, .predicate_fn, logical(1), ..., USE.NAMES = FALSE)
  if (.invert) {
    vars <- !vars
  }
  .df[vars]
}

#' Data frame with binary columns only
#'
#' @param df Data frame
#' @param invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @export
select_binary <- function(df, invert = FALSE) select_which(df, .predicate_fn = is_binary, .invert = invert)

#' Data frame with binary columns coded as 0 or 1 only
#'
#' @rdname select_binary
#' @export
select_binary_01 <- function(df, invert = FALSE) select_which(df, .predicate_fn = is_binary_01, .invert = invert)

#' Data frame with categorical columns only
#'
#' @inheritParams select_binary
#' @param max_n_unique Maximum number of unique values, below which variable assumed to be categorical
#' @export
select_cat <- function(df, max_n_unique = 10, invert = FALSE) select_which(df, .predicate_fn = is_categorical, max_n_unique = max_n_unique, .invert = invert)

#' Data frame with factor columns only
#'
#' @rdname select_binary
#' @export
select_fct <- function(df, invert = FALSE) select_which(df, .predicate_fn = is.factor, .invert = invert)

#' Data frame with character columns only
#'
#' @rdname select_binary
#' @export
select_chr <- function(df, invert = FALSE) select_which(df, .predicate_fn = is.character, .invert = invert)

#' Data frame with numeric columns only
#'
#' @inheritParams select_binary
#' @param incl_integer If `TRUE` (default), integer variables are included
#' @export
select_dbl <- function(df, incl_integer = TRUE, invert = FALSE) {
  fn <- if (incl_integer) {
    is.numeric
  } else {
    function(x) is.numeric(x) && !is.integer(x) && !all(is_integerish(x))
  }
  select_which(df, .predicate_fn = fn, .invert = invert)
}

#' Alias for cols_dbl
#'
#' @rdname select_dbl
#' @export
select_numeric <- select_dbl

#' Data frame with integer columns only
#'
#' @rdname select_binary
#' @export
select_int <- function(df, invert = FALSE) {
  select_which(df, .predicate_fn = function(x) is.integer(x) || all(is_integerish(x)), .invert = invert)
}

#' Data frame with columns coercible to a numeric only
#'
#' @rdname select_binary
#' @export
select_can_be_numeric <- function(df, invert = FALSE) select_which(df, .predicate_fn = function(x) all(can_be_numeric(x)), .invert = invert)

#' Data frame with logical columns only
#'
#' @rdname select_binary
#' @export
select_lgl <- function(df, invert = FALSE) select_which(df, .predicate_fn = is.logical, .invert = invert)

#' Data frame with date columns only
#'
#' @rdname select_binary
#' @export
select_date <- function(df, invert = FALSE) select_which(df, .predicate_fn = function(x) inherits(x, c("Date", "POSIXct")), .invert = invert)

#' Data frame with id columns only
#'
#' @inheritParams select_which
#' @param na.rm If `TRUE`, missing values are not considered
#' @export
select_id_vars <- function(df, na.rm = FALSE, invert = FALSE) {
  fn <- if (na.rm) {
    function(x) length(x) == n_unique(x)
  } else {
    n <- Nrow(df)
    function(x) length(unique.default(x)) == n
  }
  select_which(df, .predicate_fn = fn, .invert = invert)
}

#' Remove constant columns
#'
#' @param df Data frame
#' @param na.rm If `TRUE` (default), NA are not considered in determining whether a column is constant
#' @returns Data frame with constant columns removed
#' @export
select_not_constant <- function(df, na.rm = TRUE) {
  df[, vapply(df, function(x) n_unique(x, na.rm = na.rm) != 1L, logical(1), USE.NAMES = FALSE), drop = FALSE]
}

#' Select data frame columns with names matching a specified pattern
#'
#' @param df Data frame, named vector, or named list
#' @param pattern Regex pattern to search in column names
#' @param ... Arguments passed to `search_fn`
#' @param search_fn Function used to search column names for `pattern`. Default is `str_contains` if 1 term is entered as `pattern` and `str_contains_all` if > 1 term is entered. Function must have arguments `x` and `pattern`
#' @returns Data frame with columns in which names contain `pattern`
#' @export
select_names_which <- function(df, pattern, ..., search_fn = NULL) {
  search_fn <- search_fn %||% if (length(pattern) == 1L) str_contains else str_contains_all
  vars <- search_fn(x = names(df), pattern = pattern, ...)
  df[vars]
}

# Subset rows -------------------------------------------------------------

#' Remove rows with missing values
#'
#' Similar to `tidyr::drop_na`
#' @param df Data frame
#' @param cols Columns to search for missing values. Enter as character vector. Default uses all columns
#' @returns `df` without rows containing missing values in the columns specified by cols argument
#' @export
remove_na <- function(df, cols = names(df)) df[complete.cases(df[, cols, drop = FALSE]), ]

#' Remove rows which contain missing values in every column
#'
#' @param df Data frame
#' @param ... Columns to be searched for missing values. Enter using tidyselect syntax. If empty, all columns searched
#' @returns `df` without rows containing exclusively NA in the columns specified by `...`
#' @export
remove_empty_rows <- function(df, ...) {
  df_subset <- if (n_dots(...) > 0) dplyr::select(df, ...) else df
  rows_all_na <- apply(df_subset, 1, function(x) all(is.na(x)))
  df[!rows_all_na, ]
}
