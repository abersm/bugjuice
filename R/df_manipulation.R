# Change column order -----------------------------------------------------

#' Change column order
#'
#' @param df Data frame
#' @param ... Columns to move. Enter using tidyselect syntax
#' @param before,after Where to place columns selected in `...` Enter as quoted or unquoted column name. Use before to place `...` columns before columns selected by before. Use after to place `...` columns after columns selected by after. If both are `NULL`, `...` columns are placed before all other columns
#' @import dplyr
#' @export
move_cols <- function(df, ..., before = NULL, after = NULL) {
  cols <- names(dplyr::select(df, ...))
  var_numbers <- seq_along(df)
  names(var_numbers) <- df_names <- names(df)
  before <- get_input(before)
  after <- get_input(after)
  before_entered <- !is.null(before)
  after_entered <- !is.null(after)
  if (before_entered && after_entered) {
    stop("Only 'before' or 'after' can be specified in 'move_cols()', not both", call. = FALSE)
  }
  if (before_entered) {
    before <- Setdiff(before, cols)
    first_part <- df_names[var_numbers < var_numbers[before]]
    first_part <- Setdiff(first_part, cols)
    df[c(first_part, cols, before, Setdiff(df_names, c(first_part, cols, before)))]
  } else if (after_entered) {
    after <- Setdiff(after, cols)
    last_part <- df_names[var_numbers > var_numbers[after]]
    last_part <- Setdiff(last_part, cols)
    df[c(Setdiff(df_names, c(after, cols, last_part)), after, cols, last_part)]
  } else {
    df[c(cols, Setdiff(df_names, cols))]
  }
}

# Change variable class ---------------------------------------------------

#' If a column is coercible to a numeric, convert to numeric
#'
#' @param df Data frame
#' @returns Data frame with same number of columns as input, but all columns that are coercible to a numeric will be converted to numeric
#' @export
df_as_numeric <- function(df) {
  dplyr::mutate(df, dplyr::across(.cols = where(~all(can_be_numeric(.x))), .fns = as.numeric))
}

# Modify values -----------------------------------------------------------

#' Replace missing values
#'
#' @param df Data frame or vector
#' @param ... Columns to replace `NA` with `new_value`. Enter using tidyselect syntax. Default uses all columns. Only relevant when `df` is a data frame
#' @param new_value Value used to replace missing values. Default is `0`
#' @export
replace_na <- function(df, ..., new_value = 0) {
  if (is.data.frame(df)) {
    df_names <- if (n_dots(...) == 0L) names(df) else names(dplyr::select(df, ...))
    for (i in df_names) {
      df[[i]][is.na(.subset2(df, i))] <- new_value
    }
    df
  } else {
    df[is.na(df)] <- new_value
    df
  }
}

# Rowwise -----------------------------------------------------------------

#' Row-wise summary statistic
#'
#' @param df Data frame
#' @param fn Summary function. Must take a vector of values and return a single number
#' @param ... Columns used in calculation. Enter using tidyselect syntax. Default uses all columns
#' @param new_col_name Name for column containing row-wise statistic. Default is `"row_value"`
#' @returns Data frame with column for row-wise summary statistic
#' @export
row_apply <- function(df, fn, ..., new_col_name = "row_value") {
  df_cols <- if (n_dots(...) == 0L) df else dplyr::select(df, ...)
  df[[new_col_name]] <- apply(df_cols, 1, fn)
  df
}

#' Transform subset of columns by row
#'
#' @param df Data frame
#' @param fn Function used to transform values
#' @param ... Columns to transform. Enter using tidyselect syntax
#' @returns Data frame with row-wise transformed values in columns specified by `...`
#' @export
transform_by_row <- function(df, fn, ...) {
  vars <- names(dplyr::select(df, ...))
  if (length(vars) == 0L) {
    vars <- names(df)
  }
  vars <- vars_numeric(df[vars])
  df[vars] <- matrix_to_df(t.default(apply(df[vars], 1, fn)))
  df
}

# Create new variable -----------------------------------------------------

#' Add column for row number
#'
#' @param df Data frame or grouped_df
#' @param new_colname Column name for id variable. Default is `"id"`
#' @returns Data frame with new column for id variable. If `df` is a grouped data frame, rows will be numbered separately for each group
#' @export
add_id_var <- function(df, new_colname = "id") {
  new_colname <- .safe_name(.new = new_colname, .old = names(df), .sep = "")
  if (inherits(df, "grouped_df")) {
    df <- dplyr::mutate(df, "{new_colname}" := 1:n())
    dplyr::ungroup(df)
  } else {
    df[[new_colname]] <- seq_len(.row_names_info(df, 2L))
    df
  }
}

#' Add new categorical variable to data frame by pasting values from existing columns together
#'
#' @param df Data frame
#' @param ... Comma separated list of quoted column names or a character vector of column names to combine
#' @param add_colname_prefix If `TRUE` (default), values in each column are prefixed by column names
#' @param sep_colname_value Symbol used to separate column name and corresponding values. Default is `" = "`. Only relevant when `add_colname_prefix = TRUE`
#' @param sep_cols Symbol used to separate columns. Default is `", "`
#' @param new_colname Column name for new variable. Default is `"group"`
#' @param as_factor If `TRUE`, new variable converted to a factor
#' @returns `df` with new categorical variable containing combination of values in columns defined in `...`
#' @export
combine_vars <- function(df, ..., add_colname_prefix = TRUE, sep_colname_value = " = ", sep_cols = ", ", new_colname = "group", as_factor = FALSE) {
  vars <- c(...)
  df_cols <- df[vars]
  if (add_colname_prefix) {
    for (i in names(df_cols)) {
      df_cols[[i]] <- sprintf("%s%s%s", i, sep_colname_value, .subset2(df_cols, i))
    }
  }
  z <- do.call(paste, c(df_cols, sep = sep_cols))
  name <- .safe_name(.new = new_colname, .old = names(df), .sep = "")
  if (new_colname != name) {
    warning(sprintf("In combine_vars(), new_colname = '%s' but this column already exists in df.\n\nSetting new column name to '%s'", new_colname, name), call. = FALSE)
  }
  df[[name]] <- if (as_factor) factor(z, create_levels(z)) else z
  df
}


#' Add new factor variable to data frame by pasting values from existing columns together
#'
#' @param df Data frame
#' @param ... Columns to paste together. Enter using tidyselect syntax
#' @param add_colname_prefix If `TRUE` (default), values in each column are prefixed by column names
#' @param sep_colname_value Symbol used to separate column name and corresponding values. Default is `" = "`. Only relevant when `add_colname_prefix = TRUE`
#' @param sep_cols Symbol used to separate columns. Default is `", "`
#' @param new_colname Column name for new variable. Default is `"group"`
#' @param reverse If `TRUE`, levels of output variable are reversed in `create_levels()`
#' @returns `df` with new variable for combination of values in columns defined in `...`
#' @export
fct_cross <- function(df, ..., add_colname_prefix = TRUE, sep_colname_value = " = ", sep_cols = ", ", new_colname = "group", reverse = FALSE) {
  df_cols <- dplyr::select(df, ...)
  if (add_colname_prefix) {
    for (i in names(df_cols)) {
      df_cols[[i]] <- sprintf("%s%s%s", i, sep_colname_value, .subset2(df_cols, i))
    }
  }
  z <- do.call(paste, c(df_cols, sep = sep_cols))
  name <- .safe_name(.new = new_colname, .old = names(df), .sep = "")
  if (new_colname != name) {
    warning(sprintf("In fct_cross(), new_colname = '%s' but this column already exists in df.\n\nSetting new column name to '%s'", new_colname, name), call. = FALSE)
  }
  df[[name]] <- factor(z, create_levels(z, reverse = reverse))
  df
}

# Other -------------------------------------------------------------------

#' Transpose a data frame
#'
#' @param df Data frame
#' @param col_names Character vector of column names in transposed data frame
#' @returns Transposed data frame. If any input variable is a character, all output columns will be characters
#' @export
transpose_df <- function(df, col_names = "variable") {
  df_names <- names(df)
  # Dont use df_to_matrix as rownames will be removed
  df <- matrix_to_df(t.data.frame(df))
  df$variable <- df_names
  df <- df[c("variable", Setdiff(names(df), "variable"))]
  names(df)[seq_along(col_names)] <- col_names
  df
}

#' Split data frame by unique values across 1 or more columns
#'
#' @param df Data frame
#' @param ... Columns used to split data frame. Enter using tidyselect syntax
#' @returns List of data frames with 1 data frame for each unique combination of values for columns selected in dots
#' @export
split_df <- function(df, ...) {
  vars <- names(dplyr::select(df, ...))
  lapply(split.default(seq_nrow(df), f = as.list(df[vars]), drop = FALSE), function(x) df[x, , drop = FALSE])
}

#' Create empty data frame
#'
#' @param ... Enter as comma separated list of empty vectors (i.e. x = numeric(), y = character(), etc.)
#' @param df_template Data frame containing columns to be included in empty data frame. If `NULL` (default), output will be data frame with no columns
#' @returns Data frame with no rows. If `df_template` is entered, output will have matching column names, otherwise output will have no columns
#' @noRd
empty_df <- function(..., df_template = NULL) {
  if (!is.null(df_template)) return(df_template[FALSE, ])
  if (n_dots(...) == 0L) {
    structure(list(), class = "data.frame", row.names = integer(), names = character())
  } else {
    df <- list(...)
    class(df) <- "data.frame"
    attr(df, "row.names") <- integer()
    df
  }
}
