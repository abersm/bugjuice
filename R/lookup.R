# Lookup ------------------------------------------------------------------

#' Generic vlookup function
#'
#' @param x Data frame or vector to transform
#' @param lookup Mapping between old and new values. Enter 1 of the following
#'   * Data frame. Must contain columns specified by `"old"` and `"new"` arguments
#'   * Named vector. Names are current ("old") values in `x`. Values are replacement ("new") values
#'   For example, to replace lower case values with upper case values, `lookup` use c(a = "A", b = "B", etc.)
#' @param old,new Columns in `lookup` containing values that match `x` ("old") and replacement ("new") values, respectively. Only relevant when `lookup` is a data frame. Enter as quoted or unquoted column names
#' @param silent If `FALSE` (default), messages regarding irregularities in matching are reported
#' @param ... Not used
#' @returns Updated version of data frame or vector
#' @export
vlookup <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) UseMethod("vlookup", x)

#' vlookup - default method
#'
#' @rdname vlookup
#' @export
vlookup.default <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) {
  if (inherits(lookup, "data.frame")) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  } else {
    lookup_names <- names(lookup)
  }

  # Check for duplicate names of values in lookup
  if (any(duplicated(lookup_names))) {
    # Remove irrelevant lookup values
    lookup <- lookup[names(lookup) %in% x]
    lookup_names <- names(lookup)
    # Check whether duplicates are identical
    idx <- duplicated(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      stop("lookup contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"), call. = FALSE)
    }
  }

  # Check for values in x not present in lookup
  if (!silent && length(extra_vals <- Setdiff(unique.default(x[!is.na(x)]), lookup_names)) > 0) {
    message(paste0("The following values in x are not present in lookup[[old]] and will be replaced by NA: \n", paste(sQuote(extra_vals, FALSE), collapse = ", ")))
  }

  # New values
  new_values <- lookup[match(x, lookup_names)]
  names(new_values) <- NULL
  new_values
}

#' vlookup - factor
#'
#' @rdname vlookup
#' @export
vlookup.factor <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) {
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  } else {
    lookup_names <- names(lookup)
  }

  x_levels <- attr(x, "levels")

  # Check for duplicate names of values in lookup
  if (any(duplicated(lookup_names))) {
    # Remove irrelevant values from lookup
    lookup <- lookup[names(lookup) %in% x_levels]
    lookup_names <- names(lookup)
    # Check whether duplicates are identical
    idx <- duplicated(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      stop("lookup contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"), call. = FALSE)
    }
  }

  # Check for values in x not present in lookup
  if (!silent && length(extra_vals <- Setdiff(unique.default(x[!is.na(x)]), lookup_names)) > 0) {
    message(paste0("The following values in x are not present in lookup[[old]] and will be replaced by NA: \n", paste(sQuote(extra_vals, FALSE), collapse = ", ")))
  }

  # Goal: map levels(x) to lookup values. Want order of lookup values to match order of levels(x)
  # For each level of x, find location of match in lookup names
  idx <- match(x_levels, lookup_names)

  # Using location of lookup names matches, extract corresponding lookup values
  new_levels <- lookup[idx]

  # When a factor is used as an index, the integer representation of levels(x) is used. The result is an integer vector with the same length as x (the input factor) whose values represent the position in levels(x). Because the order of new_values matches levels(x), new_levels[x] replaces old values with new values
  new_values <- new_levels[x]
  names(new_values) <- NULL
  factor(new_values, levels = new_levels)
}

#' vlookup - data frame
#'
#' @rdname vlookup
#' @param col Variable in `x` to match to names of lookup vector. Enter as quoted or unquoted column name
#' @param new_col_name Name of new variable to add to `z`. If `NULL` (default), values in `x$col` are replaced. If column name is specified, a new column will be added to `x` using this name. Enter as quoted or unquoted column name
#' @export
vlookup.data.frame <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, col = NULL, new_col_name = NULL, ...) {
  col <- get_input(col)
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }
  if (is.null(col)) stop("For vlookup(), must select column to be modified using 'col' argument", call. = FALSE)
  new_col_name <- new_col_name %||% col
  x[[new_col_name]] <- vlookup(x[[col]], lookup = lookup, old = old, new = new, silent = silent)
  x
}

# Update values -----------------------------------------------------------

#' Generic update_values function
#'
#' Values in x are only replaced if included in names of lookup table
#' @param x Data frame or vector to transform
#' @param lookup Lookup table or named vector. If `lookup` is a vector, names of `lookup` correspond to values of `x` (i.e. old values) and values of `lookup` represent replacement values (i.e. new values). For example, to replace lower case values with upper case values, `lookup` is c(a = "A", b = "B", etc.)
#' @param old,new Column names of `lookup` containing values that match `x` and replacement values, respectively. Only relevant if `lookup` is a data frame. Enter as quoted or unquoted column names
#' @param ... Not used
#' @returns Updated version of data frame or vector
#' @export
update_values <- function(x, lookup, old = NULL, new = NULL, ...) UseMethod("update_values", x)

#' update_values - default method
#'
#' @rdname update_values
#' @export
update_values.default <- function(x, lookup, old = NULL, new = NULL, col = NULL, new_col_name = NULL, ...) {
  ## Consider placing ... after x to allow for update_values(x, a = "x", b = "y")
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }

  # Remove irrelevant values from lookup
  lookup <- lookup[names(lookup) %in% x]
  lookup_names <- names(lookup)

  # Check if any lookup_names are duplicated
  if (any(duplicated(lookup_names))) {
    # Check whether duplicates are identical
    idx <- duplicated(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      stop("lookup contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"), call. = FALSE)
    }
  }

  # New values
  idx <- match(x, lookup_names, nomatch = 0L)
  new_values <- lookup[idx]
  names(new_values) <- NULL
  x[idx > 0] <- new_values
  x
}

#' update_values - data frame
#'
#' @rdname update_values
#' @param col Var in `x` that matches names of `lookup` vector. Enter as quoted or unquoted column name
#' @param new_col_name Name of new variable to add to `x` If `NULL` (default), values in `x$col` are replaced. If column name is specified, a new column will be added to `x` using this name. Enter as quoted or unquoted column name
#' @export
update_values.data.frame <- function(x, lookup, old = NULL, new = NULL, col = NULL, new_col_name = NULL, ...) {
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }
  col <- get_input(col)
  if (is.null(col)) stop("Must select column to be modified using col argument", call. = FALSE)
  new_col_name <- new_col_name %||% col
  x[[new_col_name]] <- update_values.default(x[[col]], lookup = lookup, old = old, new = new)
  x
}

# Helpers -----------------------------------------------------------------

#' Process lookup, new, old arguments of a lookup functions
#'
#' @param x Values to be replaced
#' @param lookup Entry into function for lookup. Can be a function (replacement values formed by applying this function to `x`), named character vector (`names(lookup)` match values in `x`, values of `lookup` represent replacement values. For example, `lookup = c(a = "A", b = "B", etc.)` will replace lower case values with upper case values), or a data frame (with 1 column matching values in `x` and another column containing replacement values). If `lookup = NULL`, replacement will occur by matching values in `old` with the corresponding value in `new`
#' @param old Either a string containing column in `lookup` data frame which contains values that match values in `x` or a vector of values matching values in `x`. If `old` is a vector of values in `x`, it must have the same length as `new`. In this case replacement will occur by matching values in `old` with the corresponding value in `new` (i.e. 1st value in `new` replaces 1st value in `old`, 2nd value in `new` replaces 2nd value `old`, etc.)
#' @param new Either a string containing column in `lookup` data frame which contains replacement values or a vector containing replacement values. If `old` is a vector of values in `x`, it must have the same length as `new`. In this case replacement will occur by matching values in `old` with the corresponding value in `new` (i.e. 1st value in `new` replaces 1st value in `old`, 2nd value in `new` replaces 2nd value `old`, etc.)
#' @param fn_name Name of lookup function called
#' @returns List of length 2 containing character vectors of old and new values. Output is limited to relevant values only (i.e. value/replacement pairs containing values not present in `x` will be removed). If no usable value/replacement pairs are found, function will be stopped with an error
#' @noRd
.process_lookup <- function(x, lookup, old, new, fn_name) {
  if (is.function(lookup)) {
    old <- x
    new <- lookup(old)
  } else if (is.character(lookup)) {
    old <- names(lookup)
    if (is.null(old)) {
      stop(sprintf("In %s(), 'lookup' should be a named character vector with names matching old/current entries in 'x' and values representing replacements", fn_name), call. = FALSE)
    }
    new <- lookup
    names(new) <- NULL
  } else if (is.data.frame(lookup)) {
    old <- lookup[[old]]
    new <- lookup[[new]]
  }
  if (length(old) != length(new)) {
    stop(sprintf("In %s(), inputs 'old' and 'new' are not the same length", fn_name), call. = FALSE)
  }
  idx <- old %in% x
  if (sum(idx) == 0L) {
    lookup <- old
    old <- new
    new <- lookup
    idx <- old %in% x
    if (sum(idx) == 0L) {
      stop(sprintf("In %s(), 'lookup' does not contain any values in 'x'", fn_name), call. = FALSE)
    } else {
      stop(sprintf("In %s(), NAMES of 'lookup' values do not contain any values in 'x'. However, VALUES of 'lookup' do match values in 'x'.\nWere the entries for 'old' and 'new' accidentally switched?", fn_name), call. = FALSE)
    }
  }
  list(old = old[idx], new = new[idx])
}

#' Replace values in character or factor vector using lookup
#'
#' Functionality from ggplot2
#' @param x Character or factor vector
#' @param replacements Named character vector. Names match elements in `x` that will be replaced. Values are replacements
#' @returns Vector with same class as input
#' @export
replace_values <- function(x, replacements) {
  lookup <- names(replacements)
  if (is.character(x)) {
    replacements <- replacements[lookup %in% x]
    if (length(replacements) == 0L) return(x)
    x[match(lookup, x)] <- replacements
  } else {
    x_levels <- attr(x, "levels")
    replacements <- replacements[lookup %in% x_levels]
    if (length(replacements) == 0L) return(x)
    x_levels[match(lookup, x_levels)] <- replacements
    attr(x, "levels") <- x_levels
  }
  x
}
