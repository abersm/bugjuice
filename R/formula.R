#' Check if input is a formula
#'
#' @param x Input
#' @export
is.formula <- function(x) {
  tryCatch(inherits(x, "formula") || (is.call(x) && x[[1]] == "~"), error = function(e) FALSE)
}

#' Coerce string to formula
#'
#' @param x Formula as a string
#' @param env Environment of formula. Default is `parent.frame()`
#' @returns Formula
#' @export
as_formula <- function(x, env = parent.frame()) {
  x <- str2lang(x)
  class(x) <- "formula"
  environment(x) <- env
  x
}

#' Create formula
#'
#' @param lhs,rhs Quoted variable name or comma separated list of quoted variable names
#' @export
create_formula <- function(lhs, rhs) {
  formula <- paste(paste(lhs, collapse = " + "), paste(rhs, collapse = " + "), sep = " ~ ")
  as_formula(formula, env = parent.frame())
}

#' #' Extract x and y variables from formula or x/y inputs
#'
#' @param formula Formula in y ~ x format. Left-hand side cannot contain > 1 variable
#' @param x,y Character vectors of x and y variables
#' @param parent_fn Name of function called. Enter as quoted function name
#' @return Formula or list containing "y" and "x", each a character vector of variables
#' @export
formula2vars <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(formula)) {
    if (!is.null(x) || !is.null(y)) {
      stop(sprintf("In %s(), must specify variables using either 'formula' (y ~ x format) or 'x'/'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y), call. = FALSE)
    }
    vars <- all.vars(formula)
    y <- vars[1L]
    x <- vars[-1L]
  }
  list(x = x, y = y)
}

#' #' Extract formula from formula or x/y inputs
#'
#' @rdname formula2vars
#' @export
vars2formula <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(x) || !is.null(y)) {
    if (!is.null(formula)) {
      stop(sprintf("In %s(), must specify variables using either 'formula' (y ~ x format) or 'x'/'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y), call. = FALSE)
    }
    formula <- create_formula(y, x)
  }
  formula
}

#' #' Extract formula and x and y variables from formula or x/y inputs
#'
#' @rdname formula2vars
#' @export
get_vars_formula <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(x) || !is.null(y)) {
    if (!is.null(formula)) {
      stop(sprintf("In %s(), must specify variables using either 'formula' (y ~ x format) or 'x'/'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y), call. = FALSE)
    }
    formula <- create_formula(y, x)
  } else {
    vars <- all.vars(formula)
    y <- vars[1L]
    x <- vars[-1L]
  }

  list(
    formula = formula,
    x = x,
    y = y
  )
}
