# get_input ---------------------------------------------------------------

#' Get input to function if evaluation is possible, otherwise get input as character
#'
#' @param .input Input. Can be piped
#' @returns If .input is `NULL` or can be evaluated to a vector, output is same as input. Otherwise, output is a character representing the typed input. Objects in global environment are ignored
#' @export
get_input <- function(.input) {
  input <- tryCatch(suppressWarnings(force(.input)), error = function(e) "try-error")
  if (is.null(input)) return(NULL)
  if (is.vector(input) && !identical(input, "try-error")) return(input)
  input_substituted <- substitute(.input)
  for (i in seq_len(sys.nframe())) {
    if (typeof(input_substituted) == "language") return(deparse(input_substituted, width.cutoff = 500L))
    input_substituted <- do.call("substitute", list(as.name(input_substituted), parent.frame(i)))
  }
  as.character(input_substituted)
}

#' Get name of first term in a pipe argument
#'
#' @param x Input to function (piped or unpiped. Can't be a number or a call)
#' @returns `x` as a character
#' @export
get_piped_input <- function(x) {
  calls <- sys.calls()
  call_firsts <- lapply(calls, `[[`, 1)
  pipe_calls <- vapply(call_firsts, function(z) {
    identical(z, quote(`%>%`)) || identical(z, quote(`|>`))
  }, logical(1))
  if (all(!pipe_calls)) {
    input <- rlang::ensym(x)
  } else {
    pipe_calls <- which(pipe_calls)
    pipe_calls <- pipe_calls[length(pipe_calls)]
    this_call <- calls[[c(pipe_calls, 2)]]
    while (is.call(this_call) && (identical(this_call[[1L]], quote(`%>%`)) || identical(this_call[[1L]], quote(`|>`)))) {
      this_call <- this_call[[2L]]
    }
    input <- this_call
  }
  rlang::as_name(input)
}

#' Determine whether input was piped
#'
#' @param n Number of layers deep in call stack. Default is `1`. Enter as numeric
#' @returns Call `is_input_piped()` inside function to determine whether input to function was piped
#' @export
is_input_piped <- function(n = 1) {
  last_call <- as.character(sys.call(n))
  any(grepl(pattern = "%>%", x = last_call, fixed = TRUE))
}

# Dots --------------------------------------------------------------------

#' Collect dots as quoted terms
#'
#' @param ... Comma separated list of quoted or unquoted terms
#' @param .use_names Whether to use names. Default is `FALSE`
#' @returns Character vector (terms entered in dots are not evaluated/interpreted)
#' @export
dots_as_quoted <- function(..., .use_names = FALSE) {
  unlist(lapply(eval(substitute(alist(...))), function(x) {
    gsub("\"", "", deparse(x, width.cutoff = 500L))
  }), use.names = .use_names)
}

#' Collect dots as unquoted terms
#'
#' @param ... Comma separated list of quoted or unquoted terms
#' @param .use_names Whether to use names. Default is `FALSE`
#' @returns List of unquoted terms (no interpretation/evaluation)
#' @export
dots_as_unquoted <- function(..., .use_names = FALSE) {
  # Alternative 1: eval(substitute(alist(...)), envir = parent.frame())
  # Alternative 2: as.list(substitute(list(...)))[-1]
  unlist(rlang::eval_bare(substitute(alist(...))), use.names = .use_names)
}

#' Determine number of terms entered as ...
#'
#' @param ... Dots
#' @export
n_dots <- function(...) ...length()

#' Determine names of dots
#'
#' @param ... Dots
#' @returns Character vector of named elements in dots. If unnamed, result is `NA`
#' @export
dots_names <- function(...) ...names()

# Error handling ----------------------------------------------------------

#' Suppress messages and warnings
#'
#' @param ... Command
#' @export
suppress <- function(...) suppressMessages(suppressWarnings(...))

#' Try to perform evaluation silently
#'
#' @param x Expression
#' @param otherwise Return value if `x` can't be evaluated. Default is `NULL`
#' @returns Value of `x` or otherwise
#' @export
Try <- function(x, otherwise = NULL) tryCatch(suppressWarnings(x), error = function(e) otherwise)

#' Alias for `Try(x, otherwise = NULL)`
#'
#' @rdname Try
#' @export
tryNULL <- function(x) tryCatch(suppressWarnings(x), error = function(e) NULL)

#' Determine if object is a try-error
#'
#' @param x Object
#' @returns Length 1 logical
#' @export
is_error <- function(x) inherits(x, "try-error")

#' Warning about input class
#'
#' @param fn Parent function. Enter as quoted function name
#' @param x Input to `fn`
#' @noRd
.warn_input_class <- function(fn, x) {
  warning(sprintf("Unable to run %s() with input of class ", fn), paste(shQuote(class(x)), collapse = ", "), call. = FALSE)
  NULL
}

#' Get objects stored in another R work space
#'
#' @param ... Path to directory. Arguments passed to `paste_path()`
#' @param obj Name of object in work space. Enter as vector or unquoted list of object names (input does not need to be wrapped in `c()`). If `NULL` (default), output includes all objects in the work space
#' @returns If a single object is entered, class of output is determined by object entered. If  multiple objects entered or `obj = NULL`, output is a list of objects
#' @export
get_from_workspace <- function(..., obj = NULL) {
  obj <- get_input(obj)
  path <- paste0(paste_path(...), "/.RData")
  if (!file.exists(path)) stop("The following directory was not identified:\n\n", dirname(path))
  new_env <- new.env()
  load(file = path, envir = new_env)
  if (is.null(obj)) {
    as.list(new_env)
  } else {
    new_env[[obj]]
  }
}
