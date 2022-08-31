#' Extract function including
#'
#' Slightly faster version of `match.fun`
#' @param fn Function. Enter function definition, quoted or unquoted function name
#' @returns Function
#' @export
match_fun <- function(fn) {
  if (is.function(fn)) return(fn)
  if (!(is.character(fn) && length(fn) == 1L || is.symbol(fn))) {
    fn <- eval.parent(substitute(substitute(fn)))
  }
  env <- parent.frame(2)
  get(as.character(fn), mode = "function", envir = env)
}
