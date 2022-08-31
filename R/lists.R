# Create lists ------------------------------------------------------------

#' List create with self-referential components
#'
#' @param ... Input to list. Enter as named vectors
#' @returns List
#'
#' @examples
#' \dontrun{
#' # list(a = 1:10, b = a*5) # Doesn't work
#' build_list(a = 1:10, b = a*5) # Works
#' }
#' @export
build_list <- function(...) {
  dots <- eval(substitute(alist(...)))
  x <- list()
  for (i in seq_along(dots)) {
    if (is.null(names(dots)[i])) {
      x[[i]] <- eval(dots[[i]], envir = x)
    } else {
      x[[names(dots)[i]]] <- eval(dots[[i]], envir = x, enclos = parent.frame())
    }
  }
  x
}

# Edit lists --------------------------------------------------------------

#' Remove NULL values from a list
#'
#' @param x List
#' @returns List without `NULL` or other length 0 elements
#' @export
remove_null <- function(x) x[!vapply(x, function(z) length(z) == 0, FUN.VALUE = logical(1), USE.NAMES = FALSE)]

#' Unlist without keeping names
#'
#' @param x List, data frame
#' @param recursive If `TRUE` (default), sublists are unlisted
#' @returns Unlisted and unnamed contents of x
#' @export
Unlist <- function(x, recursive = TRUE) unlist(x, recursive = recursive, use.names = FALSE)

#' Update list
#'
#' Functionality from `modifyList` function in utils package
#' @param old,new Old and new lists respectively
#' @returns Updated list in which named elements shared by both lists will be updated by replacing the named element in `old` with the matching named element in `new`. If element named is present in both lists and is `NULL` in `new`, it will be removed from updated list. Named elements in `new` that do not have a matching named element in `old` will be added to `new`
#' @export
update_list <- function(old, new) {
  old_names <- names(old)
  new_names <- names(new)
  new_names <- new_names[nzchar(new_names)]
  for (i in new_names) {
    old[[i]] <- if (i %in% old_names && is.list(.subset2(old, i)) && is.list(.subset2(new, i))) {
      update_list(.subset2(old, i), .subset2(new, i))
    } else {
      .subset2(new, i)
    }
  }
  old
}

#' Transpose a list
#'
#' @param x List
#' @returns Transposed list
#' @export
transpose_list <- function(x) do.call(Map, c(c, x, USE.NAMES = FALSE))
