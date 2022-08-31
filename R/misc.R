#' Convert unquoted terms not wrapped in c() to comma separated list of quoted terms wrapped in c()
#'
#' If terms not entered, clipboard will be used. In that case, terms can be unquoted text separated by spaces or commas or unquoted terms with 1 term per line
#' @param ... Input to be quoted. If empty, clipboard contents will be used
#' @param with_c If `TRUE` (default), quoted terms wrapped in `c()`. If `FALSE`, output limited to contents inside of `c()`. Only relevant when terms entered in `...`
#' @param copy If `TRUE` (default), output is copied to clipboard
#' @returns Comma separated list of quoted terms. If `with_c` is `TRUE`, terms wrapped in `c()`
#' @export
paste_quoted <- function(..., with_c = TRUE, copy = TRUE) {
  if (missing(...)) {
    x <- paste_clipboard()
    x <- if (grepl(pattern = ",", x = x, fixed = TRUE)) strsplit(x, ",") else strsplit(x, " ")
    x <- trimws(x[[1L]], which = "both")
  } else {
    x <- c(...)
  }
  x <- noquote(x)
  x <- paste(shQuote(x, type = "cmd"), collapse = ", ")
  x <- if (with_c) sprintf("c(%s)", x) else x
  if (copy) {
    copy_to_clipboard(x)
  }
  cat(x)
}

#' Convert character vector into a comma separated list of unquoted terms
#'
#' @param ... Input to be transformed
#' @param with_c If `FALSE` (default), output not wrapped in `c()`. If `TRUE`, output wrapped in `c()`
#' @param copy If `TRUE` (default), output is copied to clipboard
#' @returns Comma separated list of unquoted terms. If `with_c` is `TRUE`, terms wrapped in `c()`
#' @export
paste_unquoted <- function(..., with_c = FALSE, copy = TRUE) {
  if (missing(...)) {
    x <- paste_clipboard()
    x <- if (grepl(",", x, fixed = TRUE)) strsplit(x, ",") else strsplit(x, " ")
    x <- trimws(x[[1L]], which = "both")
    unquoted_list <- paste(x, collapse = ", ")
  } else {
    n_terms <- as.list(...)
    unquoted_list <- if (length(n_terms) == 1L) paste(..., sep = ", ") else paste(..., collapse = ", ")
  }
  unquoted_list <- if (with_c) sprintf("c(%s)", unquoted_list) else unquoted_list
  if (copy) {
    copy_to_clipboard(unquoted_list)
  }
  cat(unquoted_list)
}

#' Convert comma separated quoted terms to vertical unquoted terms
#'
#' @param ... Enter as comma separated list of quoted terms without `c()`
#' @param suffix Suffix to include on each element in output except last. Can enter `","` or `" = ,"`
#' @param copy If `TRUE` (default), output is copied to clipboard
#' @returns Unquoted terms listed vertically
#' @export
paste_vertical <- function(..., suffix = NULL, copy = TRUE) {
  dots <- paste0(..., suffix)
  if (copy) {
    copy_to_clipboard(paste(dots, collapse = "\n"))
  }
  cat(dots, sep = "\n")
}

# Clipboard ---------------------------------------------------------------

#' Copy entered text to clipboard in mac
#'
#' @param ... Contents to copy to clipboard
#' @export
copy_to_clipboard <- function(...) {
  pkg_required("clipr")
  clipr::write_clip(...)
}

#' Paste clipboard contents in mac
#'
#' @returns Clipboard contents
#' @export
paste_clipboard <- function() {
  pkg_required("clipr")
  clipr::read_clip()
}

# Other -------------------------------------------------------------------

#' Determine number of CPU cores
#'
#' Functionality from detectCores function in parallel package
#' @returns Length 1 integer containing number of cores available (including R)
#' @export
n_cores <- function() {
  systems <- c(
    linux = "grep \"^processor\" /proc/cpuinfo 2>/dev/null | wc -l",
    darwin = "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null",
    solaris = "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l",
    freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null",
    openbsd = "/sbin/sysctl -n hw.ncpuonline 2>/dev/null")
  os <- c("linux", "darwin", "solaris", "freebsd", "openbsd")
  os_command <- pmatch(os, R.version$os)
  os_command <- os[!is.na(os_command)]
  if (length(os_command) > 0L) {
    os_command <- systems[os_command]
    a <- tryCatch(suppressWarnings(system(os_command, intern = TRUE)), error = function(e) NULL)
    if (!is.null(a)) {
      a <- gsub("^ +", "", a[1L])
      if (grepl("^[1-9]", a)) return(as.integer(a))
    }
  }
  NA_integer_
}
