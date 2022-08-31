#' Not in operator
#'
#' @param lhs,rhs left and right hand side of operator, respectively
#' @name notin
#' @rdname not-in
#' @export
`%!in%` <- function(lhs,rhs) !`%in%`(lhs,rhs)

#' Null operator
#'
#' @param x Object for which `NULL` status will be determined
#' @param y Result if x is `NULL`
#' @name nullop
#' @rdname null-op
#' @export
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Waiver operator
#'
#' Functionality from Hadley Wickham's excellent package ggplot2
#' @param x Object possibly of class "waiver"
#' @param y Value to return if `x` is a waiver
#' @returns Output is `x` unless it is a "waiver" object in which case `y` is returned
#' @export
`%W%` <- function(x, y) if (is_waiver(x)) y else x
