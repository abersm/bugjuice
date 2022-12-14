% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_explode}
\alias{str_explode}
\alias{str_reverse}
\title{List all characters in a string}
\usage{
str_explode(x, simplify = FALSE)

str_reverse(x)
}
\arguments{
\item{x}{Character vector}

\item{simplify}{If \code{FALSE} (default), output will be list of length equal to length of input. Each component of list will include all characters in each string. If \code{TRUE} and the length of \code{x} is > 1, then only the first letter of each component of \code{x} will be returned as a character vector}
}
\description{
List all characters in a string

Reverse the order of letters in a string
}
