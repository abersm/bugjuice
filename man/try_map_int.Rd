% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loops.R
\name{try_map_int}
\alias{try_map_int}
\title{Silent version of vapply for integer output (or map_int)}
\usage{
try_map_int(.x, .f, ..., .otherwise = NA_integer_, .use_names = FALSE)
}
\arguments{
\item{.x}{Input to mapping function}

\item{.f}{Function to apply to each element of \code{.x}}

\item{...}{Arguments passed to \code{.f}}

\item{.otherwise}{Value to be returned if output of \code{.f} is an error. Default is \code{NULL}}

\item{.use_names}{If \code{TRUE}, names of \code{.x} are applied to names of output}
}
\description{
Silent version of vapply for integer output (or map_int)
}
