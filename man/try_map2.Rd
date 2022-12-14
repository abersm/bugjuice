% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loops.R
\name{try_map2}
\alias{try_map2}
\title{Silent version of mapply/map2}
\usage{
try_map2(
  .x,
  .y,
  .f,
  ...,
  .otherwise = NULL,
  .remove_null = FALSE,
  .use_names = FALSE
)
}
\arguments{
\item{.x, .y}{Input to mapping function}

\item{.f}{Function to apply to each element of \code{.x}}

\item{...}{Arguments passed to \code{.f}}

\item{.otherwise}{Value to be returned if output of \code{.f} is an error. Default is \code{NULL}}

\item{.remove_null}{If \code{TRUE}, \code{NULL} values are removed from output}

\item{.use_names}{If \code{TRUE}, output can be named}
}
\description{
Silent version of mapply/map2
}
