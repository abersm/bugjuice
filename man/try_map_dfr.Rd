% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loops.R
\name{try_map_dfr}
\alias{try_map_dfr}
\title{Silent version of map_dfr}
\usage{
try_map_dfr(.x, .f, ..., .otherwise = NULL, .prefix = "V")
}
\arguments{
\item{.x}{Input to mapping function}

\item{.f}{Function to apply to each element of \code{.x}}

\item{...}{Arguments passed to \code{.f}}

\item{.otherwise}{Value to be returned if output of \code{.f} is an error. Default is \code{NULL}}

\item{.prefix}{Prefix to use for new column names. Default is \code{"V"}. Enter as quoted prefix}
}
\description{
Silent version of map_dfr
}
