% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{trend_summary}
\alias{trend_summary}
\title{Determine number/percent of increasing or decreasing between adjacent values}
\usage{
trend_summary(x, na.rm = TRUE)
}
\arguments{
\item{x}{Numeric vector}

\item{na.rm}{If \code{TRUE} (default), missing values are removed}
}
\value{
List containing n_deltas, n_plateaus, n_increase, n_decreases
}
\description{
Determine number/percent of increasing or decreasing between adjacent values
}
