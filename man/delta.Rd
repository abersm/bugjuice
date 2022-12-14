% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_descriptive.R
\name{delta}
\alias{delta}
\title{Difference between sequential numbers in a vector}
\usage{
delta(x, lag = 1L)
}
\arguments{
\item{x}{Numeric vector}

\item{lag}{Difference between position of 2 numbers to determine difference between. Default is \code{1}}
}
\value{
Numeric vector with length equal to \code{length(x) - 1L}
}
\description{
Functionality from \code{diff()} function in base package
}
