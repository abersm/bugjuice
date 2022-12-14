% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_transformation.R
\name{convert_rate_time}
\alias{convert_rate_time}
\title{Convert rates between units of time}
\usage{
convert_rate_time(
  x,
  time_from = c("seconds", "minutes", "hours", "days", "weeks", "months", "years"),
  time_to = c("seconds", "minutes", "hours", "days", "weeks", "months", "years"),
  time_multiplier = 1
)
}
\arguments{
\item{x}{Numeric vector}

\item{time_from}{Time units for \code{x}. Options: \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"}, \code{"weeks"}, \code{"months"}, \code{"years"}}

\item{time_to}{Goal units for output. Options: \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"}, \code{"weeks"}, \code{"months"}, \code{"years"}}

\item{time_multiplier}{Multiplier for units of time in output (i.e. output will be divided by \code{time_multiplier}. If \verb{time_to = "weeks", time_multiplier = 2}, output is in units per 2 weeks). Default is \code{1}}
}
\value{
Rate in time units specified by \code{time_to}
}
\description{
Convert rates between units of time
}
