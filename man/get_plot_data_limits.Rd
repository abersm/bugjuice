% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_info.R
\name{get_plot_data_limits}
\alias{get_plot_data_limits}
\title{Data limits for all data frames used to generate plot}
\usage{
get_plot_data_limits(x, axis = "both")
}
\arguments{
\item{x}{ggplot object}

\item{axis}{Options: \code{"both"} (default), \code{"x"}, \code{"y"}}
}
\value{
List containing \code{x} and \code{y}, each a length 2 numeric vector with limits in raw data units
}
\description{
Data limits for all data frames used to generate plot
}
