% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_axis.R
\name{scale_x10}
\alias{scale_x10}
\title{Programmatically create simplified x10^n axis scale for ggplot}
\usage{
scale_x10(
  axis = "y",
  title = waiver(),
  limits = waiver(),
  breaks = waiver(),
  labels = waiver(),
  expand_lower = 0,
  expand_upper = 0,
  max_digits = 3,
  n_breaks = 4,
  position = NULL,
  censor_fn = rescale_none,
  breaks_fn = pretty,
  show_parentheses = TRUE,
  add_space = TRUE,
  ...
)
}
\arguments{
\item{axis}{Options include \code{"y"} (default) or \code{"x"}}

\item{title}{Axis title}

\item{limits}{Minimum and maximum values in data. Must enter in order, i.e. c(lower, upper)}

\item{breaks}{Numeric vector or function specifying location of ticks along axis}

\item{labels}{Vector or function specifying axis tick labels}

\item{expand_lower, expand_upper}{Expansion around lower and upper range of axis. Default is \code{0.1} for \code{expand_lower} and \code{0} for \code{expand_upper}}

\item{max_digits}{Maximum number of digits allowed for maximum break. Default is \code{3} (all breaks must be < 1000)}

\item{n_breaks}{Desired number of axis breaks. Default is \code{4}}

\item{position}{Location of axis. Options: \code{"left"} (default for y axis), \code{"right"}, "bottom" (default for x axis), "top"}

\item{censor_fn}{Function used to transform data outside axis limits. Default is \code{rescale_none.} Alternative: \code{scales::censor}}

\item{breaks_fn}{Function used to create breaks. Enter as call using (). Default is \code{pretty}. Alternative is \code{scales::breaks_extended(n = n_breaks, only.loose = TRUE)}}

\item{show_parentheses}{If \code{TRUE} (default), x10^n multiplier is wrapped in \code{"()"}}

\item{add_space}{If \code{TRUE} (default), space is added between multiplication symbol and 10^n}

\item{...}{Arguments passed to scale function}
}
\value{
Enter as\code{ plot + scale_x10()}
}
\description{
Programmatically create simplified x10^n axis scale for ggplot
}
