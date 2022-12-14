% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{is_zero}
\alias{is_zero}
\alias{is_integerish}
\alias{is_continuous}
\alias{any_na}
\alias{all_na}
\title{Determine whether input value is zero}
\usage{
is_zero(x)

is_integerish(x)

is_continuous(x)

any_na(x)

all_na(x)
}
\arguments{
\item{x}{Numeric vector}
}
\value{
Logical vector with length equal to input. If \code{x == 0}, output is \code{TRUE.} If \code{x != 0}, output is \code{FALSE.} If \code{is.na(x)}, output is \code{NA}

Logical vector with length equal to input
}
\description{
Determine whether input value is zero

Determine whether numeric values are nearly equivalent to integers

Determine whether a numeric vector is continuous

Alias for anyNA

Check if vector contains only missing values
}
