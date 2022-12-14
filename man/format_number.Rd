% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{format_number}
\alias{format_number}
\title{Format numbers for printing}
\usage{
format_number(x, digits = 2, max_width = 10, big_mark = "")
}
\arguments{
\item{x}{Numeric vector (can also be character or factor)}

\item{digits}{Number of digits to include after decimal. Default is \code{2}}

\item{max_width}{Maximum number of characters (including decimal places) allowed in output (including digits before decimal, decimal, and digits after decimal). Default is \code{10}}

\item{big_mark}{Character to separate thousands. Default is \code{""}}
}
\value{
Number as character rounded to specified number of digits after decimal
}
\description{
Format numbers for printing
}
