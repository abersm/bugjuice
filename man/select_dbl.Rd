% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_subset.R
\name{select_dbl}
\alias{select_dbl}
\alias{select_numeric}
\title{Data frame with numeric columns only}
\usage{
select_dbl(df, incl_integer = TRUE, invert = FALSE)

select_numeric(df, incl_integer = TRUE, invert = FALSE)
}
\arguments{
\item{df}{Data frame}

\item{incl_integer}{If \code{TRUE} (default), integer variables are included}

\item{invert}{If \code{TRUE}, output includes variables for which the predicate is \code{FALSE}}
}
\description{
Data frame with numeric columns only

Alias for cols_dbl
}
