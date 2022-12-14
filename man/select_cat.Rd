% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_subset.R
\name{select_cat}
\alias{select_cat}
\title{Data frame with categorical columns only}
\usage{
select_cat(df, max_n_unique = 10, invert = FALSE)
}
\arguments{
\item{df}{Data frame}

\item{max_n_unique}{Maximum number of unique values, below which variable assumed to be categorical}

\item{invert}{If \code{TRUE}, output includes variables for which the predicate is \code{FALSE}}
}
\description{
Data frame with categorical columns only
}
