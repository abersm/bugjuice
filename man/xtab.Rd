% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{xtab}
\alias{xtab}
\alias{tab}
\title{Contingency table}
\usage{
xtab(df, ..., incl_na = FALSE)

tab(df, ..., incl_na = FALSE)
}
\arguments{
\item{df}{Data frame}

\item{...}{Variables to create table. Enter as character vector, comma separated list of unquoted or unquoted column names, or formula}

\item{incl_na}{If \code{FALSE} (default), NA values are not included in table. If \code{TRUE}, NA is included in table}
}
\description{
Contingency table

Alias for xtab
}
