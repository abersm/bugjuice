% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{split_df}
\alias{split_df}
\title{Split data frame by unique values across 1 or more columns}
\usage{
split_df(df, ...)
}
\arguments{
\item{df}{Data frame}

\item{...}{Columns used to split data frame. Enter using tidyselect syntax}
}
\value{
List of data frames with 1 data frame for each unique combination of values for columns selected in dots
}
\description{
Split data frame by unique values across 1 or more columns
}
