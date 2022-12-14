% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{combine_vars}
\alias{combine_vars}
\title{Add new categorical variable to data frame by pasting values from existing columns together}
\usage{
combine_vars(
  df,
  ...,
  add_colname_prefix = TRUE,
  sep_colname_value = " = ",
  sep_cols = ", ",
  new_colname = "group",
  as_factor = FALSE
)
}
\arguments{
\item{df}{Data frame}

\item{...}{Comma separated list of quoted column names or a character vector of column names to combine}

\item{add_colname_prefix}{If \code{TRUE} (default), values in each column are prefixed by column names}

\item{sep_colname_value}{Symbol used to separate column name and corresponding values. Default is \code{" = "}. Only relevant when \code{add_colname_prefix = TRUE}}

\item{sep_cols}{Symbol used to separate columns. Default is \code{", "}}

\item{new_colname}{Column name for new variable. Default is \code{"group"}}

\item{as_factor}{If \code{TRUE}, new variable converted to a factor}
}
\value{
\code{df} with new categorical variable containing combination of values in columns defined in \code{...}
}
\description{
Add new categorical variable to data frame by pasting values from existing columns together
}
