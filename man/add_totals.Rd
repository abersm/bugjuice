% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{add_totals}
\alias{add_totals}
\title{Add column and row totals to table or matrix}
\usage{
add_totals(x, title_row_totals = "Total", title_col_totals = "Total")
}
\arguments{
\item{x}{Table or array/matrix}

\item{title_row_totals}{Title of column containing row totals. Default is \code{"Total"}}

\item{title_col_totals}{Title of row containing column totals. Default is \code{"Total"}}
}
\value{
Matrix containing row and column sums as well as raw numbers in \code{x}. Number in bottom right hand corner of matrix contains overall total
}
\description{
Add column and row totals to table or matrix
}
