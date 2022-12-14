% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lists.R
\name{build_list}
\alias{build_list}
\title{List create with self-referential components}
\usage{
build_list(...)
}
\arguments{
\item{...}{Input to list. Enter as named vectors}
}
\value{
List
}
\description{
List create with self-referential components
}
\examples{
\dontrun{
# list(a = 1:10, b = a*5) # Doesn't work
build_list(a = 1:10, b = a*5) # Works
}
}
