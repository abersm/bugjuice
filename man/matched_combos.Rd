% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinations.R
\name{matched_combos}
\alias{matched_combos}
\alias{matched_pairs}
\title{Combine multiple vectors by matching positions into list of element-wise combinations}
\usage{
matched_combos(...)

matched_pairs(...)
}
\arguments{
\item{...}{Vectors. All must have same length}
}
\value{
List of vectors (each with length equal to the number of vectors entered) containing element-wise combinations (i.e. 1st element in each vector in 1st element of output list, 2nd element in each vector in 2nd element of output list, etc.)
}
\description{
Matching elements from each vector paired (element 1 from vector 1 paired with element 1 from vector 2)
}
