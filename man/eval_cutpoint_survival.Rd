% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival.R
\name{eval_cutpoint_survival}
\alias{eval_cutpoint_survival}
\title{Evaluate specific cutpoint}
\usage{
eval_cutpoint_survival(
  df,
  predictor_var,
  cp,
  time_var = "time",
  outcome_var = "death",
  type = ">=",
  cutpoint = cp
)
}
\arguments{
\item{df}{Data frame}

\item{predictor_var}{Continuous predictor variable. Enter as quoted or unquoted variable name}

\item{cp}{Cutpoints to be evaluated. Enter as numeric vector}

\item{time_var}{Variable containing follow up time. Enter as quoted or unquoted variable name. Default is \code{"time"}}

\item{outcome_var}{Variable containing outcome variable coded as \code{1} (event) or \code{0} (no event/censored). Enter as quoted or unquoted variable name. Default is \code{"death"}}

\item{type}{If \code{>=} (default), positivity is determined by values greater than or equal to cutpoint.  If \code{>}, positivity is determined by values of predictor_var greater than cutpoint}

\item{cutpoint}{Alias for \code{cp}}
}
\value{
Data frame with 1 row for each cutpoint and columns for outcome_var, time_var, predictor_var, type, cutpoint, hr, hr_lower, hr_upper, p_lrt, label, p_wald, n, pos, neg, perc_pos, perc_neg, n_events
}
\description{
Evaluate specific cutpoint
}
