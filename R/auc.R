#' Area under the curve
#'
#' @param df Data frame containing dependent variable (`y`) and independent variable (`x`)
#' @param formula Formula as y ~ x
#' @param y,x Variables for y and x axis respectively. Enter as quoted or unquoted variable name
#' @param interval Minimum and maximum x values. Range is inclusive (i.e. [])
#' @param method Method used to calculate area under the curve. Options: `"trapezoidal"` (default), `"step"`, `"spline"`, `"all"`
#' @returns If method is `"trapezoidal"`, `"step"`, or `"spline"`, result is a length 1 numeric vector. If method is `"all"`, result is a data frame with 1 column for method and 1 column for auc
#' @export
auc <- function(df, formula = NULL, y = NULL, x = NULL, interval = NULL, method = c("trapezoidal", "step", "spline", "all")) {
  if (is.null(formula)) {
    y <- get_input(y)
    x <- get_input(x)
  } else {
    y <- all.vars(formula)
    x <- y[2]
    y <- y[1]
  }
  df <- df[c(x, y)]
  df <- df[complete.cases(df), ]
  df <- df[order(df[[x]]), ]
  interval <- interval %||% range(df[[x]])
  df <- df[df[[x]] >= interval[1] & df[[x]] <= interval[2], , drop = FALSE]
  y <- df[[y]]
  x <- df[[x]]
  n <- length(y)
  method <- match.arg(method, choices = c("trapezoidal", "step", "spline", "all"))
  switch(
    method,
    trapezoidal = {
      n_1 <- seq_len(n - 1)
      n <- 2:n
      sum((x[n] - x[n_1])*(y[n_1] + y[n]))/2
    },
    step = {
      sum(y[-n]*(x[-1] - x[-n]))
    },
    spline = {
      integrate(splinefun(x, y, method = "natural"), lower = min(x), upper = max(x))$value
    },
    all = {
      idx_1 <- seq_len(n - 1)
      idx <- 2:n
      auc_trap <- sum((x[idx] - x[idx_1])*(y[idx_1] + y[idx]))/2
      auc_step <- sum(y[-n]*(x[-1] - x[-n]))
      auc_spline <- integrate(splinefun(x, y, method = "natural"), lower = min(x), upper = max(x))$value
      vec_to_df(method = c("trapezoid", "step", "spline"), auc = c(auc_trap, auc_step, auc_spline))
    })
}

# AUC + CI ----------------------------------------------------------------

#' AUC and confidence interval
#'
#' @param x Data frame or regression output
#' @param continuous_var Continuous variable (predictor). Enter as quoted or unquoted variable name. Only required if `x` is a data frame
#' @param outcome_var Binary outcome variable. Enter as quoted or unquoted variable name. Only required if `x` is a data frame
#' @param wilcox If `TRUE` (default), `wilcox.test` is used to calculate confidence interval. If `FALSE`, method created by Hanley and McNeil is used to calculate confidence interval
#' @param ci Confidence interval. Default is `0.95`
#' @param ... Arguments passed to class-specific `auc_ci` function
#' @returns List containing columns for auc, auc_lower, auc_upper. If `x` is a data frame,  "positive" and "negative" outcomes are also included
#' @export
auc_ci <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  UseMethod("auc_ci")
}

#' auc_ci - default method
#'
#' @rdname auc_ci
#' @export
auc_ci.default <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  .warn_input_class("auc_ci", x)
}

#' auc_ci - data frame
#'
#' @rdname auc_ci
#' @export
auc_ci.data.frame <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  continuous_var <- get_input(continuous_var)
  outcome_var <- get_input(outcome_var)
  x <- x[c(continuous_var, outcome_var)]
  x <- x[complete.cases(x), ]
  outcome <- x[[outcome_var]]
  unique_outcomes <- create_levels(outcome)
  n_outcomes <- length(unique_outcomes)
  if (n_outcomes != 2) {
    if (n_outcomes > 2) {
      stop(sprintf("'outcome_var' input to auc_ci() must be a binary variable. Current 'outcome_var' has %s unique groups: %s", n_outcomes, paste0(unique_outcomes, collapse = ", ")), call. = FALSE)
    } else {
      warning(sprintf("'outcome_var' input to auc_ci() has %s unique groups (%s). Unable to compute auc", n_outcomes), call. = FALSE)
      return(NULL)
    }
  }
  y <- x[[continuous_var]]
  cases <-  unique_outcomes[2]
  controls <- unique_outcomes[1]
  cases <- y[outcome == cases]
  controls <- y[outcome == controls]
  out <- list(predictor_var = continuous_var, outcome_var = outcome_var, positive = cases, negative = controls)
  n_cases <- length(cases)
  n_controls <- length(controls)
  if (wilcox) {
    ranks <- rank(c(cases, controls))
    auroc <- (sum(ranks[seq_len(n_cases)]) - n_cases*(n_cases + 1)/2)/(n_cases*n_controls)
    case_ranks <- rep(NA, n_cases)
    control_ranks <- rep(NA, n_controls)
    for (i in seq_len(n_cases)) {
      case_ranks[i] <- mean.default(controls < cases[i] + 0.5*(controls == cases[i]))
    }
    for (j in seq_len(n_controls)) {
      control_ranks[j] <- mean.default(cases > controls[j] + 0.5*(cases == controls[j]))
    }
    auc_se <- sqrt(Var(case_ranks)/n_controls + Var(control_ranks)/n_cases)
  } else {
    outcome_var <- c(rep(1, n_cases), rep(0, n_controls))
    auroc <- auc(.create_roc_data(c(cases, controls), outcome_var), controls ~ cases)
    auc_squared <- auroc*auroc
    auc_se <- sqrt((auroc*(1 - auroc) + (n_cases - 1)*(auroc/(2 - auroc) - auc_squared) + (n_controls - 1)*(2*auc_squared/(1 + auroc) - auc_squared))/(n_cases*n_controls))
  }
  auc_ci <- auroc + qnorm(0.5 + ci/2)*c(-1, 1)*auc_se
  out$auc <- auroc
  out$auc_lower <- auc_ci[1]
  out$auc_upper <- auc_ci[2]
  out
}

#' auc_ci - lm
#'
#' @rdname auc_ci
#' @importFrom survival concordance
#' @export
auc_ci.lm <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  z <- qnorm(0.5 + ci/2)*c(-1, 1)
  auroc <- survival::concordance(x)
  auc_se <- sqrt(auroc$var)
  auc_ci <- auroc$concordance + auc_se*z
  list(auc = auroc$concordance, auc_lower = auc_ci[1], auc_upper = auc_ci[2])
}

#' auc_ci - glm
#'
#' @rdname auc_ci
#' @export
auc_ci.glm <- auc_ci.lm

#' auc_ci - coxph
#'
#' @rdname auc_ci
#' @export
auc_ci.coxph <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  z <- qnorm(0.5 + ci/2)*c(-1, 1)
  auroc <- as.vector(x$concordance[6:7])
  auc_ci <- auroc[1L] + auroc[2L]*z
  list(auc = auroc[1L], auc_lower = auc_ci[1], auc_upper = auc_ci[2])
}
