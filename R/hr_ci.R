#' Hazard ratio, confidence interval, P values
#'
#' @param x Data frame, coxph object, survdiff object, or ggplot object
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param ci Confidence interval. Default is `0.95`
#' @param ... Arguments passed to class-specific `hr_ci` function
#' @returns Data frame containing columns for predictor_var, predictor, hr, hr_lower, hr_upper, p_lrt (if possible), p_wald, n, n_events
#' @export
hr_ci <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95,  ...) UseMethod("hr_ci", x)

#' hr_ci - default method
#'
#' @rdname hr_ci
#' @export
hr_ci.default <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  stop(sprintf("Unsure how to convert an object of class %s to a hazard ratio, CI, and p value", paste0(shQuote(class(x)), collapse = ", ")), call. = FALSE)
}

#' hr_ci - coxph
#'
#' @rdname hr_ci
#' @export
hr_ci.coxph <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  out <- surv_tidy(x, ci = ci)
  coxph_env <- attr(x$terms, ".Environment")
  df <- coxph_env$df
  if (is.null(df)) return(out)
  predictor_var <- coxph_env$predictor_var
  time_var <- coxph_env$time_var
  outcome_var <- coxph_env$outcome_var
  binary_predictors <- vars_binary(df[predictor_var])
  out$p_lrt <- NA_real_
  if (length(binary_predictors) > 0L && any(names(out) == "predictor_var")) {
    out$p_lrt[match(binary_predictors, out$predictor_var, nomatch = 0L)] <- vapply(binary_predictors, function(y) {
      p_log_rank(df = df, predictor_var = y, time_var = time_var, outcome_var = outcome_var)
    }, numeric(1), USE.NAMES = FALSE)
  }
  move_cols(out, "p_lrt", before = "p_wald")
}

#' hr_ci - data frame
#'
#' @rdname hr_ci
#' @export
hr_ci.data.frame <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  hr_ci.coxph(Coxph(x, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var))
}

#' hr_ci - grouped_dr
#'
#' @rdname hr_ci
#' @export
hr_ci.grouped_df <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  survival_summary <- dplyr::mutate(tidyr::nest(x), coxph_model = purrr::map(data, function(.x) surv_tidy(Coxph(df = .x, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var))), p_lrt = purrr::map_dbl(data, function(.x) p_log_rank(df = .x, outcome_var = outcome_var, time_var = time_var, predictor_var = predictor_var)))
  survival_summary <- tidyr::unnest(survival_summary, coxph_model)
  survival_summary <- dplyr::ungroup(survival_summary)
  dplyr::select(survival_summary, -data)
}
