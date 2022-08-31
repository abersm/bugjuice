# Data --------------------------------------------------------------------

#' Calculate follow up time for survival data using dates
#'
#' @param df Data frames
#' @param date_t0 Column in `df` containing start date. Enter as quoted variable name
#' @param date_censor Column in `df` containing date subject last known to be well (without outcome). Enter as quoted variable name
#' @param date_outcome Column in `df` containing date of outcome. Enter as quoted variable name
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted variable name. Default is `"death"`
#' @param unit Unit of time. Options: `"weeks"` (default), `"days"`, `"hours"`, `"mins"`, `"secs"`
#' @param colname_time Name for calculated time variable. Default is `"time"`
#' @returns Data frame with new column for follow up time
#' @export
calc_surv_time <- function(df, date_t0, date_censor, date_outcome, outcome_var = "death", unit = "weeks", colname_time = "time") {
  if (!is_binary_01(df[[outcome_var]])) {
    stop("'outcome_var' input to calc_surv_time() must be an integer binary variable with 1 for event and 0 for no event", call. = FALSE)
  }
  df <- df[complete.cases(df[, c(date_t0, outcome_var), drop = FALSE]), ]
  df <- dplyr::mutate(df,
                      {{ colname_time }} := dplyr::case_when(
                        .data[[outcome_var]] == 1 & !is.na(.data[[date_outcome]]) ~ time_interval(t0 = .data[[date_t0]], t1 = .data[[date_outcome]], units = unit),
                        .data[[outcome_var]] == 0 & !is.na(.data[[date_censor]]) ~ time_interval(t0 = .data[[date_t0]], t1 = .data[[date_censor]], units = unit),
                        TRUE ~ NA_real_))
  if (min(df[[colname_time]]) < 0) {
    stop("Negative follow up times were calculated", call. = FALSE)
  }
  df[complete.cases(df[colname_time]), ]
}

#' Set maximum duration of follow up time
#'
#' @param df Data frames
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `"time"`
#' @param max_time Maximum duration of follow up. Enter as numeric
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted variable name. Default is `"death"`
#' @export
set_max_time_follow_up <- function(df, time_var = "time", max_time, outcome_var = "death") {
  if (!is_binary_01(df[[outcome_var]])) {
    stop("'outcome_var' input to set_max_time_follow_up() must be an integer binary variable with 1 for event and 0 for no event", call. = FALSE)
  }
  df <- df[complete.cases(df[, c(time_var, outcome_var), drop = FALSE]), ]
  df[[outcome_var]] <- ifelse(df[[outcome_var]] == 1 & df[[time_var]] > max_time, 0, df[[outcome_var]])
  df[[time_var]] <- ifelse(df[[time_var]] > max_time, max_time, df[[time_var]])
  df
}

#' Alias for set_max_time_follow_up
#'
#' @rdname set_max_time_follow_up
#' @export
set_max_follow_up_time <- set_max_time_follow_up

# Survival functions ------------------------------------------------------

#' coxph wrapper function
#'
#' @param df Data frame
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is "time"
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param predictor_var Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in `c()`
#' @param ... Arguments passed to `survival::coxph`
#' @returns Output from `survival::coxph`
#' @export
Coxph <- function(df, predictor_var, time_var = "time", outcome_var = "death", ...) {
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::coxph, ...)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  out$univariate <- length(predictor_var) < 2
  out
}

#' survfit wrapper function
#'
#' @rdname Coxph
#' @export
Survfit <- function(df, predictor_var, time_var = "time", outcome_var = "death", ...) {
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::survfit, ...)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  out$univariate <- length(predictor_var) < 2
  out
}

#' survdiff wrapper function
#'
#' @rdname Coxph
#' @param method Method for determining P value. Options: `MH` (default, Mantel-Haenszel test), `PP` (Peto and Peto modification of Gehan-Wilcoxon test)
#' @export
Survdiff <- function(df, predictor_var, time_var = "time", outcome_var = "death", method = "MH", ...) {
  rho <- if (method == "MH") 0 else 1
  numeric_predictors <- vars_which(df[predictor_var], function(x) is.numeric(x) && n_unique(x) > 10)
  if (length(numeric_predictors) > 0) {
    message("Only categorical variables should be entered into 'predictor_var' argument of Survdiff()\nThe following variables are likely continuous: ", paste0(numeric_predictors, collapse = ", "))
  }
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::survdiff, rho = rho)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  out$univariate <- length(predictor_var) < 2
  out
}

# P values ----------------------------------------------------------------

#' Log-rank p value
#'
#' @param df Data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in `c()`
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param method Method for determining P value. Options: `MH` (default, Mantel-Haenszel test), `PP` (Peto and Peto modification of Gehan-Wilcoxon test)
#' @param otherwise Value to return if unable to perform log-rank test
#' @returns P value for log-rank test as length 1 numeric vector
#' @export
p_log_rank <- function(df, predictor_var, time_var = "time", outcome_var = "death", method = "MH", otherwise = NA_real_) {
  tryCatch({
    z <- Survdiff(df = df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, method = method)
    pchisq(z$chisq, length(z$n) - 1, lower.tail = FALSE)
    }, error = function(e) otherwise)
}

#' P value for log rank test
#'
#' @param x coph or survdiff object
#' @returns P value for log-rank test as length 1 numeric vector
#' @export
p_surv <- function(x) {
  if (inherits(x, "coxph")) {
    pchisq(x$score, sum(!is.na(x$coefficients)), lower.tail = FALSE)
  } else {
    pchisq(x$chisq, length(x$n) - 1, lower.tail = FALSE)
  }
}

#' Test Cox model for proportional hazards
#'
#' @param df Data frame or coxph object
#' @inheritParams Coxph
#' @returns Data frame containing columns for predictor_var (1 row for global model) and another for p. If p < 0.05, proportional hazards assumption is violated
#' @importFrom survival cox.zph
#' @export
p_prop_hazards <- function(df, predictor_var, time_var = "time", outcome_var = "death") {
  if (is.data.frame(df)) {
    df <- Coxph(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
  }
  df <- survival::cox.zph(df)
  df <- matrix_to_df(df$table, rownames_to_col = TRUE, colname_rownames = "predictor_var")
  df[c("predictor_var", "p")]
}

# Broom tidiers -----------------------------------------------------------

#' Summary of coxph object (similar to broom::tidy output)
#'
#' @param x coxph object
#' @param ci Confidence interval. Default is `0.95`
#' @param clean_predictor_names If `TRUE` (default), predictor column of output is cleaned and output contains separate columns for predictor_var ("variable" in output) and level of predictor variable ("predictor" in output)
#' @returns Data frame with columns outcome_var, time_var, predictor_var, predictor, hr, hr_lower, hr_upper, p_wald, n, n_events, univariate, covariates
#' @export
surv_tidy <- function(x, ci = 0.95, clean_predictor_names = TRUE) {
  beta <- x$coefficients
  label <- names(beta)
  beta <- as.vector(beta)
  se <- sqrt(diag(x$var))
  X <- beta/se
  z <- qnorm(0.5 + ci/2, 0, 1)*se
  out <- vec_to_df(
    outcome_var = x$outcome_var,
    label = gsub("TRUE|FALSE", "", label),
    time_var = x$time_var,
    hr = exp(beta), hr_lower = exp(beta - z), hr_upper = exp(beta + z),
    p_wald = pchisq(X*X, 1, lower.tail = FALSE),
    n = x$n, n_events = x$nevent,
    univariate = x$univariate)
  out$covariates <- list(x$predictor_var)
  out <- dplyr::left_join(out, .regression_var_lookup(x), by = "label")
  out[c("outcome_var", "time_var", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_wald", "n", "n_events", "univariate", "covariates")]
}

#' Summarize coxph model (similar to broom::glance output)
#'
#' @param x coxph object
#' @returns Data frame with 1 row and columns for outcome_var, time_var, covariates, n, n_events, p_wald_model (P value for Wald test for global model), p_log_rank_coxph_model (P value for log-rank test for global model), p_log_likelihood_model (P value for log likelihood ratio test for global model), concordance, concordance_se, r_sq, r_sq_max, aic, bic, univariate
#' @export
surv_glance <- function(x) {
  beta <- as.vector(x$coefficients)
  deg_free <- length(beta[!is.na(beta)])
  loglik <- x$loglik
  loglik_1 <- loglik[1L]
  loglik_2 <- loglik[2L]
  logtest <- 2*(loglik_2 - loglik_1)
  n <- x$n
  concordance <- as.vector(x$concordance[6:7])
  vars <- .regression_vars(x)
  out <- vec_to_df(
    outcome_var = vars$outcome_var,
    time_var = vars$time_var,
    n = n,
    n_events = x$nevent,
    p_log_rank_coxph_model = pchisq(x$score, deg_free, lower.tail = FALSE),
    p_wald_model = pchisq(as.vector(x$wald.test), deg_free, lower.tail = FALSE),
    p_log_likelihood_model = pchisq(logtest, deg_free, lower.tail = FALSE),
    concordance = concordance[1L],
    concordance_se = concordance[2L],
    r_sq = 1 - exp(-logtest/n),
    r_sq_max = 1 - exp(2*loglik_1/n),
    aic = AIC(x),
    bic = BIC(x),
    univariate = vars$univariate)
  out$covariates <- list(vars$predictor_var)
  move_cols(out, "outcome_var", "time_var", "covariates")
}

# Summary functions -------------------------------------------------------

#' Pairwise HR, CI, P values
#'
#' @param df Data frame
#' @param predictor_var Variable to be compared pairwise. Must be categorical with > 1 level. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param adjust_method Method of adjustment for multiple comparisons
#' @returns Data frame containing columns for Group1, Group2, hr, hr_lower, hr_upper, p_lrt, p_lrt_adj, label, p_wald, p_wald_adj, n, n_events
#' @export
surv_pairwise <- function(df, predictor_var, time_var = "time", outcome_var = "death", adjust_method = "holm") {
  df <- df[c(time_var, predictor_var, outcome_var)]
  df <- df[complete.cases(df), ]
  predictor <- df[[predictor_var]]
  group_pairs <- pairs_list(unique.default(predictor))
  out <- map_dfr(group_pairs, function(x) {
    df1 <- df[predictor %in% x, ]
    coxph_model <- Coxph(df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    out <- surv_tidy(coxph_model, clean_predictor_names = FALSE)
    out$p_lrt <- p_log_rank(df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    out$n <- coxph_model$n
    out$n_events <- coxph_model$nevent
    out$Group1 <- x[1L]
    out$Group2 <- x[2L]
    out
  })
  out$p_lrt_adj <- p_adjust(out$p_lrt, method = adjust_method)
  out$p_wald_adj <- p_adjust(out$p_wald, method = adjust_method)
  out$label <- sig_stars(out$p_lrt_adj)
  out$adjust_method <- adjust_method
  out[c("outcome_var", "time_var", "predictor_var", "predictor", "Group1", "Group2", "hr", "hr_lower", "hr_upper", "p_lrt", "p_lrt_adj", "label", "p_wald", "p_wald_adj", "n", "n_events", "univariate", "adjust_method")]
}

#' Extract sample sizes/number of events and p value from survdiff object
#'
#' @param x survdiff object
#' @returns List containing p_lrt and a data frame with information about grouping variables, sample size (n), number of events (n_events)
#' @export
survdiff_summary <- function(x) {
  group_sizes <- x$n
  df_groups <- strsplit(names(group_sizes), ", ")
  df_groups <- Reduce(rbind, df_groups)
  df_names <- gsub("=.*", "", df_groups)[1, ]
  df_groups <- matrix_to_df(gsub(".*=", "", df_groups))
  names(df_groups) <- df_names
  df_groups$n <- as.vector(group_sizes)
  df_groups$n_events <- x$obs
  list(p_lrt = pchisq(x$chisq, length(x$n) - 1, lower.tail = FALSE), groups = df_groups)
}

# Cutpoints ---------------------------------------------------------------

#' Cutpoint for survival outcome
#'
#' @param df Data frame
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param predictor_var Continuous predictor variable(s). Enter as comma separated list of quoted variable names wrapped in `c()`
#' @param min_prop Minimum proportion of all subjects in smaller of 2 groups when stratifying by cutpoint. Enter as numeric proportion. Default is `0.1`
#' @returns Data frame containing columns for outcome_var, time_var, predictor_var, cutpoint
#' @export
cutpoint_survival <- function(df, predictor_var, time_var = "time", outcome_var = "death", min_prop = 0.1) {
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  df <- df[complete.cases(df[, c(outcome_var, time_var, predictor_var), drop = FALSE]), ]
  if (nrow(df) == 0L) return(NULL)
  time <- .subset2(df, time_var)
  outcome <- .subset2(df, outcome_var)
  predictor <- .subset2(df, predictor_var)
  time_ordered <- order(time)
  time_rank <- as.integer(rank(time, time_ordered))
  z <- outcome/(length(outcome) - time_rank + 1)
  z <- outcome - cumsum(z[time_ordered])[time_rank]
  z_length <- length(z)
  y <- z[order(predictor)]
  x <- sort.int(predictor, method = "quick")
  z <- which(!duplicated(x)) - 1
  if (all(z < floor(z_length*min_prop))) {
    stop("In cutpoint_survival(), 'min_prop' is too large", call. = FALSE)
  }
  z <- z[z >= max(1, floor(z_length*min_prop))]
  z <- z[z <= floor(z_length*(1 - min_prop))]
  v <- sum(y)
  a <- z/z_length*v
  b <- z*(z_length - z)/(z_length*z_length*(z_length - 1))*(z_length*sum(y*y) - v*v)
  a <- abs((cumsum(y)[z] - a)/sqrt(b))
  vec_to_df(outcome_var = outcome_var, time_var = time_var, predictor_var = predictor_var, cutpoint = x[z[min(which(a == max(a)))]])
}

#' Evaluate specific cutpoint
#'
#' @param df Data frame
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @param cp Cutpoints to be evaluated. Enter as numeric vector
#' @param cutpoint Alias for `cp`
#' @param time_var Variable containing follow up time. Enter as quoted or unquoted variable name. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted or unquoted variable name. Default is `"death"`
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @returns Data frame with 1 row for each cutpoint and columns for outcome_var, time_var, predictor_var, type, cutpoint, hr, hr_lower, hr_upper, p_lrt, label, p_wald, n, pos, neg, perc_pos, perc_neg, n_events
#' @export
eval_cutpoint_survival <- function(df, predictor_var, cp, time_var = "time", outcome_var = "death", type = ">=", cutpoint = cp) {
  if (length(cutpoint) > 1) {
    return(map_dfr(cutpoint, function(x) tryCatch(eval_cutpoint_survival(df = df, predictor_var = predictor_var, outcome_var = outcome_var, cutpoint = x, type = type), error = function(e) NULL)))
  }
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  predictor <- df[[predictor_var]]
  # df$pos <- as.integer(eval(str2lang(paste("predictor", type, cutpoint))))
  df$pos <- .cont_to_binary_01(predictor, cutpoint = cutpoint, type = type)
  out <- surv_tidy(Coxph(df = df, predictor_var = "pos", time_var = time_var, outcome_var = outcome_var))
  out$predictor_var <- predictor_var
  out$type <- type
  out$cutpoint <- cutpoint
  out$pos <- sum(df$pos, na.rm = TRUE)
  out$perc_pos <- out$pos/out$n
  out$neg <- out$n - out$pos
  out$perc_neg <- out$neg/out$n
  out$p_lrt <- p_log_rank(df = df, predictor_var = "pos", time_var = time_var, outcome_var = outcome_var)
  out$label <- sig_stars(out$p_lrt)
  out[c("outcome_var", "time_var", "predictor_var", "type", "cutpoint", "hr", "hr_lower", "hr_upper", "p_lrt", "label", "p_wald", "n", "pos", "perc_pos", "neg", "perc_neg", "n_events")]
}

#' Evaluate all possible cutpoints
#'
#' Positive refers to patients with predictor_var > cp (not >= cp)
#' @param df Data frame
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted variable name. Default is `death`
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @param min_prop Minimum proportion of all subjects in smaller of 2 groups created when stratifying by cutpoint to positive and negative groups. Enter as numeric proportion. Default is `0.1`
#' @returns Data frame with 1 row for each unique value of predictor variable and columns for outcome_var, time_var, predictor_var, type, cutpoint, hr, hr_lower, hr_upper, p_lrt, label, p_wald, n, n_events, pos, perc_pos, neg, perc_neg
#' @export
eval_all_cutpoints_survival <- function(df, predictor_var, time_var = "time", outcome_var = "death", type = ">=", min_prop = 0.1) {
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  unique_cps <- unique.default(df[[predictor_var]])
  min_max <- Quantile(df[[predictor_var]], probs = c(min_prop, 1 - min_prop))
  unique_cps <- unique_cps[unique_cps >= min_max[1] & unique_cps <= min_max[2]]
  out <- eval_cutpoint_survival(df = df, cutpoint = unique_cps, predictor_var = predictor_var, outcome_var = outcome_var, time_var = time_var, type = type)
  out[order(out$p_lrt), ]
}

# Earliest significant time point -----------------------------------------

#' Evaluate all time points to identify time point when curves first separate
#'
#' @param df Data frame (can be grouped)
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @param predictor_var Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in `c()`
#' @param time_by Interval between time points. Default is `1`
#' @returns Data frame containing columns for time, variable, predictor, hr, hr_lower, hr_upper, p_lrt, p_wald, n_events, n, n_events_ever, cfd_events (proportion of events that occurred by a given time as as a proportion of total events that ever occurred)
#' @export
earliest_sig_surv <- function(df, predictor_var, time_var = "time", outcome_var = "death", time_by = 1L) {
  time_range <- ceiling(range(df[[time_var]]))
  time_eval <- seq.int(from = time_range[1], to = time_range[2], by = time_by)
  out <- purrr::map_dfr(time_eval, function(x) {
    df1 <- set_max_time_follow_up(df = df, outcome_var = outcome_var, time_var = time_var, max_time = x)
    z <- surv_tidy(Coxph(df = df1, outcome_var = outcome_var, time_var = time_var, predictor_var = predictor_var))
    z$p_lrt <- p_log_rank(df = df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    z$time <- x
    idx <- Intersect(c("time_var", "time", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_lrt", "p_wald", "n", "n_events"), names(z))
    z[idx]
  })
  out$n_events_ever <- sum(df[[outcome_var]], na.rm = TRUE)
  out$cfd_events <- out$n_events/out$n_events_ever
  out
}


# Helpers -----------------------------------------------------------------

#' Create survival formula
#'
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector. Default includes all variables
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `time`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `death`
#' @returns Survival formula
#' @export
create_surv_formula <- function(predictor_var = ".", time_var = "time", outcome_var = "death") {
  as_formula(paste0(paste(sprintf("Surv(time = %s, event = %s)", time_var, outcome_var), paste(predictor_var, collapse = " + "), sep = " ~ ")), env = parent.frame())
}

#' Survival function capable of taking quoted variable names
#'
#' @param df Data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as character vector
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as character vector
#' @param survival_fn Function from survival package. Options: coxph (default), survdiff, survfit
#' @param otherwise Output if unable to run survival model. Default is NULL
#' @param ... Arguments passed to survival_fn
#' @importFrom survival Surv coxph survdiff survfit cox.zph
#' @noRd
.survival_fn <- function(df, predictor_var, time_var = "time", outcome_var = "death", survival_fn = survival::coxph, otherwise = NULL, ...) {
  df <- df[c(time_var, outcome_var, predictor_var)]
  df <- df[complete.cases(df), ]
  vars_recode <- Setdiff(vars_which(df, function(x) n_unique(x, na.rm = TRUE) <= 2L && is.numeric(x)), outcome_var)
  if (length(vars_recode) > 0L) {
    df[vars_recode] <- lapply(df[vars_recode], function(x) factor(x, levels = create_levels(x)))
  }
  formula <- create_surv_formula(predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
  tryCatch(suppressWarnings(eval(bquote(survival_fn(formula = .(formula), data = df, ...)))), error = function(e) otherwise)
}
