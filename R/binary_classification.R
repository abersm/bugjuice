#' Generate data required for 2 x 2 table
#'
#' @param df Data frame
#' @param outcome_var Outcome variable. Binary variable with the lower value representing controls and the higher value cases (i.e. outcome occurred). Enter as quoted variable name
#' @param predictor_var Predictor variable. Either a binary variable with the lower value representing a negative result and the higher value a positive result or a continuous variable with `cutpoint` and type input specifying a positive result. Enter as quoted variable name
#' @param cutpoint Cutpoint that defines a positive or negative result for `predictor_var`
#' @param type If `">="` (default), positivity is determined by values greater than or equal to `cutpoint.`  If `">"`, positivity is determined by values of `predictor_var` greater than `cutpoint`
#' @returns List containing the following counts: total (non-missing), cases, controls, positive, negative, tp, fn, fp, tn
#' @export
classify_binary <- function(df, outcome_var, predictor_var, cutpoint = NULL, type = ">=") {
  df <- df[complete.cases(df[, c(outcome_var, predictor_var)]), ]
  .summary_counts_2_by_2(.subset2(df, outcome_var), .subset2(df, predictor_var), cutpoint = cutpoint, type = type)
}

# Sensitivity/specificity -------------------------------------------------

#' Determine sensitivity and specificity at a given cutpoint
#'
#' @param x Several options for input including: Option 1: An integer vector containing counts entered in the following order: test-/control, test-/case, test+/control, test+/case (tn, fn, fp, tp). Option 2: A matrix with columns for test (- in col 1, + in col 2) and rows for truth (- in row 1, + in row 2). tn in row 1/col 1, tp in row 2/col 2. fn in row 2/col 1, fp in row 1, col 2. Option 3: A table with structure identical to Option 2. Option 4: A data frame with columns for `outcome_var` (column name for binary variable) and `predictor_var` (column name for continuous variable)
#' @param outcome_var Binary outcome variable coded as 0 if outcome did not occur and 1 if outcome occurred. Enter as quoted or unquoted variable name. Only relevant when `x` is a data frame
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name. Only relevant when `x` is a data frame
#' @param cp,cutpoint Cutpoint for binary classification. Will be ignored if entered but `predictor_var` is categorical. Only relevant when `x` is a data frame
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of `predictor_var` greater than cutpoint. Only relevant when `x` is a data frame
#' @param ci Confidence interval. Default is `0.95`
#' @param yates If `TRUE`, Yates' correction is applied to estimation of chi-squared statistic
#' @param n_min_chisq Minimum group size that will be allowed for chi-squared test. Default is `5`
#' @param ... Arguments passed to class-specific `sens_spec` function
#' @returns Data frame with columns for sens, sens_lower, sens_upper, spec, spec_lower, spec_upper, p, label, method, p_fisher, p_chi, youden, cohen_kappa, cohen_kappa_lower, cohen_kappa_upper, tpr, fpr, tnr, fnr, fdr, ppv, npv, lr_pos, lr_neg, dor, dor_lower, dor_upper, accuracy, auc, gini, jaccard_index, n, n_outcome, n_no_outcome, pos, neg, tp, fn, fp, tn, perc_outcome, perc_no_outcome, perc_pos, perc_neg
#' @export
sens_spec <- function(x, outcome_var = NULL, predictor_var = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5, ...) {
  UseMethod("sens_spec", x)
}

#' sens_spec - default method
#'
#' @rdname sens_spec
#' @export
sens_spec.default <- function(x, outcome_var = NULL, predictor_var = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5, ...) {
  sens_spec.matrix(matrix(x, nrow = 2), ci = ci, yates = yates, n_min_chisq = n_min_chisq)
}

#' sens_spec - matrix
#'
#' @rdname sens_spec
#' @export
sens_spec.matrix <- function(x, outcome_var = NULL, predictor_var = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5, ...) {
  # Counts
  tn <- x[1, 1]
  tp <- x[2, 2]
  fn <- x[2, 1]
  fp <- x[1, 2]
  n <- sum(x)
  outcome <- tp + fn
  no_outcome <- tn + fp
  pos <- tp + fp
  neg <- tn + fn
  correct <- tp + tn
  incorrect <- fp + fn

  # Calculations
  sens <- tp/outcome
  spec <- tn/no_outcome
  fnr <- 1 - sens
  fpr <- 1 - spec
  ppv <- tp/pos
  perc_outcome <- outcome/n
  perc_no_outcome <- no_outcome/n
  perc_pos <- pos/n
  perc_neg <- neg/n
  accuracy <- correct/n
  missclass_rate <- incorrect/n
  y <- sens + spec
  auroc <- y/2
  dor <- tp*tn/(fp*fn)
  perc_expected <- perc_neg*perc_no_outcome + perc_pos*perc_outcome
  kappa <- (accuracy - perc_expected)/(1 - perc_expected)
  z <- qnorm(0.5 + ci/2)
  sens_ci <- sens + z*sqrt(sens*fnr/outcome)*c(-1, 1)
  spec_ci <- spec + z*sqrt(spec*fpr/no_outcome)*c(-1, 1)
  dor_ci <- dor*exp(z*sqrt(sum(1/x))*c(-1, 1))
  kappa_ci <- kappa + z*sqrt(accuracy*missclass_rate/(n*(1 - perc_expected)^2))*c(-1, 1)

  # P values
  p_fet <- tryCatch(suppressWarnings(fisher.test(x)$p.value), error = function(e) NA_real_)
  E <- tcrossprod(c(no_outcome, outcome), c(neg, pos))/n
  o_e <- abs(x - E)
  yates <- if (yates) min(0.5, o_e) else 0
  X2 <- sum((o_e - yates)^2/E)
  p_chisq <- tryCatch(pchisq(X2, 1, lower.tail = FALSE), error = function(e) NA_real_)
  if (min(x, na.rm = TRUE) < n_min_chisq) {
    p <- p_fet
    method <- "Fisher's exact test"
  } else {
    p <- p_chisq
    method <- "Chi-squared test"
  }
  vec_to_df(
    sens = sens, sens_lower = sens_ci[1], sens_upper = sens_ci[2],
    spec = spec, spec_lower = spec_ci[1], spec_upper = spec_ci[2],
    p = p, label = sig_stars(p), method = method, p_fisher = p_fet, p_chi = p_chisq,
    tpr = sens, fpr = fpr, tnr = spec, fnr = fnr, fdr = 1 - ppv, ppv = ppv, npv = tn/neg,
    youden = y - 1, auc = auroc, gini = 2*auroc - 1, jaccard_index = tp/(outcome + fp),
    dor = dor, dor_lower = dor_ci[1], dor_upper = dor_ci[2],
    lr_pos = sens/fpr, lr_neg = fnr/spec,
    cohen_kappa = kappa, cohen_kappa_lower = kappa_ci[1], cohen_kappa_upper = kappa_ci[2],
    n = n, outcome = outcome, perc_outcome = perc_outcome, no_outcome = no_outcome, perc_no_outcome = perc_no_outcome,
    pos = pos, perc_pos = perc_pos, neg = neg, perc_neg = perc_neg,
    correct = correct, accuracy = accuracy, incorrect = incorrect, missclass_rate = missclass_rate,
    tp = tp, fn = fn, fp = fp, tn = tn)
}

#' sens_spec - table
#'
#' @rdname sens_spec
#' @export
sens_spec.table <- function(x, outcome_var = NULL, predictor_var = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5, ...) {
  out <- sens_spec.matrix(x, ci = ci, yates = yates, n_min_chisq = n_min_chisq)
  z <- names(out)
  dim_names <- dimnames(x)
  outcomes <- dim_names[[1]] %||% c("row1", "row2")
  predictors <- dim_names[[2]] %||% c("col1", "col2")
  var_names <- names(dim_names) %||% c("outcome", "predictor")
  out$outcome_var <- var_names[1]
  out$predictor_var <- var_names[2]
  out$def_predictor_pos <- predictors[2]
  out$def_predictor_neg <- predictors[1]
  out$def_outcome <- outcomes[2]
  out$def_no_outcome <- outcomes[1]
  out[c("predictor_var", "outcome_var", z, "def_outcome", "def_no_outcome", "def_predictor_pos", "def_predictor_neg")]
}

#' sens_spec - data frame
#'
#' @rdname sens_spec
#' @returns Data frame containing the following columns: outcome, predictor, type, cutpoint, sens, sens_lower, sens_upper, spec, spec_lower, spec_upper, p, label, method, p_fisher, p_chi, youden, cohen_kappa, tpr, fpr, tnr, fnr, ppv, npv, fdr, lr_pos, lr_neg, dor, dor_lower, dor_upper, accuracy, auc, gini, jaccard_index, n, outcome, no_outcome, pos, neg, tp, fn, fp, tn, perc_outcome, perc_no_outcome, perc_pos, perc_neg. Confidence intervals for sensitivity and specificity are approximate binomial
#' @export
sens_spec.data.frame <- function(x, outcome_var = NULL, predictor_var = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5, ...) {
  outcome_var <- get_input(outcome_var)
  predictor_var <- get_input(predictor_var)
  if (!is.null(cutpoint) && (n_unique(z <- .subset2(x, predictor_var)) == 2L || !is.numeric(z))) {
    message(sprintf("In sens_spec(), cutpoint = %s, but x[[predictor_var]] is categorical. Will ignore 'cutpoint' argument"))
    cutpoint <- NULL
  }
  out <- classify_binary(x, outcome_var = outcome_var, predictor_var = predictor_var, cutpoint = cutpoint, type = type)
  if (is.null(cutpoint)) {
    cutpoint <- NA_real_
    type <- NA_character_
  }
  out <- sens_spec.matrix(matrix(c(out$tn, out$fn, out$fp, out$tp), nrow = 2), ci = ci, yates = yates, n_min_chisq = n_min_chisq)
  z <- names(out)
  out$outcome_var <- outcome_var
  out$predictor_var <- predictor_var
  out$cutpoint <- cutpoint
  out$type <- type
  out[c("outcome_var", "predictor_var", "type", "cutpoint", z)]
}

# Odds ratio --------------------------------------------------------------

#' Calculate OR, 95% CI, and P value
#'
#' @param x matrix, table, integer vector of counts (only used for 2 x 2 matrix. Must enter in following order: tn, fn, fp, tp), data frame (with predictor/outcome variables specified individually or in a formula), glm or lm object. If x is a vector of counts, enter as test-/control, test-/case, test+/control, test+/case. Also predictor-/outcome-, predictor-/outcome+, predictor+/outcome-, predictor+/outcome+. Entered into matrix by column. For matrix input, columns = predictor/test result/risk factor/case vs. control (- or control in col 1, + or case in col 2), rows = control truth/outcome (outcome- in row 1, outcome+ in row 2)
#' @param ... Arguments passed to class-specific `odds_ratio` function
#' @param outcome_var Outcome variable. Enter as quoted or unquoted variable name
#' @param predictor_var Predictor variable. Enter as quoted or unquoted variable name
#' @param formula Formula as y ~ x
#' @param cp,cutpoint Cutpoint. Will be ignored if entered but `predictor_var` is categorical
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of `predictor_var` greater than cutpoint
#' @param n_min_chisq Minimum group size that will be allowed for chi-squared test. Default is `5`
#' @param ci Confidence interval. Default is `0.95`
#' @param yates If `TRUE`, Yates' correction is applied to estimation of chi-squared statistic
#' @returns Data frame with columns for or, or_lower, or_upper, p, label, method, p_fisher, p_chi
#' @export
odds_ratio <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  UseMethod("odds_ratio", x)
}

#' odds_ratio - default method
#'
#' @rdname odds_ratio
#' @export
odds_ratio.default <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  odds_ratio.matrix(matrix(c(x, ...), nrow = 2), ci = ci, yates = yates, n_min_chisq = n_min_chisq)
}

#' odds_ratio - matrix
#'
#' @rdname odds_ratio
#' @export
odds_ratio.matrix <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  # Counts
  tn <- x[1, 1]
  tp <- x[2, 2]
  fn <- x[2, 1]
  fp <- x[1, 2]
  n <- sum(x)
  outcome <- tp + fn
  no_outcome <- tn + fp
  pos <- tp + fp
  neg <- tn + fn

  # OR, CI
  or <- tn*tp/(fp*fn)
  or_ci <- or*exp(qnorm(0.5 + ci/2)*sqrt(sum(1/x))*c(-1, 1))

  # P values
  p_fet <- p_fisher(x)
  p_chisq <- p_chi(x)
  if (min(x, na.rm = TRUE) < 5) {
    p <- p_fet
    method <- "Fisher's exact test"
  } else {
    p <- p_chisq
    method <- "Chi-squared test"
  }

  # Output
  vec_to_df(
    or = or[1], or_lower = or_ci[1], or_upper = or_ci[2],
    p = p, method = method, label = sig_stars(p), p_fisher = p_fet, p_chi = p_chisq,
    n = n, pos = pos, perc_pos = pos/n, neg = neg, perc_neg = neg/n,
    outcome = outcome/n, perc_outcome = outcome/n, no_outcome = no_outcome, perc_no_outcome = no_outcome/n,
    tn = tn, fn = fn, fp = fp, tp = tp)
}

#' odds_ratio - table
#'
#' @rdname odds_ratio
#' @export
odds_ratio.table <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  out <- odds_ratio.matrix(x, ci = ci, yates = yates, n_min_chisq = n_min_chisq)
  z <- names(out)
  dim_names <- dimnames(x)
  outcomes <- dim_names[[1]] %||% c("row1", "row2")
  predictors <- dim_names[[2]] %||% c("col1", "col2")
  var_names <- names(dim_names) %||% c("outcome", "predictor")
  out$outcome_var <- var_names[1]
  out$predictor_var <- var_names[2]
  out$def_predictor_pos <- predictors[2]
  out$def_predictor_neg <- predictors[1]
  out$def_outcome <- outcomes[2]
  out$def_no_outcome <- outcomes[1]
  out[c("predictor_var", "outcome_var", z, "def_outcome", "def_no_outcome", "def_predictor_pos", "def_predictor_neg")]
}

#' odds_ratio - data frame
#'
#' @rdname odds_ratio
#' @returns Data frame containing the following columns: outcome_var, predictor_var, type, cutpoint, or, or_lower, or_upper, p, label, method, p_fisher, p_chi, n, outcome, perc_outcome, no_outcome, perc_no_outcome, pos, perc_pos, neg, perc_neg, tn, fn, fp, tp, def_outcome, def_no_outcome, def_predictor_pos, def_predictor_neg
#' @export
odds_ratio.data.frame <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  if (is.null(formula)) {
    outcome_var <- get_input(outcome_var)
    predictor_var <- get_input(predictor_var)
  } else {
    outcome_var <- all.vars(formula)
    predictor_var <- outcome_var[2]
    predictor_var <- outcome_var[1]
  }
  predictors <- unique.default(.subset2(x, predictor_var))
  predictors <- predictors[!is.na(predictors)]
  if (!is.null(cutpoint) && (length(predictors) == 2L || !is.numeric(predictors))) {
    message(sprintf("In odds_ratio(), cutpoint = %s, but predictor variable is categorical. Will ignore 'cutpoint' argument"))
    cutpoint <- NULL
  }
  out <- classify_binary(x, outcome_var = outcome_var, predictor_var = predictor_var, cutpoint = cutpoint, type = type)
  if (is.null(cutpoint)) {
    cutpoint <- NA_real_
    type <- NA_character_
    predictors <- as.character(sort.int(predictors))
  } else {
    z <- switch(type, ">" = "<=", ">=" = "<", "<" = ">=", "<=" = ">")
    predictors <- paste(predictor_var, c(z, type))
  }
  outcomes <- unique.default(.subset2(x, outcome_var))
  outcomes <- as.character(sort.int(outcomes[!is.na(outcomes)]))
  out <- odds_ratio.matrix(matrix(c(out$tn, out$fn, out$fp, out$tp), nrow = 2), ci = ci, yates = yates, n_min_chisq = n_min_chisq)
  z <- names(out)
  out$outcome_var <- outcome_var
  out$predictor_var <- predictor_var
  out$cutpoint <- cutpoint
  out$type <- type
  out$def_outcome <- outcomes[2]
  out$def_no_outcome <- outcomes[1]
  out$def_predictor_pos <- predictors[2]
  out$def_predictor_neg <- predictors[1]
  out[c("outcome_var", "predictor_var", "type", "cutpoint", "or", "or_lower", "or_upper", "p", "label", "method", "p_fisher", "p_chi", "n", "outcome", "perc_outcome", "no_outcome", "perc_no_outcome", "pos", "perc_pos", "neg", "perc_neg", "tn", "fn", "fp", "tp", "def_outcome", "def_no_outcome", "def_predictor_pos", "def_predictor_neg")]
}

#' odds_ratio - lm
#'
#' @rdname odds_ratio
#' @export
odds_ratio.lm <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  lm_tidy(x, ci = ci)
}

#' odds_ratio - glm
#'
#' @rdname odds_ratio
#' @export
odds_ratio.glm <- function(x, ..., outcome_var = NULL, predictor_var = NULL, formula = NULL, cp = NULL, type = ">=", ci = 0.95, yates = FALSE, cutpoint = cp, n_min_chisq = 5) {
  glm_tidy(x, ci = ci)
}

# Relative risk -----------------------------------------------------------

#' Calculate RR, 95% CI, and P value
#'
#' @param ... Enter raw counts with or without c(), 2 x 2 matrix or table. If x is a vector of counts, enter as exposure-/outcome-, exposure-/outcome+, exposure+/outcome-, exposure+/outcome+ (control/outcome-, control/outcome+, case/outcome-, case/outcome+). Entered into matrix by column. For matrix input, columns = predictor/exposure/risk factor/case vs. control (- or control in col 1, + or case in col 2), rows = truth/outcome/disease (outcome- in row 1, outcome+ in row 2)
#' @param ci Confidence interval. Default is `0.95`
#' @param method Function for testing. Options: `"best"` (default), `"chi"`, `"fisher"`
#' @param n_min_chisq Minimum group size that will be allowed for chi-squared test. Default is `5`. Only relevant when `method = "best"`
#' @returns Data frame containing columns for rr, rr_lower, rr_upper, and either p, label, method or p_fisher and p_chi
#' @export
rel_risk <- function(..., ci = 0.95, method = c("both", "best", "chi", "fisher"), n_min_chisq = 5) {
  method <- match.arg(method, choices = c("both", "best", "chi", "fisher"))
  x <- matrix(c(...), nrow = 2)
  out <- .relative_risk_ci(x, ci = ci)
  switch(method,
         both = {
           list_to_df(c(out, p_fisher = p_fisher(x), p_chi = p_chi(x)))
         },
         best = {
           if (min(x) < n_min_chisq) {
             method <- "Fisher's exact test"
             pval <- p_fisher(x)
           } else {
             method <- "Chi-squared test"
             pval <- p_chi(x)
           }
           list_to_df(c(out, p = pval, label = sig_stars(pval), method = method))
         },
         chi = {
           method <- "Chi-squared test"
           pval <- p_chi(x)
           list_to_df(c(out, p = pval, label = sig_stars(pval), method = method))
         },
         fisher = {
           method <- "Fisher's exact test"
           pval <- p_fisher(x)
           list_to_df(c(out, p = pval, label = sig_stars(pval), method = method))
         })
}

#' Alias for rel_risk
#'
#' @rdname rel_risk
rr_ci <- rel_risk

#' Calculation of RR and CI
#'
#' @param x Matrix containing 2 x 2 counts. Columns = predictor/test result/risk factor/case vs. control (- or control in col 1, + or case in col 2), rows = truth/outcome/disease (outcome- in row 1, outcome+ in row 2)
#' @param ci Confidence interval. Default is `0.95`
#' @returns List containing rr, rr_lower, rr_upper
#' @noRd
.relative_risk_ci <- function(x, ci = 0.95) {
  if (anyNA(x) || any_zero(x)) {
    return(list(rr = NA_real_, rr_lower = NA_real_, rr_upper = NA_real_))
  }
  case_outcome <- x[2, 2]
  case_no_outcome <- x[1, 2]
  control_outcome <- x[2, 1]
  control_no_outcome <- x[1, 1]
  cases <- case_outcome + case_no_outcome
  controls <- control_outcome + control_no_outcome
  outcome <- case_outcome + control_outcome
  no_outcome <- case_no_outcome + control_no_outcome
  n <- cases + controls
  risk_controls <- control_outcome/controls
  risk_cases <- case_outcome/cases
  rr <- risk_cases/risk_controls
  log_rr <- log(rr)
  se <- sqrt(1/case_outcome + 1/control_outcome - 1/cases - 1/controls)
  conf_int <- exp(log_rr + qnorm(0.5 + ci/2)*se*c(-1, 1))
  attrib_risk <- abs(risk_cases - risk_controls)
  list(
    rr = rr, rr_lower = conf_int[1], rr_upper = conf_int[2],
    risk_cases = risk_cases, risk_controls = risk_controls,
    p = p, method = method, p_chi = p_chisq, p_fisher = p_fet,
    attrib_risk = attrib_risk, attrib_risk_perc = attrib_risk/perc_cases,
    nnt = 1/attrib_risk, rrr = 1 - rr,
    cases = cases, perc_cases = cases/n, controls = controls, perc_controls = controls/n,
    outcome = outcome, outcome_perc = outcome/n, no_outcome = no_outcome, perc_no_outcome = no_outcome/n,
    case_outcome = case_outcome, case_no_outcome = case_no_outcome, control_outcome = control_outcome, control_no_outcome = control_no_outcome)
}

# Cutpoints ---------------------------------------------------------------

#' Cutpoint for binary outcome variable
#'
#' Functionality from pROC package
#' @param df Data frame
#' @param outcome_var Binary outcome variable. Enter as quoted or unquoted variable name
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @returns Data frame containing columns for outcome variable (outcome_var), predictor variable (predictor_var), cutpoint, direction of predictor variable used to determine positivity (type), sens, spec, auc, n, above_cp, perc_above_cp, below_cp, perc_below_cp
#' @export
cutpoint_binary <- function(df, outcome_var, predictor_var)  {
  outcome_var <- get_input(outcome_var)
  predictor_var <- get_input(predictor_var)
  df <- df[complete.cases(df[, c(outcome_var, predictor_var)]), ]
  df <- df[order(.subset2(df, outcome_var), .subset2(df, predictor_var)), ]
  n <- .row_names_info(df, 2L)
  outcome <- as.character(df[[outcome_var]])
  levels <- unique.default(outcome)
  n_levels <- length(levels)
  if (n_levels != 2) {
    stop(sprintf("'outcome_var' in cutpoint_binary() must contain 2 levels, not %s (%s)", n_levels, paste0(levels, collapse = ", "), call. = FALSE))
  }
  y <- .subset2(df, predictor_var)
  z <- split(y, outcome)
  group1 <- .subset2(z, levels[1])
  group2 <- .subset2(z, levels[2])
  n1 <- length(group1)
  n2 <- length(group2)
  direction <- if (Median(group1) > Median(group2)) ">" else "<"
  unique_candidates <- sort.int(unique.default(c(group1, group2)))
  thresholds1 <- (c(-Inf, unique_candidates) + c(unique_candidates, Inf))/2
  thresholds2 <- (c(-Inf, unique_candidates)/2 + c(unique_candidates, Inf)/2)
  thresholds <- ifelse(abs(thresholds1) > 1e+100, thresholds2, thresholds1)
  if (any(ties <- thresholds %in% c(group1, group2))) {
    if (direction == ">") {
      for (tie_idx in which(ties)) {
        if (thresholds[tie_idx] == unique_candidates[tie_idx - 1]) {
        } else if (thresholds[tie_idx] == unique_candidates[tie_idx]) {
          thresholds[tie_idx] <- unique_candidates[tie_idx - 1]
        }
      }
    } else if (direction == "<") {
      for (tie_idx in which(ties)) {
        if (thresholds[tie_idx] == unique_candidates[tie_idx - 1]) {
          thresholds[tie_idx] <- unique_candidates[tie_idx]
        }
      }
    }
  }
  response <- c(rep(0, n1), rep(1, n2))
  predictor <- c(group1, group2)
  decreasing <- direction == "<"
  predictor_ordered <- order(predictor, decreasing = decreasing)
  predictor_sorted <- predictor[predictor_ordered]
  response.sorted <- response[predictor_ordered]
  tp <- cumsum(response.sorted == 1)
  fp <- cumsum(response.sorted == 0)
  se <- tp/n2
  sp <- (n1 - fp)/n1
  dups_pred <- Rev(duplicated.default(Rev(predictor_sorted)))
  dups_sesp <- duplicated.default(se) & duplicated.default(sp)
  dupes <- dups_pred | dups_sesp
  if (decreasing) {
    se <- rev(c(0, se[!dupes]))
    sp <- rev(c(1, sp[!dupes]))
  } else {
    se <- c(0, se[!dupes])
    sp <- c(1, sp[!dupes])
  }
  x_diffs <- sp[-1] - sp[-length(sp)]
  means_vert <- (se[-1] + se[-length(se)])/2
  optimal_cp <- se + sp
  max_cp <- max(optimal_cp)
  se <- se[optimal_cp == max_cp]
  sp <- sp[optimal_cp == max_cp]
  threshold <- thresholds[optimal_cp == max_cp]
  if (length(threshold) == 0) return(NULL)
  above_cp <- sum(y > threshold)
  below_cp <- sum(y <= threshold)
  optimal_cp <- optimal_cp[optimal_cp == max_cp]
  vec_to_df(
    outcome_var = outcome_var, predictor_var = predictor_var,
    cutpoint = threshold, type = if (direction == "<") ">" else "<",
    sens = se, spec = sp,
    auc = abs(sum(means_vert*x_diffs)),
    n = n,
    above_cp = above_cp, perc_above_cp = above_cp/n,
    below_cp = below_cp, perc_below_cp = below_cp/n)
}

#' Evaluate a specific cutpoint for classification of a binary variable
#'
#' @param df Data frame
#' @param outcome_var Binary outcome variable. Enter as quoted or unquoted variable name
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @param cp Cutpoints to be evaluated. Enter as numeric vector
#' @param cutpoint Alias for `cp`
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @returns Data frame with 1 row for each cutpoint and columns for outcome_var, predictor_var, type, cutpoint, sens, sens_lower, sens_upper, spec, spec_lower, spec_upper, p, label, method, p_fisher, p_chi, youden, cohen_kappa, cohen_kappa_lower, cohen_kappa_upper, tpr, fpr, tnr, fnr, ppv, npv, fdr, lr_pos, lr_neg, dor, dor_lower, dor_upper, accuracy, auc, gini, jaccard_index, n, cases, controls, pos, neg, tp, fn, fp, tn, perc_cases, perc_controls, perc_pos, perc_neg
#' @export
eval_cutpoint_binary <- function(df, outcome_var, predictor_var, cp, type = ">=", cutpoint = cp) {
  predictor <- get_input(predictor_var)
  formula <- create_formula(get_input(outcome_var), "y")
  if (length(cutpoint) == 1L) {
    out <- sens_spec.data.frame(df, outcome_var = outcome_var, predictor_var = predictor_var, cutpoint = cutpoint, type = type)
    df$y <- .cont_to_binary_01(.subset2(df, predictor), cutpoint = cutpoint, type = type)
    z <- glm_tidy(Glm(df, formula))
    out$or_glm <- z$or
    out$or_glm_lower <- z$or_lower
    out$or_glm_upper <- z$or_upper
    out$p <- z$p
    out
  } else {
    map_dfr(cutpoint, function(x) {
      out <- sens_spec.data.frame(x = df, outcome_var = outcome_var, predictor_var = predictor_var, type = type, cutpoint = x)
      df$y <- .cont_to_binary_01(.subset2(df, predictor), cutpoint = x, type = type)
      z <- tryCatch(glm_tidy(Glm(df, formula)), error = function(e) NULL)
      if (length(z) == 0L) {
        out$or_glm <- NA_real_
        out$or_glm_lower <- NA_real_
        out$or_glm_upper <- NA_real_
        out$p <- NA_real_
      } else {
        out$or_glm <- z$or
        out$or_glm_lower <- z$or_lower
        out$or_glm_upper <- z$or_upper
        out$p <- z$p
      }
      out
    })
  }
}

#' Evaluate all cutpoints for classification of a binary variable
#'
#' @rdname eval_cutpoint_binary
#' @param df Data frame
#' @param outcome_var Binary outcome variable. Enter as quoted or unquoted variable name
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @param min_prop Minimum proportion of all subjects in smaller of 2 groups when stratifying by cutpoint. Enter as numeric proportion. Default is `0.1`
#' @returns Data frame with 1 row for each unique value of predictor variable and columns for outcome_var, predictor_var, type, cutpoint, sens, sens_lower, sens_upper, spec, spec_lower, spec_upper, p, label, method, p_fisher, p_chi, youden, cohen_kappa, cohen_kappa_lower, cohen_kappa_upper, tpr, fpr, tnr, fnr, ppv, npv, fdr, lr_pos, lr_neg, dor, dor_lower, dor_upper, accuracy, auc, gini, jaccard_index, n, cases, controls, pos, neg, tp, fn, fp, tn, perc_cases, perc_controls, perc_pos, perc_neg
#' @export
eval_all_cutpoints_binary <- function(df, outcome_var, predictor_var, type = ">=", min_prop = 0.1)  {
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  unique_cps <- unique.default(df[[predictor_var]])
  min_max <- Quantile(df[[predictor_var]], probs = c(min_prop, 1 - min_prop))
  fn <- function(x) {
    tryCatch(sens_spec.data.frame(x = df, outcome_var = outcome_var, predictor_var = predictor_var, type = type, cutpoint = x), error = function(e) NULL)
  }
  unique_cps <- unique_cps[unique_cps >= min_max[1] & unique_cps <= min_max[2]]
  out <- map_dfr(unique_cps, fn)
  out[order(out$p), ]
}

# Helper functions --------------------------------------------------------

#' Counts for 2 x 2 classification
#'
#' @param outcome,predictor Outcome and predictor vectors respectively. No missing values allowed
#' @param cutpoint Cutpoint
#' @param type Type of evaluation. Options: `">="` (default), `">"`, `"<="`, `"<"`
#' @returns List containing total, cases, controls, positive, negative, tp, fn, fp, tn
#' @noRd
.summary_counts_2_by_2 <- function(outcome, predictor, cutpoint = NULL, type = ">=") {
  outcome_unique <- unique.default(outcome)
  n <- length(outcome_unique)
  if (n > 2L) {
    stop(sprintf("Outcome variable must contain only 2 levels, not %s levels (%s)", n, paste0(outcome_unique, collapse = ", ")), call. = FALSE)
  }
  predictor <- if (!is.null(cutpoint)) {
    fn <- match_fun(type)
    fn(predictor, cutpoint)
  } else {
    predictor_unique <- unique.default(predictor)
    n <- length(predictor_unique)
    if (n > 2L) {
      stop(sprintf("Predictor variable must contain only 2 levels, not %s levels (%s)", n, paste0(predictor_unique, collapse = ", ")), call. = FALSE)
    }
    c(FALSE, TRUE)[match(predictor, sort.int(predictor_unique))]
  }
  outcome <- c(FALSE, TRUE)[match(outcome, sort.int(outcome_unique))]
  n <- length(outcome)
  cases <- sum(outcome)
  pos <- sum(predictor)
  neg <- n - pos
  tp <- sum(predictor & outcome)
  fn <- cases - tp
  list(total = n, cases = cases, controls = n - cases, positive = pos, negative = neg, tp = tp, fn = fn, fp = pos - tp, tn = neg - fn)
}

#' Dichotomize a continuous variable into binary groups
#'
#' @param x Numeric vector
#' @param cutpoint Cutpoint
#' @param type Type of evaluation. Options: `">="` (default), `">"`, `"<="`, `"<"`
#' @returns Integer vector containing `0` or `1` for statements that evaluate to `FALSE` or `TRUE`, respectively. Missing values result in NA
#' @noRd
.cont_to_binary_01 <- function(x, cutpoint, type = ">=") {
  fn <- match_fun(type)
  as.integer(fn(x, cutpoint))
}
