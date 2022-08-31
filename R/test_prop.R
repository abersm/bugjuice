#' P value from either chi-squared or Fisher's exact test depending on group size
#'
#' @param x Matrix or table containing counts. If `x` is a vector of counts, enter as tn, fn, fp, tp. Values entered into matrix by column. Middle 2 numbers must share true/false designation (i.e. either tp and tn or fp and fn)
#' @param test Function for testing. Options: NULL (function determined by group size), p_chi, p_fisher
#' @param n_min_chisq Minimum group size that will be allowed for chi-squared test. Default is 5. Only relevant when test is NULL
#' @param otherwise Value to return if P value can't be determined
#' @param ... Arguments passed to `p_chi()` or `p_fisher()`
#' @returns P value as length 1 numeric vector
#' @export
p_prop <- function(x, test = NULL, n_min_chisq = 5, otherwise = NA_real_, ...) {
  test_fn <- test %||% if (min(x) >= n_min_chisq) p_chi else p_fisher
  test_fn(x, otherwise = otherwise, ...)
}

#' P value for Pearson's Chi-squared test
#'
#' Functionality from stats package
#' @rdname p_prop
#' @param yates If `FALSE` (default), Yates continuity correction is not used If TRUE and `x` is 2 x 2, Yates' continuity correction is applied
#' @export
p_chi <- function(x, yates = FALSE, otherwise = NA_real_, ...) {
  if (anyNA(x)) return(otherwise)
  x <- if (is.matrix(x)) x else matrix(x, nrow = 2)
  nrows <- nrow(x)
  ncols <- ncol(x)
  sum_rows <- rowSums(x)
  sum_cols <- colSums(x)
  E <- tcrossprod(sum_rows, sum_cols)/sum(x)
  yates <- if (yates && nrows == 2 && ncols == 2) min(0.5, abs(x - E)) else 0
  X2 <- sum((abs(x - E) - yates)^2/E)
  deg_free <- (nrows - 1)*(ncols - 1)
  tryCatch(pchisq(X2, deg_free, lower.tail = FALSE), error = function(e) otherwise)
}

#' P value for Fisher's exact test
#'
#' @rdname p_prop
#' @export
p_fisher <- function(x, otherwise = NA_real_, ...) {
  x <- if (is.matrix(x)) x else matrix(x, nrow = 2)
  tryCatch(suppressWarnings(fisher.test(x, ...)$p.value), error = function(e) otherwise)
}

# McNemar's test ----------------------------------------------------------

#' McNemar's Chi-squared test
#'
#' Functionality from stats package
#' @param x,y Integer vector for each group
#' @param corrected If `TRUE` (default), continuity correction applied
#' @param otherwise Value returned if P value cannot be determined
#' @returns P value as length 1 numeric vector
#' @export
p_mcnemar <- function(x, y, corrected = TRUE, otherwise = NA_real_) {
  if (length(x) != length(y)) return(otherwise)
  idx_nna <- complete.cases(x, y)
  x <- factor(x[idx_nna])
  y <- factor(y[idx_nna])
  r <- length(attr(x, "levels"))
  if (r < 2) return(otherwise)
  if (length(attr(y, "levels")) != r) return(otherwise)
  x <- table(x, y)
  b <- r*(r - 1)/2
  y <- if (corrected && r == 2 && any(x - t.default(x) != 0)) abs(x - t.default(x)) - 1 else x - t.default(x)
  x <- x + t.default(x)
  z <- sum(y[upper.tri(x)]^2/x[upper.tri(x)])
  tryCatch(pchisq(z, b, lower.tail = FALSE), error = function(e) otherwise)
}

# Trend in proportions ----------------------------------------------------

#' Chi-squared test for trend in proportions
#'
#' Functionality from stats package
#' @param n_positive,n_total Integer vectors containing number of positive observations and total number of observations at each time point respectively
#' @returns P value as length 1 numeric vector
#' @export
p_prop_test_trend <- function(n_positive, n_total) {
  score <- seq_along(n_positive)
  p <- sum(n_positive)/sum(n_total)
  w <- n_total/p/(1 - p)
  a <- anova(lm(freq ~ score, data = list(freq = n_positive/n_total, score = score), weights = w))
  chisq <- a["score", "Sum Sq"]
  pchisq(as.numeric(chisq), 1, lower.tail = FALSE)
}

#' Cochrane Armitage test for trend
#'
#' Functionality from Andri Signorell's excellent package DescTools
#' @param x Table containing 2 columns (1 for each group). Rows contain values at each time point
#' @param hypothesis_type Type of hypothesis test. Options: `"two.sided"` (default), `"increasing"`, `"decreasing"`
#' @returns P value as length 1 numeric vector
#' @export
p_cochrane_armitage <- function(x, hypothesis_type = c("two.sided","increasing","decreasing")) {
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided","increasing","decreasing"))
  col1 <- x[, 1]
  dims <- dim(x)
  n_rows <- dims[1L]
  n_cols <- dims[2L]
  idx <- seq_len(n_rows)
  n_counts <- .rowSums(x, n_rows, n_cols)
  n <- sum(n_counts)
  r_bar <- sum(n_counts*idx)/n
  s2 <- sum(n_counts*(idx - r_bar)^2)
  out <- sum(col1)/n
  out <- sum(col1*(idx - r_bar))/sqrt(out*(1 - out)*s2)
  switch(hypothesis_type, two.sided = 2*pnorm(abs(out), lower.tail = FALSE), increasing = pnorm(out), decreasing = pnorm(out, lower.tail = FALSE))
}
