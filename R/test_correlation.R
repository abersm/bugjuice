#' P value for correlation test
#'
#' @param df Data frame
#' @param formula Formula in format y ~ x (same as x ~ y)
#' @param x,y Column names containing data to correlate. Enter as quoted variable names
#' @param method Method for correlation. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param otherwise Value to return if unable to perform correlation test
#' @export
p_cor <- function(df, formula = NULL, x = NULL, y = NULL, method = c("spearman", "pearson", "kendall"), otherwise = NA_real_) {
  tryCatch(cor_test(df, formula = formula, x = x, y = y, method = method)$p, error = function(e) otherwise)
}

#' Correlation coefficient and 95% CI
#'
#' @param df Data frame
#' @param formula Formula in format y ~ x (same as x ~ y)
#' @param x,y Column names containing data to correlate. Enter as quoted variable names
#' @param method Method for correlation. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Default is `0.95`
#' @returns List of 4 numeric vectors (all length 1): "cor_coeff", "cor_lower", "cor_upper", "p"
#' @export
cor_test <- function(df, formula = NULL, x = NULL, y = NULL, method = c("spearman", "pearson", "kendall"), ci = 0.95) {
  method <- match.arg(method, choices = c("spearman", "pearson", "kendall"))
  if (!is.null(formula)) {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- df[c(x, y)]
  df <- df[complete.cases(df), ]
  res <- suppress(cor.test(df[[x]], df[[y]], method = method, conf.level = ci))
  list(cor_coeff = unname(res$estimate), cor_lower = res$conf.int[1L], cor_upper = res$conf.int[2L], p = res$p.value)
}
