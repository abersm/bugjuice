# F test (variance test) --------------------------------------------------

#' F test (aka var.test)
#'
#' @param df Data frame
#' @param formula Formula entered as continuous variable ~ grouping variable
#' @param y Continuous variable. Enter as quoted variable name
#' @param x Categorical variable. Enter as quoted variable name
#' @param otherwise Output if F test fails. Default is `0`
#' @returns P value as length 1 numeric vector
#' @export
p_F_test <- function(df, formula = NULL, y = NULL, x = NULL, otherwise = 0) {
  vars <- formula2vars(formula = formula, x = x, y = y, parent_fn = "p_F_test")
  values <- split(df[[vars$y]], df[[vars$x]])
  if (length(values) != 2) return(otherwise)
  tryCatch(suppressWarnings(.p_F_test(values[[1L]], values[[2L]], otherwise = otherwise)), error = function(e) otherwise)
}

#' P value for F test (aka variance test)
#'
#' @param x,y Values for each group. Must have missing values removed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param otherwise Output if F test fails. Default is `0`
#' @param ... Not used
.p_F_test <- function(x, y, hypothesis_type = "two.sided", otherwise = 0, ...) {
  x_len <- length(x)
  y_len <- length(y)
  x_deg_free <- x_len - 1
  y_deg_free <- y_len - 1
  if (min(x_len, y_len) < 2) return(otherwise)
  x_var <- Var(x)
  y_var <- Var(y)
  pval <- pf(x_var/y_var, x_deg_free, y_deg_free)
  switch(hypothesis_type, two.sided = 2*min(pval, 1 - pval), less = pval, greater = 1 - pval)
}

# Levene test -------------------------------------------------------------

#' P value for Levene test
#'
#' @param df Data frame
#' @param formula Formula entered as continuous variable ~ grouping variable
#' @param x Continuous variable. Enter as quoted variable name
#' @param y Categorical variable. Enter as quoted variable name
#' @param na.rm If `TRUE` (default), observations with missing values for the grouping variable are removed prior to analysis
#' @param brown_forsythe If `TRUE` (default), Brown Forsythe version of Levene test performed (using median). If `FALSE`, original Levene test (using mean) performed
#' @param otherwise Output if F test fails. Default is `0`
#' @param ... Not used
#' @returns P value as length 1 numeric vector. If P < 0.05, variance is not same in each group
#' @export
p_levene <- function(df, formula = NULL, x = NULL, y = NULL, na.rm = TRUE, brown_forsythe = TRUE, otherwise = 0, ...) {
  summary_fn <- if (brown_forsythe) Median else Mean
  vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_levene")
  y <- vars$y
  x <- vars$x
  df <- droplevels(df[c(y, x)])
  df <- if (na.rm) df[complete.cases(df), ] else df[complete.cases(df[, y]), ]
  y <- df[[y]]
  x <- df[[x]]
  x <- factor(x)
  group_summary <- tapply(y, x, summary_fn)
  y <- abs(y - group_summary[x])
  tryCatch(suppressWarnings(anova(lm(y ~ x))$`Pr(>F)`[1L]), error = function(e) otherwise)
}

#' P value for Levene test
#'
#' @param x,y Values for each group
#' @param brown_forsythe If `TRUE` (default), Brown Forsythe version of Levene test performed (using median). If `FALSE`, original Levene test (using mean) performed
#' @param otherwise Output if Levene test fails. Default is `0`
#' @param ... Not used
#' @export
.p_levene <- function(x, y, brown_forsythe = TRUE, otherwise = 0, ...) {
  summary_fn <- if (brown_forsythe) Median else Mean
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  x_len <- length(x)
  y_len <- length(y)
  values <- c(x, y)
  groups <- c(rep("a", times = x_len), rep("b", times = y_len))
  groups <- factor(groups)
  group_summary <- tapply(values, groups, summary_fn)
  values <- abs(values - group_summary[groups])
  tryCatch(suppressWarnings(anova(lm(values ~ groups))$`Pr(>F)`[1]), error = function(e) otherwise)
}

# Bartlett test -----------------------------------------------------------

#' Bartlett test
#'
#' @rdname p_levene
#' @export
p_bartlett <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, otherwise = 0, ...) {
  vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_bartlett")
  y <- vars$y
  x <- vars$x
  df <- droplevels(df)
  df <- if (na.rm) df[complete.cases(df), ] else df[complete.cases(df[, y]), ]
  .p_bartlett(split(df[[y]], df[[x]]), otherwise = otherwise)
}

#' P value for Bartlett test
#'
#' @param ... Vectors containing values for each group or a list of vectors
#' @param otherwise Output if Bartlett test fails. Default is `0`
#' @export
.p_bartlett <- function(..., otherwise = 0) {
  y <- if (is.list(y <- c(...))) y else list(...)
  y <- lapply(y, function(x) x[!is.na(x)])
  k <- length(y)
  if (k < 2) return(otherwise)
  n <- lengths(y) - 1
  if (any(n == 0)) return(otherwise)
  v <- vapply(y, Var, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  n_total <- sum(n)
  v_total <- sum(n*v)/n_total
  z <- ((n_total*log(v_total) - sum(n*log(v)))/(1 + (sum(1/n) - 1/n_total)/(3*(k - 1))))
  tryCatch(pchisq(z, k - 1, lower.tail = FALSE), error = function(e) otherwise)
}
