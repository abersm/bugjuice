# Shapiro-Wilk test -------------------------------------------------------

#' Shapiro-Wilk test
#'
#' @param x Numeric vector. Missing values allowed. Need at least 3 values to get a reliable result for `p_shapiro` (8 for `p_dagostino`)
#' @param otherwise Output if normality test fails. Default is `0`
#' @returns P-value as length 1 numeric vector
#' @export
p_shapiro <- function(x, otherwise = 0) {
  tryCatch(suppressWarnings(shapiro.test(x)$p.value), error = function(e) otherwise)
}

# D'Agostino-Pearson omnibus test -----------------------------------------

#' D'Agostino-Pearson omnibus test
#'
#' Functionality from fBasics package
#' @rdname p_shapiro
#' @export
p_dagostino <- function(x, otherwise = 0) {
  x <- x[!is.na(x)]
  x_len <- length(x)
  if (x_len < 8) return(otherwise)
  z <- x - mean.default(x)
  s <- sqrt(mean.default(z^2))
  a3 <- mean.default(z^3)/(s^3)
  a4 <- mean.default(z^4)/(s^4)
  n1 <- x_len + 1
  SD3 <- sqrt(6*(x_len - 2)/(n1*(n1 + 2)))
  SD4 <- sqrt(24*(x_len - 2)*(x_len - 3)*x_len/((n1^2)*(n1 + 2)*(n1 + 4)))
  U3 <- a3/SD3
  U4 <- (a4 - 3 + 6/n1)/SD4
  b  <- (3*(x_len^2 + 27*x_len - 70)*n1*(n1 + 2))/((x_len - 2)*(n1 + 4)*(n1 + 6)*(n1 + 8))
  W2 <- sqrt(2*(b - 1)) - 1
  delta <- 1/sqrt(log(sqrt(W2)))
  a <- sqrt(2/(W2 - 1))
  Z3 <- delta*log((U3/a) + sqrt((U3/a)^2 + 1))
  B <- (6*(x_len*x_len - 5*x_len + 2)/((n1 + 6)*(n1 + 8)))*sqrt((6*(n1 + 2)*(n1 + 4))/(x_len*(x_len - 2)*(x_len - 3)))
  A <- 6 + (8/B)*((2/B) + sqrt(1 + 4/(B^2)))
  jm <- sqrt(2/(9*A))
  pos <- ((1 - 2/A)/(1 + U4*sqrt(2/(A - 4))))^(1/3)
  Z4 <- (1 - 2/(9*A) - pos)/jm
  omni <- Z3^2 + Z4^2
  pchisq(omni, 2, lower.tail = FALSE)
}

# Anderson-Darling test ---------------------------------------------------

#' Anderson-Darling test
#'
#' Functionality from nortest package
#' @rdname p_shapiro
#' @export
p_anderson <- function(x, otherwise = 0) {
  x <- sort.int(x[!is.na(x)], method = "quick")
  x_len <- length(x)
  if (x_len < 8) return(otherwise)
  z <- x - mean.default(x)
  x_sd <- SD(x)
  logp1 <- pnorm(z/x_sd, log.p = TRUE)
  logp2 <- pnorm(-z/x_sd, log.p = TRUE)
  h <- (2*seq_len(x_len) - 1)*(logp1 + Rev(logp2))
  A <- -x_len - mean.default(h)
  AA <- (1 + 0.75/x_len + 2.25/x_len^2)*A
  if (AA < 0.2) {
    1 - exp(-13.436 + 101.14*AA - 223.73*AA^2)
  } else if (AA < 0.34) {
    1 - exp(-8.318 + 42.796*AA - 59.938*AA^2)
  } else if (AA < 0.6) {
    exp(0.9177 - 4.279*AA - 1.38*AA^2)
  } else if (AA < 10) {
    exp(1.2937 - 5.709*AA + 0.0186*AA^2)
  } else {
    3.7e-24
  }
}

# Kolmogorov-Smirnov test -------------------------------------------------

#' Kolmogorov-Smirnov test
#'
#' Input can include missing values
#' @rdname p_shapiro
#' @export
p_ks <- function(x, otherwise = 0) tryCatch(ks.test(x)$p.value, error = function(e) otherwise)

# Check for normality -----------------------------------------------------

#' Determine whether values in 2 groups are both normally distributed
#'
#' @inheritParams compare_means
#' @returns `TRUE` if normally distributed (all P >= 0.05), `FALSE` if not normally distributed (any P < 0.05)
#' @export
is_normal <- function(df, formula = NULL, y = NULL, x = NULL, normality_test = p_shapiro, ...) {
  if (!is.null(formula)) {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- df[, c(y, x)]
  df <- df[complete.cases(df), ]
  p_vals <- tapply(df[[y]], df[[x]], normality_test, ...)
  all(p_vals >= 0.05)
}
