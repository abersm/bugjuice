#' Number of non-missing values
#'
#' @param x Vector
#' @importFrom stats aggregate AIC anova approx approxfun BIC binomial bw.bcv bw.SJ bw.ucv confint coef cor cor.test complete.cases fisher.test gaussian glm.fit integrate kmeans ks.test lm lm.fit loess model.frame model.response model.matrix optimize pchisq pf pnorm predict profile psignrank pt pwilcox qchisq qf qnorm qt runif shapiro.test smooth.spline spline splinefun TukeyHSD uniroot var
#' @export
N <- function(x) sum(!is.na(x))

#' Number of missing values
#'
#' @rdname N
#' @export
N_na <- function(x) sum(is.na(x))

#' Percent of values that are true or 1
#'
#' @rdname N
#' @export
Perc <- function(x) sum(x)/length(x)

#' Percent of values that are missing
#'
#' @rdname N
#' @export
Perc_na <- function(x) mean.default(is.na(x))

#' Difference between values in a vector
#'
#' Functionality from `base::diff`
#' @param x Numeric vector (not dates, time series, or matrices)
#' @param lag Difference in position between numbers to be compared. Enter as integer. Default is `1`
#' @return Numeric vector of length equal to `length(x) - lag`
#' @export
Diff <- function(x, lag = 1L) {
  n <- length(x)
  x[-seq_len(lag)] - x[-n:-(n - lag + 1L)]
}

#' Sum
#'
#' @param x Numeric or logical vector
#' @export
Sum <- function(x) sum(x, na.rm = TRUE)

#' Median
#'
#' Functionality from `stats::median`
#' @param x Numeric vector
#' @export
Median <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(NaN)
  z <- (n + 1L) %/% 2L
  if (n %% 2L == 1L) {
    sort.int(x, partial = z)[z]
  } else {
    mean.default(sort.int(x, partial = z + 0L:1L)[z + 0L:1L])
  }
}

#' Mean
#'
#' @param x Numeric vector
#' @export
Mean <- function(x) mean.default(x, na.rm = TRUE)

#' Min
#'
#' @param x Numeric or logical vector
#' @export
Min <- function(x) {
  if (length(x) == 0L) return(NaN)
  min(x, na.rm = TRUE)
}

#' Max
#'
#' @param x Numeric or logical vector
#' @rdname Min
#' @export
Max <- function(x) {
  if (length(x) == 0L) return(NaN)
  max(x, na.rm = TRUE)
}

#' Range
#'
#' @param x Numeric or logical vector
#' @return Min and max as length 2 numeric vector
#' @export
Range <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(c(NaN, NaN))
  c(min(x), max(x))
}

#' Variance
#'
#' @param x Numeric vector
#' @export
Var <- function(x) var(x, na.rm = TRUE, use = "na.or.complete")

#' Standard deviation
#'
#' @param x Numeric vector
#' @export
SD <- function(x) sqrt(Var(x))

#' Standard error
#'
#' @param x Numeric vector
#' @export
SE <- function(x) sqrt(Var(x)/N(x))

#' Quantile
#'
#' Functionality from `stats::quantile`
#' @param x Numeric vector
#' @param probs Probabilities. Enter numeric 0-1. Default uses min, Q1, median, Q3, max
#' @param type Type of quantile algorithm. Options: `7` (default), `6` (used by Prism)
#' @return Numeric vector of length equal to the length of probs
#' @export
Quantile <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), type = 7) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (type == 7) {
    index <- 1 + max(n - 1, 0)*probs
    lo <- floor(index)
    hi <- ceiling(index)
    x <- sort.int(x, partial = if (n == 0) numeric() else unique.default(c(lo, hi)))
    z <- x[lo]
    i <- which(index > lo & x[hi] != z)
    h <- (index - lo)[i]
    z[i] <- (1 - h)*z[i] + h*x[hi[i]]
    z
  } else {
    fuzz <- 4*.Machine$double.eps
    nppm <- probs*(n + 1)
    j <- floor(nppm + fuzz)
    h <- nppm - j
    if (any(sml <- abs(h) < fuzz)) {
      h[sml] <- 0
    }
    x <- sort.int(x, partial = unique.default(c(1, j[j > 0L & j <= n], (j + 1)[j > 0L & j < n], n)))
    x <- c(x[1L], x[1L], x, x[n], x[n])
    z <- x[j + 2L]
    z[h == 1] <- x[j + 3L][h == 1]
    other <- (h > 0) & (h < 1) & (x[j + 2L] != x[j + 3L])
    if (any(other)) {
      z[other] <- ((1 - h)*x[j + 2L] + h*x[j + 3L])[other]
    }
    z
  }
}

#' 25th percentile
#'
#' @param x Numeric vector
#' @export
Q1 <- function(x) Quantile(x, probs = 0.25)

#' 75th percentile
#'
#' @param x Numeric vector
#' @export
Q3 <- function(x) Quantile(x, probs = 0.75)

#' IQR
#'
#' @param x Numeric vector
#' @return Length 2 numeric vector containing 25th and 75th percentile values
#' @export
iqr <- function(x) Quantile(x, probs = c(0.25, 0.75))

#' Confidence interval difference from mean
#'
#' @param x Numeric vector
#' @param ci Confidence level. Default is `0.95`
#' @return Absolute difference between mean and either lower or upper bound of CI
#' @export
CI <- function(x, ci = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) return(NA_real_)
  qt(0.5 + ci/2, n - 1)*SD(x)/sqrt(n)
}

#' Confidence interval for proportions
#'
#' @param n Numerator of proportion
#' @param total Denominator of proportion
#' @param ci Type of confidence interval. Default is `0.95`
#' @param method Method for calculating confidence interval. Options include: `"wilson"` (default, also used by prism), `"wald"`, `"wald_corrected"`, `"agresti_coull"`, `"exact"`, `"asymptotic"`, `"all"`
#' @return Length 2 numeric vector containing lower and upper bounds of CI. If `method = "all"`, output is a matrix with 1 row for each method
#' @export
CI_prop <- function(n, total, ci = 0.95, method = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic", "all")) {
  method <- match.arg(method, choices = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic", "all"))
  l <- 0.5 + ci/2
  p <- n/total
  if (method == "wilson") {
    z_crit <- qnorm(l)
    z2 <- z_crit*z_crit
    ci_wilson <- (p + z2/2/total + c(-1, 1)*z_crit*sqrt((p*(1 - p) + z2/4/total)/total))/(1 + z2/total)
    if (n == 1) {
      ci_wilson[1] <- -log(ci)/total
    }
    if (n == total - 1) {
      ci_wilson[2] <- 1 + log(ci)/total
    }
    c(p, ci_wilson)
  } else if (method == "exact") {
    nu2 <- 2*n
    nu1 <- 2*total - nu2 + 2
    ll_exact <- if (n > 0) n/(n + qf(l, nu1, nu2)*(total - n + 1)) else 0
    z <- if (n < total) qf(l, nu2 + 2, nu1 - 2) else 1
    ul_exact <- (n + 1)*z/(total - n + (n + 1)*z)
    c(p, ll_exact, ul_exact)
  } else if (method %in% c("wald", "wald_corrected")) {
    z_crit <- qnorm(l)
    z <- z_crit*sqrt(p*(1 - p)/total)
    if (method == "wald_corrected") {
      z <- z + 0.5/n
    }
    c(p, max(0, p - z), min(1, p + z))
  } else if (method == "asymptotic") {
    z_crit <- qnorm(l)
    c(p, p + z_crit*sqrt(p*(1 - p)/total)*c(-1, 1))
  } else if (method == "agresti_coull") {
    z_crit <- qnorm(l)
    z2 <- z_crit*z_crit
    n_ac <- n + z2/2
    total_ac <- total + z2
    p_ac <- n_ac/total_ac
    z <- z_crit*sqrt(p_ac*p_ac*(1 - p_ac)/total_ac)
    c(p_ac, max(0, p_ac - z), min(1, p_ac + z))
  } else {
    y <- lapply(c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "asymptotic"), function(x) CI_prop(n = n, total = total, ci = ci, method = x))
    y <- matrix_to_df(Reduce(rbind, y))
    names(y) <- c("prop", "ci_lower", "ci_upper")
    y$method <- c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "asymptotic")
    y[c("method", "prop", "ci_lower", "ci_upper")]
  }
}

#' Coefficient of variation
#'
#' @param x Numeric vector
#' @export
CV <- function(x) SD(x)/Mean(x)*100

#' Difference between sequential numbers in a vector
#'
#' Functionality from `diff()` function in base package
#' @param x Numeric vector
#' @param lag Difference between position of 2 numbers to determine difference between. Default is `1`
#' @return Numeric vector with length equal to `length(x) - 1L`
#' @export
delta <- function(x, lag = 1L) {
  n <- length(x)
  if (lag >= n) return(x[0L])
  x[-seq_len(lag)] - x[-n:-(n - lag + 1L)]
}

# Geometric summary statistics --------------------------------------------

#' Geometric mean
#'
#' @param x Numeric vector
#' @param log_fn Function used to log transform data. Default is `log1p`
#' @param exp_fn Function used to back transform log data. Default is `expm1`
#' @export
geometric_mean <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- x[is.finite(x)]
  x_log <- log_fn(x)
  exp_fn(mean.default(x_log))
}

#' Geometric standard deviation
#'
#' @rdname geometric_mean
#' @export
geometric_sd <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- x[is.finite(x)]
  x <- log_fn(x)
  exp_fn(SD(x))
}

#' Geometric standard error
#'
#' @rdname geometric_mean
#' @export
geometric_se <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- x[is.finite(x)]
  x <- log_fn(x)
  exp_fn(sqrt(Var(x)/length(x)))
}

#' Geometric confidence interval
#'
#' @inheritParams geometric_mean
#' @param ci Type of confidence interval. Default is `0.95`
#' @export
geometric_ci <- function(x, log_fn = log1p, exp_fn = expm1, ci = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  x <- log_fn(x)
  exp_fn(qt(0.5+ ci/2, n - 1)*sqrt(Var(x)/n))
}

# Skewness/kurtosis -------------------------------------------------------

#' Skewness
#'
#' @param x Numeric vector
#' @param type Method used to calculate skewness. Options: `1` (default), `2`, `3`
#' @export
Skew <- function(x, type = 1) {
  x <- x[!is.na(x)]
  n <- length(x)
  x_mean <- mean.default(x)
  delta_x_mean <- x - x_mean
  if (type == 1) {
    x_sd <- SD(x)
    sum(delta_x_mean^3)/(n*x_sd^3)
  } else if (type == 2) {
    n*sqrt(n - 1)*(sum(delta_x_mean^3)/((n - 2)*sum(delta_x_mean*delta_x_mean)^(1.5)))
  } else {
    sqrt(n)*(sum(delta_x_mean^3)/(sum(delta_x_mean*delta_x_mean)^(1.5)))
  }
}

#' Kurtosis
#'
#' @rdname Skew
#' @export
Kurtosis <- function(x, type = 1) {
  x <- x[!is.na(x)]
  n <- length(x)
  x_mean <- mean.default(x)
  delta_x_mean <- x - x_mean
  if (type == 1) {
    x_sd <- SD(x)
    sum(delta_x_mean^4)/(n*x_sd^4)-3
  } else if (type == 2) {
    n*sum(delta_x_mean^4)/(sum(delta_x_mean*delta_x_mean)^2)-3
  } else {
    x_n1 <- n - 1
    x_n2 <- x_n1 - 1
    x_n3 <- x_n2 - 1
    n*(n + 1)*sum(delta_x_mean^4)/(x_n1*x_n2*x_n3*(sum(delta_x_mean*delta_x_mean)/x_n1)^2)-3*x_n1^2/(x_n2*x_n3)
  }
}
