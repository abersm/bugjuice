#' Grubbs test
#'
#' Determine whether the minimum or maximum value is an outlier
#' @param x Numeric vector
#' @param n_outliers Number of outliers to search for. Options: `1` (tests whether min or max value in x is an outlier), `2` (tests whether min or max values are outliers)
#' @param hypothesis_type Type of hypothesis test to perform. If `"one.sided"` (default), 1 sided test is performed. If `"two.sided"`, 2 sided test is performed. Enter as quoted hypothesis test type
#' @param test_min If `NULL` (default), test determines whether min or max value (whichever is further from mean value) is an outlier. If `TRUE`, outlier test performed on min value. If `FALSE`, outlier test performed on max value
#' @param ... Not used
#' @export
p_grubbs <- function(x, n_outliers = 2, hypothesis_type = "one.sided", test_min = NULL, ...) {
  x <- sort.int(x[!is.na(x)], method = "quick")
  x_sd <- SD(x)
  n <- length(x)
  if (n_outliers == 2) {
    p <- (x[n] - x[1])/x_sd
    f <- function(x, y, z = 0) {
      sqrt((2*(y - 1)*qt((1 - x)/(y*(y - 1)), y - 2)^2)/(y - 2 + qt((1 - x)/(y*(y - 1)), y - 2)^2)) - z
    }
    q <- p
    p <- vector()
    z <- f(0.9999, n)
    y <- f(2e-16, n)
    for (i in seq_along(q)) {
      if (q[i] > z) {
        pp <- 1
      } else if (q[i] < y) {
        pp <- 0
      } else {
        pp <- uniroot(f, c(0.001, 0.9999), z = q[i], y = n)$root
      }
      p <- c(p, pp)
    }
    pval <- 1 - p

  } else {
    m <- mean.default(x)
    test_min <- test_min %||% ((x[n] - m) < (m - x[1]))
    o <- if (test_min) x[1] else x[n]
    z <- abs(o - m)/x_sd
    z <- sqrt((z*z*n*(2 - n))/(z*z*n - (n - 1)^2))
    if (is.nan(z)) {
      pval <- 0
    } else {
      pval <- n*(1 - pt(z, n - 2))
      pval[pval > 1] <- 1
    }
  }
  if (hypothesis_type == "two.sided") {
    pval <- 2*pval
    if (pval > 1) {
      pval <- 2 - pval
    }
  }
  pval
}
