# Summary statistics - 2 variables ----------------------------------------

#' Percent change
#'
#' @param x0,x1 Baseline/reference (`x0`) and subsequent (`x1`) values respectively
#' @param digits Number of digits to include after decimal. Default is `1`
#' @returns Percentage change in value expressed as a percentage of starting value (`x0`)
#' @export
perc_change <- function(x0, x1, digits = 1) {
  x0 <- ifelse(x0 == 0, NA_real_, x0)
  round_up((x1 - x0)/x0*100, digits = digits)
}

# Mean difference ---------------------------------------------------------

#' Mean difference with confidence intervals
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of `x` and `y` must be the same and their lengths must be equal
#' @param test_fn Function used for testing. Options: `NULL` (default, uses normality test to determine whether to use t-test or Wilcoxon ranked sum test), `diff_mean_ttest`, `diff_mean_wilcox`
#' @param normality_test Normality test used to determine method for calculating mean difference. Default is `p_shapiro.` Only relevant when `test_fn` is `NULL`
#' @param ci Type of confidence interval. Default is `0.95`
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`
#' @param ... Arguments passed to either `diff_mean_ttest` or `diff_mean_wilcox`
#' @returns Length 3 numeric vector containing mean difference (diff_mean), lower ci (ci_lower), and upper ci (ci_upper)
#' @export
diff_mean <- function(x, y, test_fn = NULL, normality_test = p_shapiro, ci = 0.95, paired = FALSE, hypothesis_type = "two.sided", ...) {
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided", "less", "greater"))
  fn <- test_fn %||% (if (normality_test(x) < 0.05 || normality_test(y) < 0.05) diff_mean_wilcox else diff_mean_ttest)
  if (paired) {
    x <- x - y
    x <- x[!is.na(x)]
    y <- NULL
  } else {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  fn(x = x, y = y, ci = ci, paired = paired, hypothesis_type = hypothesis_type, ...)
}

#' t-test estimation of mean difference
#'
#' @param x,y Values in group 1 and 2 respectively. If `paired`, `y` is ignored
#' @param ci Confidence level. Default is `0.95`
#' @param welch If `NULL` (default), use of Welch's correction is determined by variance test (If P < 0.05, variance is not equal between groups and Welch's correction is applied). If `TRUE`, Welch's correction is applied, otherwise student's t-test is performed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param variance_test Function used to perform variance test. Must take numeric vector and returns P value. Default is `p_F_test`
#' @param hypothesis_type Type of hypothesis test to perform. If `"two.sided"` (default), 2 sided test is performed. Other options: `"less"`, `"greater"`
#' @param ... Not used
#' @returns Length 3 numeric vector containing mean difference (diff_mean), lower ci (ci_lower), and upper ci (ci_upper)
#' @export
diff_mean_ttest <- function(x, y = NULL, ci = 0.95, welch = NULL, paired = FALSE, variance_test = .p_F_test, hypothesis_type = "two.sided", ...) {
  x_length <- length(x)
  if (x_length < 3) return(c(diff_mean = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
  x_mean <- mean.default(x)
  x_var <- Var(x)
  if (paired) {
    deg_free <- x_length - 1
    stderr <- sqrt(x_var/x_length)
    if (stderr < 10*.Machine$double.eps*abs(x_mean)) return(c(diff_mean = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
    mean_difference <- x_mean
  } else {
    y_length <- length(y)
    if (y_length < 2) return(c(diff_mean = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
    y_mean <- mean.default(y)
    y_var <- Var(y)
    welch <- welch %||% (variance_test(x = x, y = y, hypothesis_type = hypothesis_type, otherwise = 0) < 0.05)
    if (welch) {
      stderrx <- sqrt(x_var/x_length)
      stderry <- sqrt(y_var/y_length)
      stderr <- sqrt(stderrx^2 + stderry^2)
      deg_free <- stderr^4/(stderrx^4/(x_length - 1) + stderry^4/(y_length - 1))
    } else {
      deg_free <- x_length + y_length - 2
      v <- (x_length - 1)*x_var + (y_length - 1)*y_var
      v <- v/deg_free
      stderr <- sqrt(v*(1/x_length + 1/y_length))
    }
    if (stderr < 10*.Machine$double.eps*max(abs(x_mean), abs(y_mean))) return(c(diff_mean = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_))
    mean_difference <- x_mean - y_mean
  }
  tstat <- mean_difference/stderr
  conf_interval <- switch(hypothesis_type,
                          two.sided = tstat + qt(0.5 + ci/2, deg_free)*c(-1, 1),
                          less = c(-Inf, tstat + qt(ci, deg_free)),
                          greater = c(tstat - qt(ci, deg_free), Inf))
  values <- c(mean_difference, conf_interval*stderr)
  names(values) <- c("diff_mean", "ci_lower", "ci_upper")
  values
}

#' Wilcoxon estimation of mean difference
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of `x` and `y` must be the same and their lengths must be equal. Must have missing values removed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param correct If `TRUE` (default), continuity correction is applied
#' @param ci Confidence level. Default is `0.95`
#' @param ... Not used
#' @returns Length 3 numeric vector containing mean difference (diff_mean), lower ci (ci_lower), and upper ci (ci_upper)
#' @export
diff_mean_wilcox <- function(x, y = NULL, hypothesis_type = c("two.sided", "less", "greater"), paired = FALSE, correct = TRUE, ci = 0.95, ...) {
  if (paired) {
    x <- x - y
    if (any(x == 0)) {
      x <- x[x != 0]
    }
    n <- length(x)
    # r <- rank(abs(x))
    # statistic <- sum(r[x > 0])
    alpha <- 1 - ci
    if (n > 0) {
      mumin <- min(x)
      mumax <- max(x)
      W <- function(d) {
        xd <- x - d
        xd <- xd[xd != 0]
        nx <- length(xd)
        dr <- rank(abs(xd))
        zd <- sum(dr[xd > 0]) - nx*(nx + 1)/4
        ci_n_ties <- table(dr)
        ci_sigma <- sqrt(nx*(nx + 1)*(2*nx + 1)/24 - sum(ci_n_ties^3 - ci_n_ties)/48)
        ci_correction <- if (!correct) 0 else switch(hypothesis_type, two.sided = sign(zd)*0.5, greater = 0.5, less = -0.5)
        (zd - ci_correction)/ci_sigma
      }
      Wmumin <- W(mumin)
      Wmumax <- if (!is.finite(Wmumin)) NA else W(mumax)
    }
    if (n == 0 || !is.finite(Wmumax)) {
      conf_interval <- c(if (hypothesis_type == "less") -Inf else NaN, if (hypothesis_type == "greater") Inf else NaN)
      mean_difference <- if (n > 0) (mumin + mumax)/2 else NaN
    } else {
      wdiff <- function(d, zq) W(d) - zq
      root <- function(zq) {
        uniroot(wdiff, lower = mumin, upper = mumax, f.lower = Wmumin - zq, f.upper = Wmumax - zq, tol = 1e-04, zq = zq)$root
      }
      conf_interval <- switch(hypothesis_type,
                     two.sided = {
                       repeat {
                         mindiff <- Wmumin - qnorm(alpha/2, lower.tail = FALSE)
                         maxdiff <- Wmumax - qnorm(alpha/2)
                         if (mindiff < 0 || maxdiff > 0) alpha <- alpha*2 else break
                       }
                       if (alpha < 1) {
                         c(root(zq = qnorm(alpha/2, lower.tail = FALSE)), root(zq = qnorm(alpha/2)))
                       } else {
                         rep(Median(x), 2)
                       }
                     },
                     greater = {
                       repeat {
                         mindiff <- Wmumin - qnorm(alpha, lower.tail = FALSE)
                         if (mindiff < 0) alpha <- alpha*2 else break
                       }
                       l <- if (alpha < 1) root(zq = qnorm(alpha, lower.tail = FALSE)) else Median(x)
                       c(l, +Inf)
                     },
                     less = {
                       repeat {
                         maxdiff <- Wmumax - qnorm(alpha/2)
                         if (maxdiff > 0) alpha <- alpha*2 else break
                       }
                       u <- if (alpha < 1) root(zq = qnorm(alpha)) else Median(x)
                       c(-Inf, u)
                     })
      mean_difference <- uniroot(W, lower = mumin, upper = mumax, tol = 1e-04)$root
    }
  } else {
    x_length <- length(x)
    y_length <- length(y)
    # r <- rank(c(x, y))
    # statistic <- c(W = sum(r[seq_len(x_length)]) - x_length*(x_length + 1)/2)
    alpha <- 1 - ci
    x_range <- Range(x)
    y_range <- Range(y)
    mumin <- x_range[1] - y_range[2]
    mumax <- x_range[2] - y_range[1]
    W <- function(d) {
      dr <- c(x - d, y)
      dr <- rank(dr)
      ci_n_ties <- table(dr)
      dz <- sum(dr[seq_len(x_length)]) - x_length*(x_length + 1)/2 - x_length*y_length/2
      ci_correction <- if (!correct) 0 else switch(hypothesis_type, two.sided = sign(dz)*0.5, greater = 0.5, less = -0.5)
      ci_sigma <- sqrt(x_length*y_length/12*((x_length + y_length + 1) - sum(ci_n_ties^3 - ci_n_ties)/((x_length + y_length)*(x_length + y_length - 1))))
      (dz - ci_correction)/ci_sigma
    }
    wdiff <- function(d, zq) W(d) - zq
    Wmumin <- W(mumin)
    Wmumax <- W(mumax)
    root <- function(zq) {
      f.lower <- Wmumin - zq
      if (f.lower <= 0) return(mumin)
      f.upper <- Wmumax - zq
      if (f.upper >= 0) return(mumax)
      uniroot(wdiff, lower = mumin, upper = mumax, f.lower = f.lower, f.upper = f.upper, tol = 1e-04, zq = zq)$root
    }
    conf_interval <- switch(hypothesis_type, two.sided = c(root(zq = qnorm(alpha/2, lower.tail = FALSE)), root(zq = qnorm(alpha/2))), greater = c(root(zq = qnorm(alpha, lower.tail = FALSE)), Inf), less = c(-Inf, root(zq = qnorm(alpha))))
    mean_difference <- uniroot(W, lower = mumin, upper = mumax, tol = 1e-04)$root
  }
  values <- c(mean_difference, conf_interval)
  names(values) <- c("diff_mean", "ci_lower", "ci_upper")
  values
}

#' Alias for diff_mean_wilcox
#'
#' @rdname diff_mean_wilcox
#' @export
diff_mean_mann_whitney <- diff_mean_wilcox

#' Difference and CI between proportions
#'
#' @param x Table (2 x 2) in which columns represent controls (1st column) and cases (2nd column) and rows represent negative (1st row) and positive (2nd row) individuals
#' @param ci Confidence interval. Default is `0.95`
#' @param yates If `FALSE` (default), Yates continuity correction is not used If `TRUE` and x is 2 x 2, Yates' continuity correction is applied
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @returns Length 3 numeric vector containing diff_prop, lower_ci, upper_ci
#' @export
diff_prop <- function(x, ci = 0.95, yates = FALSE, hypothesis_type = c("two.sided", "less", "greater")) {
  hypothesis_type <- match.arg(hypothesis_type)
  n <- rowSums(x)
  x <- x[, 1L]
  p <- x/n
  yates <- if (yates) 0.5 else 0
  p_diff <- p[1L] - p[2L]
  yates <- min(yates, abs(p_diff)/sum(1/n))
  w <- if (hypothesis_type == "two.sided") qnorm(0.5 + ci/2) else  qnorm(ci)
  w <- w*sqrt(sum(p*(1 - p)/n)) + yates*sum(1/n)
  conf_int <- switch(hypothesis_type, two.sided = c(max(p_diff - w, -1), min(p_diff + w, 1)), greater = c(max(p_diff - w, -1), 1), less = c(-1, min(p_diff + w, 1)))
  c(diff_prop = p_diff, ci_lower = conf_int[1], ci_upper = conf_int[2])
}

#' Find elbow point in curve
#'
#' @param df Data frame
#' @param formula Enter in y ~ x format
#' @returns Coordinates of elbow point as length 2 numeric vector
#' @export
find_elbow <- function(df, formula) {
  y <- all.vars(formula)
  x <- y[2L]
  y <- y[1L]
  df <- remove_na(df, c(x, y))
  x <- df[[x]]
  y <- df[[y]]
  max_x_x <- max(x)
  max_x_y <- y[which.max(x)]
  max_y_y <- max(y)
  max_y_x <- x[which.max(y)]

  # Creating straight line between the max values
  fit1 <- Lm(vec_to_df(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y)), y ~ x)$coefficients
  fit2 <- fit1[2L]
  fit1 <- fit1[1L]
  distances <- c()
  for (i in seq_along(x)) {
    distances <- c(distances, abs(fit2*x[i] - y[i] + fit1)/abs(fit2))
  }
  idx <- which.max(distances)
  c(x[idx], y[idx])
}

#' Random number
#'
#' @param min,max Minimum and maximum values allowed for random number
#' @returns Length 1 integer
#' @export
random_number <- function(min = 1, max = 100) floor(runif(n = 1, min = min, max = max))
