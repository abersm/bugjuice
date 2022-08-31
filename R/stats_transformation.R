#' Convert units
#'
#' @param x Quantity to convert. Units of `x` specified in `from` argument
#' @param from Units for `x`. Options: `"in"`, `"cm"`, `"mm"`, `"pt"`, `"lines"`, `"npc"`, `"C"`, `"F"`. Enter as quoted units
#' @param to Desired output units. Same options as `from`. Enter as quoted unis
#' @param pt_per_in Points per inch. Default is `72.27` (used by ggplot2). Powerpoint uses 72.009
#' @return Value expressed in units determined by `to` argument
#' @export
convert <- function(x, from, to, pt_per_in = 72.27) {
  from <- match.arg(from, choices = c("inches", "cm", "mm", "pts", "lines", "npc", "C", "F"))
  to <- match.arg(to, choices = c("inches", "cm", "mm", "pts", "lines", "npc", "C", "F"))
  if (from %!in% c("C", "F")) {
    z <- c(1, 2.54, 25.4, pt_per_in, 5, 0.1130298)
    names(z) <- c("inches", "cm", "mm", "pts", "lines", "npc")
    z <- x/z[from]*z[to]
    names(z) <- NULL
    z
  } else if (from %in% c("C", "F") && to %in% c("C", "F")) {
    if (from == "C" && to == "F") {
      x*(9/5) + 32
    } else if (from == "F" && to == "C") {
      (x-32)*(5/9)
    } else {
      x
    }
  } else {
    stop("Units used for 'from' and 'to' arguments of 'convert' must be one of inches, cm, mm, pts, lines, npc, C, or F", call. = FALSE)
  }
}

#' Convert rates between units of time
#'
#' @param x Numeric vector
#' @param time_from Time units for `x`. Options: `"seconds"`, `"minutes"`, `"hours"`, `"days"`, `"weeks"`, `"months"`, `"years"`
#' @param time_to Goal units for output. Options: `"seconds"`, `"minutes"`, `"hours"`, `"days"`, `"weeks"`, `"months"`, `"years"`
#' @param time_multiplier Multiplier for units of time in output (i.e. output will be divided by `time_multiplier`. If `time_to = "weeks", time_multiplier = 2`, output is in units per 2 weeks). Default is `1`
#' @return Rate in time units specified by `time_to`
#' @export
convert_rate_time <- function(x, time_from = c("seconds", "minutes", "hours", "days", "weeks", "months", "years"), time_to = c("seconds", "minutes", "hours", "days", "weeks", "months", "years"), time_multiplier = 1) {
  unit_choices <- c("seconds", "minutes", "hours", "days", "weeks", "months", "years")
  time_from <- match.arg(arg = time_from, choices = unit_choices)
  time_to <- match.arg(arg = time_to, choices = unit_choices)
  z <- c(1, 60, 3600, 86400, 604800, 2592000, 31536000)
  names(z) <- unit_choices
  x <- x/z[time_from]*z[time_to]/time_multiplier
  names(x) <- NULL
  x
}

#' Calculate log10 + 1
#'
#' @param x Numeric vector
#' @returns Numeric vector with length equal to input
#' @export
log10_1 <- function(x) log10(x + 1)

#' Z-score
#'
#' @param x Numeric vector
#' @export
z_score <- function(x) (x - Mean(x))/SD(x)

#' Alias for `z_score()`
#'
#' @rdname z_score
#' @export
standardize <- z_score

#' Normalize numeric vector
#'
#' @param x Numeric vector
#' @param desired_range Desired range for output values. If `NULL`, normalize to z-score. Enter as length 2 numeric vector containing desired minimum and maximum values
#' @returns Numeric vector with values transformed to make the range equal to `desired_range`
#' @export
normalize <- function(x, desired_range = NULL) {
  rng <- Range(x)
  if (has_zero_range(rng)) {
    return(ifelse(is.na(x), NA_real_, mean.default(desired_range %||% 0)))
  }
  if (is.null(desired_range)) {
    z_score(x)
  } else {
    delta_rng <- rng[2L] - rng[1L]
    delta_desired_rng <- desired_range[2L] - desired_range[1L]
    (x - rng[1L])/delta_rng*delta_desired_rng + desired_range[1L]
  }
}

#' Normalize subset of columns by row to specified range
#'
#' @param df Data frame
#' @param ... Columns to include. Enter using tidyselect syntax
#' @param desired_range Desired range for normalized values. If `NULL`, normalize to z-score. Enter as length 2 numeric vector containing desired minimum and maximum values
#' @returns Data frame with row-wise normalized values
#' @export
normalize_by_row <- function(df, ..., desired_range = NULL) {
  vars <- if (n_dots(...) == 0L) names(df) else names(dplyr::select(df, ...))
  vars <- vars_numeric(df[vars])
  df[vars] <- matrix_to_df(t.default(apply(df[vars], 1, normalize, desired_range = desired_range)))
  df
}

#' Normalize a subset of variables in data frame
#'
#' @param df Data frame
#' @param ... Variables to normalize. Enter using tidyselect syntax
#' @param suffix Suffix for normalized variable names. Default is `"_norm"`
#' @param norm_fn Function used to normalize data. Default is `normalize()`
#' @returns Data frame with additional columns for normalized version of selected variables
#' @export
normalize_cols <- function(df, ..., suffix = "_norm", norm_fn = normalize) {
  vars <- if (n_dots(...) == 0L) vars_numeric(df, incl_integer = FALSE) else names(dplyr::select(df, ...))
  var_names <- paste0(vars, suffix)
  for (i in seq_along(vars)) {
    df[[var_names[i]]] <- norm_fn(df[[vars[i]]])
  }
  df
}

#' Squish a vector into a given range
#'
#' Functionality from `squish` function in scales package
#' @param x Numeric vector
#' @param range Desired range. Enter as length 2 numeric vector. Default is `c(0, 1)`
#' @returns Transformed numeric vector with range defined by range argument
#' @export
squish <- function(x, range = c(0, 1)) {
  # Alternative:
  # x <- pmin(x, range[1L])
  # x <- pmax(x, range[2L])
  force(range)
  idx <- is.finite(x)
  x[idx & x < range[1]] <- range[1]
  x[idx & x > range[2]] <- range[2]
  x
}

#' Box–Cox transformation
#'
#' Functionality from bestNormalize package
#' @param x Numeric vector not containing any 0s
#' @param inverse If `TRUE`, inverse transformation is performed
#' @param lambda Lambda value
#' @param tol Numeric value close to zero used to determine whether lambda > 0
#' @returns Numeric vector with attribute "lambda"
#' @export
transform_boxcox <- function(x, inverse = FALSE, lambda = NULL, tol = 0.001) {
  if (inverse) {
    lambda <- lambda %||% attr(x, "lambda")
    if (lambda < 0) {
      x[x > -1/lambda] <- NA
    }
    x <- if (abs(lambda) < tol) {
      exp(x)
    } else {
      x <- x*lambda + 1
      sign(x)*abs(x)^(1/lambda)
    }
    attributes(x) <- NULL
    x
  } else {
    idx_na <- is.na(x)
    out <- vector("numeric", length = length(x))
    x_nna <- x[!idx_na]
    if (min(x_nna) <= 0) {
      warning("Input to transform_boxcox() should not contain 0's or negative values", call. = FALSE)
    }
    n <- length(x_nna)
    log_x <- suppress(log(x_nna))
    x_mean <- exp(mean(log_x))
    boxcox_loglik <- function(y) {
      m <- x_mean^(y - 1)
      z <- if (abs(y) <= tol) log_x/m else (x_nna^y - 1)/(y*m)
      -0.5*n*log(var(z)*(n - 1)/n)
    }
    lambda <- lambda %||% suppress(optimize(boxcox_loglik, lower = -1, upper = 2, maximum = TRUE, tol = 0.0001)$maximum)
    if (lambda < 0) {
      x_nna[x_nna < 0] <- NA
    }
    x_nna <- if (abs(lambda) < tol) log(x_nna) else (sign(x_nna)*abs(x_nna)^lambda - 1)/lambda
    out[idx_na] <- NA_real_
    out[!idx_na] <- x_nna
    attr(out, "lambda") <- lambda
    out
  }
}

#' Yeo–Johnson transformation
#'
#' Functionality from bestNormalize package
#' @rdname transform_boxcox
#' @export
transform_yj <- function(x, inverse = FALSE, lambda = NULL, tol = 0.001) {
  if (inverse) {
    lambda <- lambda %||% attr(x, "lambda")
    idx_na <- is.na(x)
    out <- vector("numeric", length = length(x))
    x_nna <- x[!idx_na]
    idx_neg <- x_nna < 0
    y <- x_nna
    if (any(!idx_neg)) {
      y[!idx_neg] <- if (abs(lambda) < tol) exp(x_nna[!idx_neg]) - 1 else (x_nna[!idx_neg]*lambda + 1)^(1/lambda) - 1
    }
    if (any(idx_neg)) {
      y[idx_neg] <- if (abs(lambda - 2) < tol) -expm1(-x_nna[idx_neg]) else 1 - (-(2 - lambda)*x_nna[idx_neg] + 1)^(1/(2 - lambda))
    }
    attributes(y) <- NULL
    out[!idx_na] <- y
    out[idx_na] <- NA_real_
    out
  } else {
    idx_na <- is.na(x)
    out <- vector("numeric", length = length(x))
    x_nna <- x[!idx_na]
    n <- length(x_nna)
    pos_idx <- which(x_nna >= 0)
    neg_idx <- which(x_nna < 0)
    constant <- sum(sign(x_nna)*log(abs(x_nna) + 1))
    yj_loglik <- function(y) {
      if (length(pos_idx) > 0) {
        x_nna[pos_idx] <- if (abs(y) < tol) log(x_nna[pos_idx] + 1) else ((x_nna[pos_idx] + 1)^y - 1)/y
      }
      if (length(neg_idx) > 0) {
        x_nna[neg_idx] <- if (abs(y - 2) < tol) -log(-x_nna[neg_idx] + 1) else -((-x_nna[neg_idx] + 1)^(2 - y) - 1)/(2 - y)
      }
      x_t_var <- var(x_nna)*(n - 1)/n
      -0.5*n*log(x_t_var) + (y - 1)*constant
    }
    lambda <- lambda %||% suppressWarnings(optimize(yj_loglik, lower = -5, upper = 5, maximum = TRUE, tol = 0.0001)$maximum)
    if (length(pos_idx) > 0) {
      x_nna[pos_idx] <- if (abs(lambda) < tol) log(x_nna[pos_idx] + 1) else ((x_nna[pos_idx] + 1)^lambda - 1)/lambda
    }
    if (length(neg_idx) > 0) {
      x_nna[neg_idx] <- if (abs(lambda - 2) < tol) -log(-x_nna[neg_idx] + 1) else -((-x_nna[neg_idx] + 1)^(2 - lambda) - 1)/(2 - lambda)
    }
    out[!idx_na] <- x_nna
    out[idx_na] <- NA_real_
    attr(out, "lambda") <- lambda
    out
  }
}

#' Hyperbolic arc-sine transformation
#'
#' @inheritParams transform_boxcox
#' @param constant Constant factor for transformation. Default is `5`
#' @export
transform_asinh <- function(x, inverse = FALSE, constant = 5) {
  if (inverse) {
    constant*sinh(x)
  } else {
    asinh(x/constant)
  }
}

#' Logit transformation
#'
#' @rdname transform_asinh
#' @export
transform_logit <- function(x, inverse = FALSE) {
  if (inverse) {
    x <- exp(x)
    x/(1 + x)
  } else {
    log(x/(1 - x))
  }
}
