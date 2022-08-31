# Significance stars ------------------------------------------------------

#' Convert p values into significance stars
#'
#' @param p Numeric vector of p values
#' @param cutpoints Cutpoints for significance stars. Enter as numeric vector including 0 and 1
#' @param symbols Symbols representing significance levels from lowest to highest P value. Enter as character vector
#' @returns Character vector containing significance stars
#' @export
sig_stars <- function(p, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*",  "ns")) {
  z <- findInterval(as.numeric(p), vec = cutpoints, all.inside = TRUE, left.open = TRUE, rightmost.closed = TRUE)
  symbols[z]
}

# Adjusted p values -------------------------------------------------------

#' Generic adjust p value function
#'
#' Simplified version of `stats::p.adjust` for broader range of input types
#' @param x Data frame or numeric vector containing p values
#' @param method Method for p value correction. Options: `"holm"` (default), `"BH"`, `"BY"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"fdr"` (same as `"BH")`, `"none"`. Enter as quoted method. Can be any case
#' @param p_column Column containing p values. Enter as quoted column name
#' @param p_adj_name Column name for adjusted p values. Default is `"p_adj"`
#' @param ... Arguments passed to class-specific `p_adjust` function
#' @export
p_adjust <- function(x, p_column = "p", p_adj_name = "p_adj", method = "holm", ...) {
  UseMethod("p_adjust", x)
}

#' Adjust a numeric vector of p values
#'
#' @rdname p_adjust
#' @export
p_adjust.default <- function(x, p_column = "p", p_adj_name = "p_adj", method = "holm", ...) {
  method <- casefold(method, upper = nchar(method) == 2L)
  if (method == "fdr") {
    method <- "BH"
  }
  idx_nna <- !is.na(x)
  p <- x[idx_nna]
  n <- length(p)
  if (n <= 1) return(x)
  if (n == 2 && method == "hommel") {
    method <- "hochberg"
  }
  out <- rep_len(NA_real_, length(x))
  out[idx_nna] <- switch(method,
                         bonferroni = pmin(1, n*p),
                         holm = {
                           i <- seq_len(n)
                           z <- order(p)
                           pmin(1, cummax((n + 1L - i)*p[z]))[order(z)]
                         },
                         hommel = {
                           i <- seq_len(n)
                           z <- order(p)
                           p <- p[z]
                           q <- pa <- rep.int(min(n*p/i), n)
                           for (j in (n - 1L):2L) {
                             ij <- seq_len(n - j + 1L)
                             i2 <- (n - j + 2L):n
                             q1 <- min(j*p[i2]/(2L:j))
                             q[ij] <- pmin(j*p[ij], q1)
                             q[i2] <- q[n - j + 1L]
                             pa <- pmax(pa, q)
                           }
                           pmax(pa, p)[order(z)]
                         },
                         hochberg = {
                           i <- n:1L
                           z <- order(p, decreasing = TRUE)
                           pmin(1, cummin((n + 1L - i)*p[z] ))[order(z)]
                         },
                         BH = {
                           i <- n:1L
                           z <- order(p, decreasing = TRUE)
                           pmin(1, cummin(n/i*p[z]))[order(z)]
                         },
                         BY = {
                           i <- n:1L
                           z <- order(p, decreasing = TRUE)
                           q <- sum(1/(1L:n))
                           pmin(1, cummin(q*n/i*p[z]))[order(z)]
                         },
                         none = p)
  out
}

#' Add column of adjusted p values
#'
#' @rdname p_adjust
#' @export
p_adjust.data.frame <- function(x, p_column = "p", p_adj_name = "p_adj", method = "holm", ...) {
  x[[p_adj_name]] <- p_adjust(x[[p_column]], method = method)
  dplyr::relocate(x, dplyr::all_of(p_adj_name), .after = dplyr::all_of(p_column))
}

# q values ----------------------------------------------------------------

#' Generic q_value function
#'
#' @param x Numeric vector of p values or data frame
#' @param x Data frame or numeric vector
#' @param p_column Column containing p values. Enter as quoted column name. Default is `p`
#' @param q_col_name Column name for q values. Enter as quoted column name. Default is `q_val`
#' @param ... Arguments passed to class-specific `q_value` function
#' @returns Numeric vector of q values
#' @export
q_value <- function(x, p_column = "p", q_col_name = "q_val", ...) {
  UseMethod("q_value", x)
}

#' q values - default method
#'
#' @rdname q_value
#' @export
q_value.default <- function(x, p_column = "p", q_col_name = "q_val", ...) {
  # p_input <- x
  qvals <- x
  idx_nna <- !is.na(x)
  x <- x[idx_nna]
  n <- length(x)
  lambda <- seq(0.05, 0.95, 0.05)
  i <- 19L:1L
  pi0 <- cumsum(tabulate(findInterval(x, vec = lambda))[i])/(n*(1 - lambda[i]))
  pi0 <- pi0[i]
  pi0 <- min(predict(smooth.spline(lambda, pi0, df = 3), x = lambda)$y[19L], 1)
  pi0 <- if (pi0 <= 0) 1 else pi0
  i <- n:1L
  o <- order(x, decreasing = TRUE)
  qvals[idx_nna] <- pi0*pmin(1, cummin(x[o]*n/i))[order(o)]
  qvals
  # lfdr_out <- x
  # x <- qnorm(pmin(pmax(x, 1e-08), 1 - 1e-08))
  # myd <- density(x, adjust = 1.5)
  # y <- predict(smooth.spline(x = myd$x, y = myd$y), x)$y
  # lfdr <- pi0*dnorm(x)/y
  # lfdr[lfdr > 1] <- 1
  # o <- order(x, decreasing = FALSE)
  # lfdr_out[idx_nna] <- cummax(lfdr[o])[order(o)]
  # list(qvalues = qvals, pvalues = p_input, lfdr = lfdr_out)
}

#' q values - data frame
#'
#' @rdname q_value
#' @export
q_value.data.frame <- function(x, p_column = "p", q_col_name = "q_val", ...) {
  df[[q_col_name]] <- q_value(df[[p_column]])
  df
}

# Formatting --------------------------------------------------------------

#' Format p values
#'
#' @param p Numeric vector containing p values
#' @param p_prefix Prefix used for P value. Default is `"P"`
#' @param show_leading_0 If `TRUE` (default), 0s before decimal point are included in output
#' @param trim_ws If `FALSE` (default), white space is allowed in output. If `TRUE`, white space is removed from output
#' @param min_digits Number of digits included after decimal for P values > 0.06
#' @returns p value formatted as character
#' @export
format_p_value <- function(p, p_prefix = "P", trim_ws = FALSE, show_leading_0 = TRUE, min_digits = 2) {
  label <- as.character(findInterval(as.numeric(p), vec = c(0, 0.001, 0.01, 0.045, 0.06, 1), all.inside = TRUE, left.open = TRUE, rightmost.closed = TRUE))
  label[label == "1"] <- paste(p_prefix, "< 0.001")
  label[label == "2"] <- paste(p_prefix, "=", sprintf("%.3f", p[label == "2"]))
  label[label == "3"] <- paste(p_prefix, "=", sprintf("%.2f", p[label == "3"]))
  label[label == "4"] <- paste(p_prefix, "=", sprintf("%.3f", p[label == "4"]))
  label[label == "5"] <- paste(p_prefix, "=", sprintf(paste0("%.", min_digits, "f"), pmin(p[label == "5"], 0.99)))
  if (is.null(p_prefix) || !nzchar(p_prefix)) {
    label[grepl(pattern = "= ", x = label, fixed = TRUE)] <- gsub(pattern = "= ", replacement = "", x = label)
  }
  if (trim_ws) {
    label <- gsub(pattern = " ", replacement = "", x = label, fixed = TRUE)
  }
  if (!show_leading_0) {
    label <- gsub(pattern = "0\\.", replacement = "\\.", x = label)
  }
  label
}
