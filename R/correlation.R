#' Pairwise combination of p-values and correlation coefficients
#'
#' @param df Data frame in wide format or grouped data frame
#' @param ... Variables in `df` to include in correlation analysis. Enter using tidyselect syntax
#' @param method Method of comparison. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence level. Default is `0.95`. Enter as numeric 0-1
#' @returns Data frame with columns col_1, col_2 (variables compared), cor, cor_lower, cor_upper, p, label, method, n, n_nna, perc_na
#' @export
cor_pairs <- function(df, ..., method = c("spearman", "pearson", "kendall"), ci = 0.95) {
  method <- match.arg(arg = method, choices = c("spearman", "pearson", "kendall"))
  df <- Select(df, ...)
  if (inherits(df, "grouped_df")) {
    df <- tidyr::nest(df)
    df <- dplyr::mutate(df, cor = purrr::map(data, .cor_pairs, method = method, ci = ci))
    df <- df[names(df) != "data"]
    df <- tidyr::unnest(df, cor)
    df <- dplyr::ungroup(df)
  } else {
    df <- .cor_pairs(df, method = method, ci = ci)
  }
  df$label <- sig_stars(df$p)
  df$perc_na <- 100 - 100*df$n_nna/df$n
  core_cols <- c("col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "label", "method", "n", "n_nna", "perc_na")
  other_cols <- Setdiff(names(df), core_cols)
  df[c(other_cols, core_cols)]
}

#' Helper function to determine pairwise correlation coefficients and P value
#'
#' @param df Data frame
#' @param method Method of comparison. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Default is `0.95`
#' @noRd
.cor_pairs <- function(df, method, ci = 0.95) {
  df <- df[vars_numeric(df)]
  df_nna <- crossprod(!is.na(df))
  df_nna[upper.tri(df_nna, diag = TRUE)] <- Inf
  df_nna <- matrix_to_df(df_nna, rownames_to_col = TRUE, colname_rownames = "col_1")
  df_nna <- tidyr::pivot_longer(df_nna, cols = -col_1, names_to = "col_2", values_to = "nna")
  df_nna <- df_nna[is.finite(df_nna$nna), , drop = FALSE]
  z <- suppress(cor(x = df, y = NULL, use = "pairwise.complete.obs", method = method))
  z[upper.tri(z, diag = TRUE)] <- Inf

  # Don't use rownames_to_col because z is a matrix
  z <- matrix_to_df(z, rownames_to_col = TRUE, colname_rownames = "col_1")
  z <- tidyr::pivot_longer(z, cols = -col_1, names_to = "col_2", values_to = "cor")
  z <- z[is.finite(z$cor) | is.na(z$cor), , drop = FALSE]
  z$method <- method
  z$n_nna <- df_nna$nna
  z$se <- suppress(1/sqrt(z$n_nna - 3))
  z$n <- .row_names_info(df, type = 2L)
  z$p <- 2*pnorm(-abs(z$cor/z$se))
  l <- atanh(z$cor)
  k <- qnorm(0.5 + ci/2)*z$se
  z$cor_lower <- tanh(l - k)
  z$cor_upper <- tanh(l + k)
  z[order(z$p), c("col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "method", "n", "n_nna"), drop = FALSE]
}
