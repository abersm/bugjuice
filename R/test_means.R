# t-test ------------------------------------------------------------------

#' P value for t-test
#'
#' @param df Data frame
#' @param formula Entered in format of continuous variable ~ grouping variable
#' @param y Continuous variable. Enter as quoted variable name
#' @param x Categorical grouping variable. Enter as quoted variable name
#' @param id Identification variable to link paired observations. Enter as quoted variable name. Only relevant when paired is `TRUE`
#' @param welch If `NULL` (default), use of Welch's correction is determined by variance test (If P < 0.05, variance is not equal between groups and Welch's correction is applied). If `TRUE`, Welch's correction is applied, otherwise student's t-test is performed
#' @param variance_test Function used to perform variance test. Must take numeric vector and returns P value. Default is `p_F_test`
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `two.sided` (default), `less`, `greater.` Enter as quoted hypothesis test type
#' @param otherwise Output if t-test fails. Default is `NA`
#' @param ... Not used
#' @export
p_ttest <- function(df, formula = NULL, y = NULL, x = NULL, id = NULL, welch = NULL, variance_test = p_F_test, paired = FALSE, hypothesis_type = "two.sided", otherwise = NA_real_, ...) {
  vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_ttest")
  y <- vars$y
  x <- vars$x
  df <- droplevels(df)
  values <- if (paired) {
    .paired_values(df = df, y = y, x = x, id = id)
  } else {
    df <- df[, c(y, x), drop = FALSE]
    df <- df[complete.cases(df), ]
    split(df[[y]], df[[x]])
  }
  if (length(values) != 2) return(otherwise)
  tryCatch(suppressWarnings(.p_ttest(x = values[[1]], y = values[[2]], welch = welch, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise)), error = function(e) otherwise)
}

#' P values for t-test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param welch If `NULL` (default), variance_test is used to determine whether to apply Welch's correction. If TRUE, Welch's correction is applied (assumes variance is not equal between groups), otherwise student's t-test performed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param variance_test Function used to perform variance test. Must take 2 numeric vectors and return P value. Default is .p_F_test
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param otherwise Output if t-test fails. Default is `NA`
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @noRd
.p_ttest <- function(x, y, welch = NULL, paired = FALSE, variance_test = .p_F_test, hypothesis_type = "two.sided", otherwise = NA_real_, ...) {
  if (paired) {
    x <- x - y
  }
  x_len <- length(x)
  if (x_len < 2 || !is.numeric(x) || !is.vector(x)) return(otherwise)
  x_mean <- mean.default(x)
  x_var <- Var(x)
  if (paired) {
    deg_free <- x_len - 1
    stderr <- sqrt(x_var/x_len)
    if (stderr < 10*.Machine$double.eps*abs(x_mean)) return(otherwise)
    tstat <- x_mean/stderr
  } else {
    y_len <- length(y)
    if (y_len < 2 || !is.numeric(y) || !is.vector(y)) return(otherwise)
    y_mean <- mean.default(y)
    y_var <- Var(y)
    welch <- welch %||% (variance_test(x = x, y = y, hypothesis_type = hypothesis_type, otherwise = 0) < 0.05)
    if (welch) {
      stderrx <- sqrt(x_var/x_len)
      stderry <- sqrt(y_var/y_len)
      stderr <- sqrt(stderrx^2 + stderry^2)
      deg_free <- stderr^4/(stderrx^4/(x_len - 1) + stderry^4/(y_len - 1))
    } else {
      deg_free <- x_len + y_len - 2
      v <- (x_len - 1)*x_var
      v <- v + (y_len - 1)*y_var
      v <- v/deg_free
      stderr <- sqrt(v*(1/x_len + 1/y_len))
    }
    if (stderr < 10*.Machine$double.eps*max(abs(x_mean), abs(y_mean))) return(otherwise)
    tstat <- (x_mean - y_mean)/stderr
  }
  switch(hypothesis_type,
         two.sided = 2*pt(-abs(tstat), deg_free),
         less = pt(tstat, deg_free),
         greater = pt(tstat, deg_free, lower.tail = FALSE))
}

# Mann-Whitney test -------------------------------------------------------

#' Mann-Whitney U test (Wilcoxon rank sum test)
#'
#' @rdname p_ttest
#' @export
p_mann_whitney <- function(df, formula = NULL, y = NULL,  x = NULL, id = NULL, paired = FALSE, otherwise = NA_real_, hypothesis_type = "two.sided", ...) {
  df <- droplevels(df)
  vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_mann_whitney")
  y <- vars$y
  x <- vars$x
  df <- droplevels(df)
  values <- if (paired) {
    .paired_values(df = df, y = y, x = x, id = id)
  } else {
    df <- df[, c(y, x), drop = FALSE]
    df <- df[complete.cases(df), ]
    split(df[[y]], df[[x]])
  }
  if (length(values) != 2) return(otherwise)
  tryCatch(.p_mann_whitney(x = values[[1]], y = values[[2]], paired = paired, hypothesis_type = hypothesis_type), error = function(e) otherwise)
}

#' Alias for p_mann_whitney
#'
#' @rdname p_ttest
#' @export
p_wilcox <- p_mann_whitney

#' P values for Mann-Whitney test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param otherwise Output if Mann-Whitney U test fails. Default is `NA`
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param correct_p If `TRUE` (default), continuity correction is applied to P value
#' @param ... Unused
#' @returns P value as length 1 numeric vector
#' @noRd
.p_mann_whitney <- function(x, y, paired = FALSE, otherwise = NA_real_, hypothesis_type = c("two.sided", "less", "greater"), correct_p = TRUE, ...) {
  hypothesis_type <- match.arg(hypothesis_type)
  correction <- 0
  if (paired) {
    x <- x - y
    if (zeros <- any(x == 0)) {
      x <- x[x != 0]
    }
    n <- length(x)
    exact <- n < 50
    r <- rank(abs(x))
    statistic <- sum(r[x > 0])
    ties <- length(r) != n_unique(r, na.rm = FALSE)
    if (exact && !ties && !zeros) {
      switch(hypothesis_type, two.sided = {
        p <- if (statistic > (n*(n + 1)/4)) psignrank(statistic - 1, n, lower.tail = FALSE) else psignrank(statistic, n)
        min(2*p, 1)
      }, greater = psignrank(statistic - 1, n, lower.tail = FALSE), less = psignrank(statistic, n))
    } else {
      n_ties <- table(r)
      z <- statistic - n*(n + 1)/4
      sig <- sqrt(n*(n + 1)*(2*n + 1)/24 - sum(n_ties^3 - n_ties)/48)
      if (correct_p) {
        correction <- switch(hypothesis_type, two.sided = sign(z)*0.5, greater = 0.5, less = -0.5)
      }
      z <- (z - correction)/sig
      switch(hypothesis_type, less = pnorm(z), greater = pnorm(z, lower.tail = FALSE), two.sided = 2*min(pnorm(z), pnorm(z, lower.tail = FALSE)))
    }
  } else {
    r <- rank(c(x, y))
    x_length <- length(x)
    y_length <- length(y)
    exact <- x_length < 50 && y_length < 50
    statistic <- sum(r[seq_len(x_length)]) - x_length*(x_length + 1)/2
    ties <- length(r) != n_unique(r, na.rm = FALSE)
    if (exact && !ties) {
      switch(hypothesis_type, two.sided = {
        p <- if (statistic > x_length*y_length/2) pwilcox(statistic - 1, x_length, y_length, lower.tail = FALSE) else pwilcox(statistic, x_length, y_length)
        min(2*p, 1)
      }, greater = pwilcox(statistic - 1, x_length, y_length, lower.tail = FALSE), less = pwilcox(statistic, x_length, y_length))
    } else {
      n_ties <- table(r)
      z <- statistic - x_length*y_length/2
      sig <- sqrt(x_length*y_length/12*((x_length + y_length + 1) - sum(n_ties^3 - n_ties)/((x_length + y_length)*(x_length + y_length - 1))))
      if (correct_p) {
        correction <- switch(hypothesis_type, two.sided = sign(z)*0.5, greater = 0.5, less = -0.5)
      }
      z <- (z - correction)/sig
      switch(hypothesis_type, less = pnorm(z), greater = pnorm(z, lower.tail = FALSE), two.sided = 2*min(pnorm(z), pnorm(z, lower.tail = FALSE)))
    }
  }
}

# P by normality ----------------------------------------------------------

#' P value for comparison using test as determined by application of normality and variance tests
#'
#' @rdname p_ttest
#' @param normality_test Function used to determine normality. Must take numeric vector and returns P value. Default is `p_shapiro`
#' @export
p_by_normality <- function(df, formula = NULL, y = NULL, x = NULL, id = NULL, paired = FALSE, welch = NULL, normality_test = p_shapiro, variance_test = p_F_test, hypothesis_type = "two.sided", otherwise = NA_real_, ...) {
  df <- droplevels(df)
  if (is.null(formula)) {
    formula <- create_formula(y, x)
  }
  normality <- is_normal(df = df, formula = formula, normality_test = normality_test)
  if (normality) {
    p_ttest(df = df, formula = formula, id = id, welch = welch, paired = paired, otherwise = otherwise, ...)
    } else {
      p_mann_whitney(df = df, formula = formula, id = id, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise, ...)
  }
}

#' P values as determined by normality test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param x_normality,y_normality P values of normality tests for x and y respectively
#' @param normality_test Function used to determine normality. Must take numeric vector and returns P value. Default is `p_shapiro`
#' @param variance_test Function used to perform variance test. Must take 2 numeric vectors and return P value. Default is `.p_F_test`
#' @param welch If `TRUE` (default), Welch's correction is applied (assumes variance is not equal between groups), otherwise student's t-test performed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `two.sided` (default), `less`, `greater.` Enter as quoted hypothesis test type
#' @param otherwise Output if test fails. Default is `NA`
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @noRd
.p_by_normality <- function(x, y, x_normality = NULL, y_normality = NULL, normality_test = p_shapiro, variance_test = .p_F_test, welch = NULL, paired = FALSE, hypothesis_type = "two.sided", otherwise = NA_real_, ...) {
  if (length(x) < 2 || length(y) < 2) {
    otherwise
  } else if (normality_test(x) < 0.05 || normality_test(y) < 0.05) {
    .p_mann_whitney(x = x, y = y, paired = paired, hypothesis_type = hypothesis_type, otherwise = otherwise)
  } else {
    .p_ttest(x = x, y = y, welch = welch, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise)
  }
}

# ANOVA -------------------------------------------------------------------

#' One-way ANOVA with or without Welch's correction
#'
#' Functionality from oneway.test function in stats package
#' @param df Data frame
#' @param formula Entered as continuous variable ~ grouping variable
#' @param x Continuous variable. Enter as quoted variable name
#' @param y Continuous variable. Enter as quoted variable name
#' @param na.rm If `TRUE` (default), missing values for grouping variable are not considered a distinct group
#' @param welch If `NULL` (default), variance testing is used to determine whether Welch's correction should be applied. If `TRUE`, Welch's correction performed. If `FALSE`, ordinary ANOVA performed
#' @param variance_test Test used to determine whether variance differs by group. Default is Levene test
#' @param otherwise Output if ANOVA fails. Default is `NA`
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @export
p_anova <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, welch = NULL, variance_test = p_levene, otherwise = NA_real_, ...) {
  if (is.null(formula)) {
    formula <- create_formula(y, x)
  } else {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- droplevels(df[, c(y, x)])
  df <- if (na.rm) df[complete.cases(df), ] else df[complete.cases(df[, y]), ]
  g <- df[[x]] <- factor(df[[x]])
  variance_test <- match_fun(variance_test)
  welch <- welch %||% (variance_test(df = df, formula = formula, otherwise = 0) < 0.05)
  y <- df[[y]]
  n_groups <- length(attr(g, "levels"))
  if (n_groups < 2) return(otherwise)
  y_length <- tapply(y, g, length)
  if (any(y_length < 2)) return(otherwise)
  y_mean <- tapply(y, g, mean)
  y_variance <- tapply(y, g, Var)
  if (welch) {
    z <- y_length/y_variance
    z_sum <- sum(z)
    j <- sum((1 - z/z_sum)^2/(y_length - 1))/(n_groups^2 - 1)
    m <- sum(z*y_mean)/z_sum
    statistic <- sum(z*(y_mean - m)^2)/((n_groups - 1)*(1 + 2*(n_groups - 2)*j))
    tryCatch(pf(statistic, n_groups - 1, 1/(3*j), lower.tail = FALSE), error = function(e) otherwise)
  } else {
    n <- sum(y_length)
    statistic <- (sum(y_length*(y_mean - mean.default(y))^2)/(n_groups - 1))/(sum((y_length - 1)*y_variance)/(n - n_groups))
    tryCatch(pf(statistic, n_groups - 1, n - n_groups, lower.tail = FALSE), error = function(e) otherwise)
  }
}

# Kruskal-Wallis ----------------------------------------------------------

#' Kruskal-Wallis test
#'
#' @rdname p_anova
#' @export
p_kruskal <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, otherwise = NA_real_, ...) {
  if (is.null(formula)) {
    formula <- create_formula(y, x)
  } else {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- droplevels(df[, c(y, x)])
  df <- if (na.rm) df[complete.cases(df), ] else df[complete.cases(df[, y]), ]
  g <- df[[x]] <- factor(df[[x]])
  y <- df[[y]]
  n_groups <- length(attr(g, "levels"))
  if (n_groups < 2) return(otherwise)
  n <- length(y)
  if (n < 2) return(otherwise)
  r <- rank(y)
  z <- table(y)
  statistic <- sum(tapply(r, g, sum)^2/tapply(r, g, length))
  statistic <- (12*statistic/(n*(n + 1)) - 3*(n + 1))/(1 - sum(z^3 - z)/(n^3 - n))
  tryCatch(pchisq(statistic, n_groups - 1L, lower.tail = FALSE), error = function(e) otherwise)
}

# Dunn test ---------------------------------------------------------------

#' Dunn test
#'
#' @inheritParams compare_means
#' @param p_adj_method Method for p value adjustment. Options: `BH` (default), `Holm`, `hochberg`, `hommell`, `bonferroni`, `BY`, `fdr`, `none.` Enter as quoted method
#' @return Data frame with comparisons similar to compare_means output
#' @export
p_dunn <- function(
    df,
    formula,
    ...,
    x = NULL, y = NULL,
    p_adj_method = "BH",
    ns_symbol = "ns",
    summary_fns = list(
      median = Median,
      mean = Mean,
      q1 = Q1, q3 = Q3,
      sd = SD, se = SE,
      ci = CI,
      min = min, max = max
    )) {
  vars <- formula2vars(formula, x = get_input(x), y = get_input(y), parent_fn = "p_dunn")
  grouping_var <- vars$x
  continuous_var <- vars$y

  # Create character vector containing names of subgrouping variables
  if (n_dots(...) > 0) {
    is_input_vector <- tryCatch(is.atomic(c(...)), error = function(e) FALSE)
    subgroups <- Intersect(if (is_input_vector) c(...) else dots_as_quoted(...), names(df))
  } else {
    subgroups <- character(0)
  }
  if (length(subgroups) == 0) {
    subgroups <- NULL
  }

  # Remove missing values
  df <- df[c(continuous_var, grouping_var, subgroups)]
  df <- df[complete.cases(df), ]

  # Remove unused levels of factors
  df <- droplevels(df)

  # Nested statistical analysis
  suppress({
    pval_table <- dplyr::group_by(df, !!!dplyr::syms(subgroups)) |> tidyr::nest() |> dplyr::mutate(p_val_table = purrr::map(data, ~.p_dunn_helper(.df = .x, .continuous_var = continuous_var, .group = grouping_var, .p_adj_method = p_adj_method, .summary_fns = summary_fns))) |> dplyr::select(-data) |> tidyr::unnest(p_val_table) |> dplyr::ungroup()
  })

  # Add column for comparisons and significance labels
  pval_table <- dplyr::mutate(pval_table,
                              grouping_var = grouping_var,
                              p_adj_label = sig_stars(p, symbols = c("****", "***", "**", "*",  ns_symbol)),
                              greater = dplyr::case_when(mean_1 > mean_2 ~ paste0(Group1, " > ", Group2), mean_2 > mean_1 ~ paste0(Group2, " > ", Group1), mean_1 == mean_2 ~ paste0(Group1, " = ", Group2)))
  pval_table <- dplyr::select(pval_table, dplyr::any_of(subgroups), Group1, Group2, greater, p_adj, p_adj_label, p, n_1, n_2, median_1, median_2, mean_1, mean_2, q1_1, q1_2, q3_1, q3_2, sd_1, sd_2, se_1, se_2, min_1, min_2, max_1, max_2, ci_1, ci_2, dplyr::everything())
  pval_table[order(pval_table$p_adj), ]
}

#' Helper function to perform Dunn's test for each subgrouping variable
#'
#' @inheritParams .compare_means_pairs
#' @param .p_adj_method Method for P value adjustment
#' @param .summary_fns Named list of summary functions. Enter as list(col_name = fn_name)
#' @noRd
.p_dunn_helper <- function(.df, .continuous_var, .group, .p_adj_method = "BH", .summary_fns = list(mean = Mean, median = Median, sd = SD, se = SE, q1 = Q1, q3 = Q3, ci = CI, min = Min, max = Max)) {
  y <- .df[[.continuous_var]]
  g <- factor(.df[[.group]])
  group_sizes <- summary(g)
  if (length(group_sizes) < 2) return(NULL)
  unique_groups <- names(group_sizes)
  df_summary <- dplyr::summarize(dplyr::group_by(.df, as.character(.data[[.group]])), dplyr::across(.cols = dplyr::all_of(.continuous_var), .fns = .summary_fns, .names = "{.fn}"))
  y_rank <- rank(y)
  mean_ranks <- tapply(y_rank, g, Mean)
  n <- length(y)
  y_rank_sorted <- sort.int(y_rank)
  pos <- 1
  tiesum <- 0
  while (pos <= n) {
    val <- y_rank_sorted[pos]
    nt <- length(y_rank_sorted[y_rank_sorted == val])
    pos <- pos + nt
    if (nt > 1) {
      tiesum <- tiesum + nt^3 - nt
    }
  }
  C <- tiesum/(12*(n - 1))
  compare_levels <- function(i, j) {
    D <- abs(mean_ranks[i] - mean_ranks[j])
    A <- n*(n + 1)/12
    B <- 1/group_sizes[i] + 1/group_sizes[j]
    z <- D/sqrt((A - C)*B)
    2*pnorm(abs(z), lower.tail = FALSE)
  }
  df_pval <- pmap_dfr(.combos_1_vec_as_df(unique_groups, n = 2), ~{
    list(Group1 = .x, Group2 = .y, p = compare_levels(.x, .y), n_1 = group_sizes[.x], n_2 = group_sizes[.y])
  })
  df_pval$p_adj <- p_adjust.default(df_pval$p, method = .p_adj_method)

  # Merge df_summary and df_pval
  names(df_summary)[1] <- "Group1"
  df_pval <- dplyr::left_join(df_pval, df_summary, by = "Group1")
  df_pval <- dplyr::left_join(df_pval, df_summary, by = c("Group2" = "Group1"), suffix = c("_1", "_2"))
  df_pval
}

# Tukey test --------------------------------------------------------------

#' aov
#'
#' @inheritParams Lm
#' @export
Aov <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE) {
  structure(Lm(df, formula = formula, y = y, x = x, na.rm = na.rm), class = c("aov", "lm"), projections = NULL)
}

#' Tukey's HSD (honestly significant difference)
#'
#' @rdname p_anova
#' @returns Data frame with comparisons similar to compare_means and p_dunn output
#' @export
p_tukey <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, variance_test = p_F_test, ...) {
  # Data and variables
  if (!is.null(formula)) {
    continuous_var <- all.vars(formula)
    grouping_var <- continuous_var[2L]
    continuous_var <- continuous_var[1L]
  } else {
    continuous_var <- y
    grouping_var <- x
    formula <- create_formula(y, x)
  }

  df <- droplevels(df[, c(continuous_var, grouping_var)])
  df <- if (na.rm) df[complete.cases(df), ] else df[complete.cases(df[, continuous_var]), ]
  y <- df[[continuous_var]]
  g <- as.character(df[[grouping_var]])
  g <- gsub(pattern = "-", replacement = "___", x = g)
  group_size <- summary(g)
  group_name_na <- names(group_size) == "NA's"
  if (any(group_name_na)) {
    names(group_size)[group_name_na] <- "missing"
    df[is.na(df[[grouping_var]]), grouping_var] <- "missing"
  }

  # Summary statistics for each level of grouping variable
  df_summary <- dplyr::group_by(df, as.character(.data[[grouping_var]]))
  df_summary <- dplyr::summarize(df_summary, dplyr::across(.cols = continuous_var, .fns = list(n = N, mean = Mean, median = Median, sd = SD, se = SE, q1 = Q1, q3 = Q3, ci = CI, min = Min, max = Max), .names ="{.fn}"))

  # Check for > 1 group
  if (length(group_size) < 2) return(NULL)

  pval_kruskal <- p_kruskal(df, formula = formula)
  pval_var <- variance_test(df, formula = formula)
  pval_anova_welch <- p_anova(df, formula = formula, welch = TRUE)
  anova_model <- tryCatch(suppressWarnings(Aov(df = df, formula = formula)), error = function(e) NULL)
  pval_anova <- if (is.null(anova_model)) NA_real_ else summary(anova_model)[[1L]]$`Pr(>F)`[1L]

  df_tukey <- data.frame(TukeyHSD(anova_model)[[1]])
  comps <- rownames(df_tukey)
  rownames(df_tukey) <- NULL
  names(df_tukey) <- c("tukey_diff", "tukey_lower", "tukey_upper", "p_adj")
  df_tukey$p_adj_label <- sig_stars(df_tukey$p_adj)
  comps <- strsplit(comps, split = "-")
  comps <- do.call("rbind", comps)
  df_tukey <- cbind(comps, df_tukey)
  names(df_tukey)[1:2] <- c("Group1", "Group2")
  df_tukey$grouping_var <- grouping_var
  df_tukey <- df_tukey[, c("grouping_var", "Group1", "Group2", "p_adj", "p_adj_label", "tukey_diff", "tukey_lower", "tukey_upper")]
  df_tukey$p_kruskal <- pval_kruskal
  df_tukey$p_anova <- pval_anova
  df_tukey$p_anova_welch <- pval_anova_welch
  df_tukey$p_var_test <- pval_var
  if (identical(variance_test, p_levene)) {
    df_tukey$variance_test <- "Levene"
  } else if (identical(variance_test, p_bartlett)) {
    df_tukey$variance_test <- "Bartlett"
  } else {
    df_tukey$variance_test <- "F-test"
  }
  # Add columns for summary statistics
  names(df_summary)[1L] <- "Group1"
  df_tukey <- dplyr::left_join(df_tukey, df_summary, by = "Group1")
  df_tukey <- dplyr::left_join(df_tukey, df_summary, by = c("Group2" = "Group1"), suffix = c("_1", "_2"))
  df_tukey$Group1 <- gsub(pattern = "___", replacement = "-", x = df_tukey$Group1)
  df_tukey$Group2 <- gsub(pattern = "___", replacement = "-", x = df_tukey$Group2)
  df_tukey
}

# Helpers -----------------------------------------------------------------

#' Create paired values for hypothesis testing
#'
#' @param df Data frame entered
#' @param y Continuous variable. Enter as quoted variable name
#' @param x Categorical variable (should have 2 levels). Enter as quoted variable name
#' @param id Identification variable. Enter as quoted variable name
#' @returns List of length 2 containing pre and post values
#' @noRd
.paired_values <- function(df, y = NULL, x = NULL, id = NULL) {
  df <- df[c(y, x, id)]
  null_id <- is.null(id)
  null_x <- is.null(x)
  if (!null_id && !null_x) {
    names(df) <- c("y", "x", "id")
  } else if (null_id && !null_x) {
    names(df) <- c("y", "x")
    df <- df[order(df$x), ]
    x_unique <- unique.default(df$x)
    x_unique <- x_unique[!is.na(x_unique)]
    if (length(x_unique) != 2) return(NULL)
    y1 <- df$y[df$x == x_unique[1]]
    y2 <- df$y[df$x == x_unique[2]]
    n_1 <- length(y1)
    n_2 <- length(y2)
    if (n_1 != n_2) return(NULL)
    df$id <- rep(seq_len(n_1), 2)
    message("Paired t-test requested but no id variable entered. Will assume order of individuals (rows) in group 1 matches that of group 2")
  } else if (null_x && !null_id) {
    names(df) <- c("y", "id")
    df <- df[order(df$id), ]
    n <- length(df$y)
    if (!is_even(n)) return(NULL)
    df$x <- rep(seq_len(2), n)
    message("Paired t-test requested but 'x' (grouping variable) not provided. Unclear which values should are pre vs. post. Will assume order of values for each individual is pre before post")
  } else {
    return(NULL)
  }
  df <- tidyr::pivot_wider(id_cols = "id", names_from = "x", values_from = "y")
  df <- df[complete.cases(df), ]
  list(y1 = df[[2]], y2 = df[[3]])
}
