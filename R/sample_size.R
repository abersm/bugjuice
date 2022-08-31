#' Sample size calculation, continuous outcome
#'
#' @param mean1,mean2 Mean value for groups 1 and 2, respectively
#' @param sd1,sd2 sd for groups 1 and 2, respectively
#' @param beta power = 1 - beta. Default is `0.2`
#' @returns Number of subjects required to reach signifance
#' @export
sample_size_continuous <- function(mean1, sd1, mean2, sd2, beta = 0.2) {
  # Assumption: alpha = 0.05
  z <- dplyr::case_when(
    beta == 0.05 ~ 13,
    beta == 0.1 ~ 10.5,
    beta == 0.2 ~ 7.8,
    beta == 0.5 ~ 3.8)
  m <- mean1 - mean2
  ceiling(z*(sd1*sd1 + sd2*sd2)/(m*m))
}

#' Sample size calculation, binary outcome
#'
#' @param p1,p2 Proportion with outcome for groups 1 and 2, respectively
#' @param beta power = 1 - beta. Default is `0.2`
#' @returns Number of subjects required to reach signifance
#' @export
sample_size_binary <- function(p1, p2, beta = 0.2) {
  # Assumption: alpha = 0.05
  g <- dplyr::case_when(
    beta == 0.05 ~ 13,
    beta == 0.1 ~ 10.5,
    beta == 0.2 ~ 7.8,
    beta == 0.5 ~ 3.8)
  n <- ((p1*(1 - p1)) + (p2*(1 - p2)))*g/((p1 - p2)^2)
  ceiling(n)
}
