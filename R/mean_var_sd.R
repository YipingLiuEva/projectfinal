#' Mean, Variance, and standard deviation calculation.
#'
#' There are three functions calculate mean, var, and sd for dataset.
#'
#' @param input datagroup and weights for calculation
#' @param Mean, Variance, and standard deviation
#'
#' @example

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}
