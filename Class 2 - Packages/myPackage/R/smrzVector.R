#' @title Summarize A Vector
#' @description Produce standard summary measures for a numeric vector.
#'
#' @param x a vector of numbers
#'
#' @return a vector of summary values
#' @export

smrzVector <- function(x) {
  num.na <- sum(is.na(x))
  mn <- mean(x, na.rm = TRUE)
  md <- median(x, na.rm = TRUE)
  vr <- var(x, na.rm = TRUE)
  c(NAs = num.na, mean = mn, median = md, variance = vr)
}