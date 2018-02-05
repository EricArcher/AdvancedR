
#' @title Summarize Numeric vectors
#'
#' @description Gives summaries of a set of numbers
#' @param x a vector of numbers
#' @return mean, median, and variance of vector
#' @export

smrzvectors <- function (x) {
  mn <- mean(x)
  md <- median(x)
  vr <- var (x)
  c(mean = mn, median = md, variance = vr)

}



#'
#' using create to create a package folder
#' library(devtools)
#> ?create
#create(nameofproject)
