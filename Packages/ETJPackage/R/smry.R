#'@title Summary of numeric vectors
#'
#'@description Returns the length, mean, and median of a vectorof numbers
#'
#'@param x a vector of numbers
#'@param y a second number
#'
#'@return A vector with the length (n), mean, and median.
#'
#'@examples
#'# summarize 100 normally distributed numbers with mean of 5 and sd of 1
#'x.vec <- rnorm(100, 5, 1)
#'smry(x,vec)
#'
#'\dontrun{
#'# can't use with characters
#'smry(letters)}
#'
#'
#'@export
smry <- function(x, y = 5) {
  if (!is.numberic(x)) stop("x must be a number!")
  mean <- mean(x)
  median <- median(x)
  n <- length(x)
  c(n = n, mean = mean, median = median)
}
#click all roxygen options under Build Tools in Package options
#at minimum, all script functions need the following roxygen tags at the begining of script #'@title Summary of numeric vectors
#'
#'@description Returns the length, mean, and median of a vectorof numbers
#'
#'@param x a vector of numbers
#'
#'@return A vector with the length (n), mean, and median.
#'
#'@export
#@param lists all arguments in function
#side note from class:
#package.skeleton("anotherPkg", list = ls()) - not clear on usage...Eric doesn't use very much
