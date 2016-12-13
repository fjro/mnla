#' The null hypothesis. Generates indepenent random variables and measures the association.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
h0 <- function(type, measures, distribution, n, noise, noiseLevel, numNoise, ...) {
  #set.seed(1)
  y <- type(distribution(n, ...), noise, noiseLevel, numNoise, n)
  #set.seed(1)
  x <- distribution(n, ...)
  res <- sapply(measures, function(f) f(x,y))
  #cat("\nh0: ", res[1], res[2], res[3], res[4], sep=", ")
  res
}
