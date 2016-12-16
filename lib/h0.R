#' The null hypothesis. Generates indepenent random variables and measures the association.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
h0 <- function(type, measures, distribution, n, noise, noiseLevel, numNoise) {
  y <- type(distribution(n), noise, noiseLevel, numNoise, n)
  x <- distribution(n)
  sapply(measures, function(f) f(x,y))
}


