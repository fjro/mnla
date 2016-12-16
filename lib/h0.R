#' The null hypothesis. Generates indepenent random variables and measures the association.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
#' @param dp1 A paramter passed to the distribution, e.g. shape1 for rbeta
#' @param dp2 A second paramter passed to the distribution, e.g. shape2 for rbeta
h0 <- function(type, measures, distribution, n, noise, noiseLevel, numNoise, dp1, dp2) {
  print(as.list(match.call()))
  y <- type(distribution(n, dp1, dp2), noise, noiseLevel, numNoise, n)
  x <- distribution(n, dp1, dp2)
  sapply(measures, function(f) f(x,y))
}


