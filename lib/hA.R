#' The alt hypothesis. Generates depenent random variables and measures the association.
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
hA <- function(type, measures, distribution, n, noise, noiseLevel, numNoise, dp1, dp2) {
  x <- distribution(n, dp1, dp2)
  y <- type(x, noise, noiseLevel, numNoise, n)
  sapply(measures, function(f) f(x,y))
}