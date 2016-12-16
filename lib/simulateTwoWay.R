#' Simulates a hypothesis nsim times.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param h h0 or hA
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
simulateTwoWay <- function(type, h, measures, nsim, distribution, n, noise, noiseLevel, numNoise) {
  res <- replicate(nsim, h(type, measures, distribution, n, noise, noiseLevel, numNoise))
  t(res)
}
