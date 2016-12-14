#' Estimates the power for a functional relationship for the given measures.
#'
#' @param type The functional relationship, e.g. sineLow
#' @param measures The estimates of association, e.g. dcor.
#' @param measureNamess The corresponding names.
#' @param nsim The number of times the simulation is repeated.
#' @param distribution The samoling distribution, e.g. runif if ~U, rbeta with relevant parameters 
#'  for a skewed case. 
#' @param noise The total level of noise simulated, e.g. 3.
#' @param noiseLevel The noise level to use.
#' @param numNoise The number of noise levels simulated, e.g. 30. 
#' @param ... Extra parameters passed to distribution.
#' @return The power estimate for each measure.
powerForType <- function(type, measures, measureNames, nsim, distribution, n, noise, noiseLevel, numNoise, ...) {
  hoRes <- simulateTwoWay(type, h0, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(hoRes)
  cuts <- apply(hoRes, 2, quantile, (1 - 0.05))
  haRes <- simulateTwoWay(type, hA, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(haRes)
  powerRes <- colSums(sweep(haRes, 2, cuts, ">"))/nsim
  names(powerRes) <- measureNames#paste("A", 1:length(measures), sep="")
  cat(".")
  powerRes
}
