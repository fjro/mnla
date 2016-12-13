#' Estimates the power for a functional relationship for the given measures.
#'
#'
powerForType <- function(type, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...) {
  hoRes <- simulateTwoWay(type, h0, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(hoRes)
  cuts <- apply(hoRes, 2, quantile, (1 - 0.05))
  haRes <- simulateTwoWay(type, hA, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(haRes)
  powerRes <- colSums(sweep(haRes, 2, cuts, ">"))/nsim
  names(powerRes) <- paste("A", 1:length(measures), sep="")
  cat(".")
  powerRes
}