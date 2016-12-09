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
  y <- type(distribution(n, ...), noise, noiseLevel, numNoise, n)
  x <- distribution(n, ...)
  sapply(measures, function(f) f(x,y))
}

#' The alt hypothesis. Generates depenent random variables and measures the association.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
hA <- function(type, measures, distribution, n, noise, noiseLevel, numNoise, ...) {
  x <- distribution(n, ...)
  y <- type(x, noise, noiseLevel, numNoise, n)
  sapply(measures, function(f) f(x,y))
}

#' The null hypothesis. Generates indepenent random variables and measures the association.
#'
#' @param type The type of functional relationship, e.g. linear.
#' @param h h0 or hA
#' @param measures A list of estimates of association.
#' @param distribution The distribution to sample from.
#' @param n The sample size.
#' @param noise The total level of noise.
#' @param noiseLevel The fraction of noise.
#' @param numNoise The noise increment.
simulateTwoWay <- function(type, h, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...) {
  res <- replicate(nsim, h(type, measures, distribution, n, noise, noiseLevel, numNoise, ...))
  t(res)
}

powerForType <- function(type, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...) {
  hoRes <- simulateTwoWay(type, h0, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  cuts <- apply(hoRes, 2, quantile, (1 - 0.05))
  haRes <- simulateTwoWay(type, hA, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  powerRes <-  apply(haRes > cuts, 2, sum)/nsim
  names(powerRes) <- paste("A", 1:length(measures), sep="")
  powerRes
}

getPower <- function(types, measures, nsim = 100, distribution, noise, numNoise, sizes, ...) {
  #build a grid of all noise levels and sample sizes
  parameters <- expand.grid(1:numNoise, sizes)
  colnames(parameters) <- c("noiseLevel", "n")
  
  powerNoiseAndSize <- function(fn) {
    f <- match.fun(fn)
    res <- apply(parameters, 1, function(x) powerForType(f, measures, nsim, distribution, x[2], noise, x[1], numNoise))
    res <- data.frame(t(res))
    res <- cbind(res, parameters)
    res$Function <- fn
    res 
  }

  #format the results
  res <- lapply(types, powerNoiseAndSize)
  res <- plyr::ldply(res)
  res$noiseLevel <- res$noiseLevel/numNoise
  res <- gather(res, key = measure, value = power, -n, -Function, -noiseLevel)
  res
}

library(tidyr)

#test parameters
types <- c('linear')
distributions <- c(runif, rnorm)
noise <- 3
numNoise <- 30
r2 <- function(x, y) (cor(x, y))^2
myMine <- function(x, y )mine(x,y)$MIC
myMA <- function(x, y) ma(data.frame(x,y))$A
spear <- function(x, y) (cor(x,y,method='spearman'))^2
measures <- c(r2, dcor, spear, myMine, myMA)

powerForType(linearCpp, measures, 100, runif, 10, 3, 2, 20)
res = apply(parameters, 1, function(x) powerForType(linearCpp, measures, 100, runif, x[2], noise, x[1], numNoise))
r2  <- plyr::ldply(res)
r3 <- cbind(r2, parameters)


system.time(res <- getPower(types, measures, nsim=100, runif, noise=3, 10, sizes=c(320)))
ggplot(res, aes(noiseLevel, power, colour=measure)) + geom_line() + facet_grid(Function~n)

# powerVersusNoise <- function(#measures,
#   functions, 
#   distribution=runif, 
#   nsim = 500, 
#   alpha=0.05, 
#   nl=3, 
#   numNoise=30, 
#   n=320, ...)
system.time(noiseResults <- powerVersusNoise(functions, nsim = 100, n = 320, numNoise = 10))
ggplot(noiseResults, aes(x=Noise, y=Power,group=Statistic,colour=Statistic)) +
  geom_line(size=1.1) + 
  facet_wrap(~ Form, ncol=3) + 
  theme(legend.position="bottom") 
