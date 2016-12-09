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
  #set.seed(1)
  x <- distribution(n, ...)
  # y <- functions[[typ]](x, noise, l, numNoise, n)
  y <- type(x, noise, noiseLevel, numNoise, n)
  sapply(measures, function(f) f(x,y))
}
#x <- runif(n)
#type <- linearCpp
# distribution = runif
# nsim = 500
# n =  100
# noise =  3
# noiseLevel =  15
# numNoise = 20

# res = simulateTwoWay(linearCpp, h0, measures, nsim, distribution, n, noise, noiseLevel, numNoise)
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
  #cat("simulateTwoWay: nsim =", nsim, ", n = ", n, ", noise = ", noise, ", noiseLevel = ", noiseLevel, ", ", "numNoise =", numNoise)
  res <- replicate(nsim, h(type, measures, distribution, n, noise, noiseLevel, numNoise, ...))
  #write.table(res, 'res1.csv', append=TRUE)
  #cat("dim(res) = ", dim(res))
  res <- t(res)
  #write.table(res, 'res2.csv', append=TRUE)
  #cat("\nsimulateTwoWay: ", res[1,1], res[1,2], res[1,3], res[1,4], sep=", ")
  res
}

powerForType <- function(type, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...) {
  hoRes <- simulateTwoWay(type, h0, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(hoRes)
  cat("\nmean h0: ", means[1], means[2], means[3], means[4], means[5], sep=", ")
  cuts <- apply(hoRes, 2, quantile, (1 - 0.05))
  cat("\ncuts: ", cuts[1], cuts[2], cuts[3], cuts[4], cuts[5], sep=", ")
  haRes <- simulateTwoWay(type, hA, measures, nsim, distribution, n, noise, noiseLevel, numNoise, ...)
  means <- colMeans(haRes)
  cat("\nmean hA: ", means[1], means[2], means[3], means[4], means[5], "\n", sep=", ")
  #write.table(haRes, 'haRes.csv', append=TRUE)
  #write.table(haRes > cuts, 'haResL.csv', append=TRUE)
  #powerRes <-  apply(t(haRes > cuts), 2, sum)/nsim
  cs <- colSums(sweep(haRes, 2, cuts, ">"))
  cat("\ncs: ", cs[1], cs[2], cs[3], cs[4], cs[5], "\n", sep=", ")
  powerRes <- colSums(sweep(haRes, 2, cuts, ">"))/nsim
  #write.table(powerRes, 'powerRes.csv', append=TRUE)
  names(powerRes) <- paste("A", 1:length(measures), sep="")
  powerRes
}

tt <- matrix(3, 3, 3)
#apply(tt, 1, function(x) x/c(1,2,3))
colSums(sweep(tt, 2, c(1,2,3), ">"))/3
# tt %*% diag(1/c(1,2,3))
# 
# t(tt/c(1,2,3))/3
# 
# dim(haRes)
# t(replicate(2, c(0.04468796, 0.0549975, 0.232219)))

getPower <- function(types, measures, nsim = 100, distribution, noise, numNoise, sizes, ...) {
  #build a grid of all noise levels and sample sizes
  parameters <- expand.grid(sizes, 1:numNoise)
  colnames(parameters) <- c("n", "noiseLevel")
  
  powerNoiseAndSize <- function(fn) {
    f <- match.fun(fn)   # y <- functions[[typ]](x, noise, l, numNoise, n)
    res <- apply(parameters, 1, function(x) powerForType(f, measures, nsim, distribution, x[1], noise, x[2], numNoise))
    res <- data.frame(t(res))
    res <- cbind(res, parameters)
    res$Function <- fn
    res 
  }

  #format the results
  res <- lapply(types, powerNoiseAndSize)
  res <- plyr::ldply(res)
  res$noiseLevel <- res$noiseLevel/10
  res <- gather(res, key = measure, value = power, -n, -Function, -noiseLevel)
  res
}
library(minerva)
#test parameters
types <- c('linear')
noise <- 3
numNoise <- 10
r2 <- function(x, y) (cor(x, y))^2
myMine <- function(x, y )mine(x,y)$MIC
myMA <- function(x, y) ma(data.frame(x,y))$A
spear <- function(x, y) (cor(x,y,method='spearman'))^2
measures <- c(r2, spear, dcor, myMine, myMA)

powerForType(linearCpp, measures, nsim=500, runif, n=100, noise=3, noiseLevel=15, numNoise=20)
tpvn(500, alpha=0.05, noise=3, noiseLevel=15, numNoise=20, n = 100)

res = apply(parameters, 1, function(x) powerForType(linearCpp, measures, 100, runif, x[2], noise, x[1], numNoise))
r2  <- plyr::ldply(res)
r3 <- cbind(r2, parameters)


system.time(res <- getPower(types, measures, nsim=100, runif, noise=3, 10, sizes=c(320)))
ggplot(res, aes(noiseLevel, power, colour=measure)) + 
  geom_line(size=1.1) + 
  facet_grid(Function~n)+ 
  theme(legend.position="bottom") 

# powerVersusNoise <- function(#measures,
#   functions, 
#   distribution=runif, 
#   nsim = 500, 
#   alpha=0.05, 
#   nl=3, 
#   numNoise=30, 
#   n=320, ...)
functions <- list("linear"=linear)
set.seed(1)
library(minerva)
library(reshape2)
#install.packages('minerva')
system.time(noiseResults <- powerVersusNoise(functions, nsim = 100, n = 320, numNoise = 10))
ggplot(noiseResults, aes(x=Noise, y=Power,group=Statistic,colour=Statistic)) +
  geom_line(size=1.1) + 
  facet_wrap(~ Form, ncol=3) + 
  theme(legend.position="bottom") 
