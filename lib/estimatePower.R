#' Estimates the power of measures of association to detect different types of functional relationships.
#'
#' @param types The bivariate function names.
#' @param measures The functions to estimate association, e.g. dcor.
#' @param measureNamess The corresponding names.
#' @param nsim The number of times the simulation is repeated.
#' @param distribution The samoling distribution, e.g. runif if ~U, rbeta with relevant parameters 
#'  for a skewed case. Use ... for extra paramters.
#' @param noise The total level of noise simulated, e.g. 3.
#' @param noiseLevels The number of noise levels simulated, e.g. 1:30.
#' @param sizes The smaple sizes to use, e.g. c(50, 100, 250, 500)
#' @param ncores The number of cores to use for processing. Use "all" for maximum available.
#' @return A tidy dataframe with the results.
#' 
estimatePower <- function(types, measures, measureNames, nsim, distribution, noise, noiseLevels, sizes, ncores='all') {
  #build a grid of all noise levels and sample sizes
  parameters <- expand.grid(sizes, noiseLevels)
  colnames(parameters) <- c("n", "noiseLevel")
  
  powerNoiseAndSize <- function(fn) {
    cat("Simulating ", fn, "at", format(Sys.time()), "\n")
    f <- match.fun(fn)   
    res <- apply(parameters, 1, function(x) powerForType(f, measures, measureNames, nsim, distribution, x[1], noise, x[2], max(noiseLevels)))
    res <- data.frame(t(res))
    res <- cbind(res, parameters)
    res$Function <- fn
    res
  }
  
  #use a sensible number of cores for processing
  if (ncores == 'all') {
    ncores <- detectCores() -1
    if (length(types) < ncores) {
      ncores <- length(types)
    }
  }

 # do the work in parallel
  sfInit( parallel=TRUE, cpus=ncores, slaveOutfile='logs/log.txt')
  sfLibrary(nlf)
  sfLibrary(energy)
  sfLibrary(matie)
  sfLibrary(minerva)
  sfExportAll()
  #sfExport('powerForType', 'simulateTwoWay', 'h0', 'hA', 'parameters', 'measures', 'measureNames', 'noise', 'r2', 'spear')
  res <- sfLapply(types, powerNoiseAndSize)
  sfStop()
  
  #format the results
  res <- plyr::ldply(res)
  gather(res, key = measure, value = power, -n, -Function, -noiseLevel)
}
