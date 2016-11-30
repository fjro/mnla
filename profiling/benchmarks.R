# simulations is compute intensive so benchmark the core functions to see if things can be improved.

#' Gets the function for a given name and Benchmarks it.
#' 
#' @param fn THe function name
#' @param times The number of repetitions
#' @return A dataframe with the times, expression and function name 
#'
benchmarkFunction <- function(fn, times = 500) {
  f <- match.fun(fn)
  res <- microbenchmark(times = times, f(x, nl, l, num.noise, n))
  res$Function <- fn
  res
}

#set up the parameters and functions to benchmark.
n <- 320
nl <- 3 
l <- 6
num.noise <- 30
x <- runif(n)

functions <- c("linear", "quadratic", "cubic", "qroot", "exponential2", "logE", "sigmoid", "step", "spike", "sinLow", "sinHigh",
               "linearPeriodic", "varyingFreq", "circle", "xShaped")

#run the benchmarks and aggregate the results
benchmarks <- lapply(functions, benchmarkFunction)
benchmarks <- plyr::ldply(benchmarks)

#plot the results
ggplot(benchmarks, aes(Function, time)) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
