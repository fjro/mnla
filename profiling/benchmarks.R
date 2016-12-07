# simulations is compute intensive so benchmark the core functions to see if things can be improved.

#' Gets the function for a given name and Benchmarks it.
#' 
#' @param fn THe function name
#' @param times The number of repetitions
#' @return A dataframe with the times, expression and function name 
#'
benchmarkFunction <- function(fn, times = 500, n = 320) {
  f <- match.fun(fn)
  x <- runif(n)
  res <- microbenchmark(times = times, f(x, nl, l, num.noise, n))
  res$Function <- fn
  res$n <- n
  res
}

microbenchmark(linear(x, nl, l, num.noise, n), linearCpp(x, nl, l, num.noise, n))
microbenchmark(cubic(x, nl, l, num.noise, n), cubicCpp(x, nl, l, num.noise, n))

#set up the parameters and functions to benchmark.
n <- 1000
nl <- 3 
l <- 6
num.noise <- 30


functions <- c("linear", "quadratic", "cubic", "qroot", "exponential2", "logE", "sigmoid", "step", "spike", "sinLow", "sinHigh",
               "linearPeriodic", "varyingFreq", "circle", 
               "linearCpp", "quadraticCpp", "cubicCpp", "qrootCpp", "exponentialCpp", "logECpp", "sigmoidCpp", "stepCpp", "spikeCpp",
               "sinLowCpp", "sinHighCpp", "linearPeriodicCpp", "varyingFreqCpp", "circleCpp")

functions <- c("linear", "quadratic", "cubic", "qroot", "exponential2", "logE", "sigmoid", "step", "spike",
               "linearCpp", "quadraticCpp", "cubicCpp", "qrootCpp", "exponentialCpp", "logECpp", "sigmoidCpp", "stepCpp", "spikeCpp")

#run the benchmarks and aggregate the results
benchmarks <- sapply(c(10, 20, 30, 50, 100, 200, 350, 500, 1000, 2000, 3000), function(n) lapply(functions, benchmarkFunction, n=n))
benchmarks <- plyr::ldply(benchmarks)

benchmarks$Implementation <- 'R'
benchmarks$Implementation[which(grepl('Cpp', benchmarks$Function))] <- 'Cpp'
benchmarks$Implementation <- as.factor(benchmarks$Implementation)
benchmarks$Function <- str_replace(benchmarks$Function, "Cpp", "")
benchmarks$Function <- str_replace(benchmarks$Function, "2", "")
benchmarks$Function <- as.factor(benchmarks$Function)

#plot the median results
by_median <- benchmarks %>% 
  group_by(Implementation, Function, n) %>% 
  summarise(Time = median(time))

ggplot(by_median, aes(n, Time, colour = Implementation)) + 
  geom_line() +
  ylab("Time (ns)") + 
  facet_wrap(~ Function)
