#define the functions with associatied parameters to scale the amount of noise added
linear <- function(x, noise, noiseLevel, numNoise, n) {x + noise *(noiseLevel/numNoise)* rnorm(n)}

quadratic <- function(x, noise, noiseLevel, numNoise, n) { 4*(x-.5)^2+  noise * (noiseLevel/numNoise) * rnorm(n)}

cubic <- function(x, noise, noiseLevel, numNoise, n) { 
  128*(x-1/3)^3-48*(x-1/3)^3-12*(x-1/3)+10* noise  * (noiseLevel/numNoise) *rnorm(n)
}
qroot <- function(x, noise, noiseLevel, numNoise, n) { 
  x^(1/4) + noise * (noiseLevel/numNoise) *rnorm(n)
} 

exponential2 <- function(x, noise, noiseLevel, numNoise, n){ exp(x^2) + (1.5 *noise * (noiseLevel/numNoise) * rnorm(n))}
logE <- function(x, noise, noiseLevel, numNoise, n) { log(x) + 2 * (noise * (noiseLevel/numNoise) * rnorm(n))}

sigmoid <- function(x, noise, noiseLevel, numNoise, n) {((1 + exp(10*(0.5 - x)))^-1) +( noise * (noiseLevel/numNoise) * rnorm(n))}
step <- function(x, noise, noiseLevel, numNoise, n) { (x > 0.5) + noise*5*noiseLevel/numNoise *rnorm(n) }

spike <- function(x, noise, noiseLevel, numNoise, n) { 
  v <- 0
  if (x < 0.05) {
    v <- 4
  } else if(x < 0.1) {
    v <- -18*x + 1.9 
  } else {
    v <- -x/9 + 1/9
  }
  v + noise*5*noiseLevel/numNoise *rnorm(n)
}

sinLow <- function(x, noise, noiseLevel, numNoise, n) { sin(4*pi*x) + 2*noise * (noiseLevel/numNoise) *rnorm(n)}

sinHigh <- function(x, noise, noiseLevel, numNoise, n) { sin(16*pi*x) + 2*noise * (noiseLevel/numNoise) *rnorm(n)}
linearPeriodic <- function(x, noise, noiseLevel, numNoise, n) { sin(10*pi*x) + x + 3*noise * (noiseLevel/numNoise) *rnorm(n)}

varyingFreq <- function(x, noise, noiseLevel, numNoise, n) { sin(5*pi*x*(1+x)) + x + 3*noise * (noiseLevel/numNoise) *rnorm(n)}

circle <- function(x, noise, noiseLevel, numNoise, n) {(2*rbinom(n,1,0.5)-1) * (sqrt(1 - (2*x - 1)^2)) + noise/4*noiseLevel/numNoise *rnorm(n)}
xShaped <- function(x, noise, noiseLevel, numNoise, n) {((4*(x-.5)^2 + (noiseLevel/numNoise) * rnorm(n)) * sample( c(-1,1), size=n, replace=T ) )}
