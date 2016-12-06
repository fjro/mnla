#define the functions with associatied parameters to scale the amount of noise added
linear <- function(x, noise, noiseLevel, num.noise, n){x+ noise *(noiseLevel/num.noise)* rnorm(n)}

quadratic <- function(x, noise, noiseLevel, num.noise, n){y=4*(x-.5)^2+  noise * (noiseLevel/num.noise) * rnorm(n)}

cubic <- function(x, noise, noiseLevel, num.noise, n){y=128*(x-1/3)^3-48*(x-1/3)^3-12*(x-1/3)+10* noise  * (noiseLevel/num.noise) *rnorm(n)}
qroot <- function(x, noise, noiseLevel, num.noise, n){y=x^(1/4) + noise * (noiseLevel/num.noise) *rnorm(n)} #x^(1/4) + noise

exponential2 <- function(x, noise, noiseLevel, num.noise, n){y=exp(x^2) + (1.5 *noise * (noiseLevel/num.noise) * rnorm(n))}
logE <- function(x, noise, noiseLevel, num.noise, n){y=log(x) + 2 * (noise * (noiseLevel/num.noise) * rnorm(n))}

sigmoid <- function(x, noise, noiseLevel, num.noise, n){((1 + exp(10*(0.5 - x)))^-1) +( noise * (noiseLevel/num.noise) * rnorm(n))}
step <- function(x, noise, noiseLevel, num.noise, n){y = (x > 0.5) + noise*5*noiseLevel/num.noise *rnorm(n)}

#spike <- function(x, noise, noiseLevel, num.noise, n) { 
 # ifelse (x < 0.05, 4,ifelse (x < 0.1, -18*x + 1.9 ,-x/9 + 1/9 ))+ noise*5*noiseLevel/num.noise *rnorm(n)}

spike <- function(x, noise, noiseLevel, num.noise, n) { 
  v <- 0
  if (x < 0.05) {
    v <- 4
  } else if(x < 0.1) {
    v <- -18*x + 1.9 
  } else {
    v <- -x/9 + 1/9
  }
  v + noise*5*noiseLevel/num.noise *rnorm(n)
}

sinLow <- function(x, noise, noiseLevel, num.noise, n){y=sin(4*pi*x) + 2*noise * (noiseLevel/num.noise) *rnorm(n)}

sinHigh <- function(x, noise, noiseLevel, num.noise, n){y=sin(16*pi*x) + 2*noise * (noiseLevel/num.noise) *rnorm(n)}
linearPeriodic <- function(x, noise, noiseLevel, num.noise, n){y= sin(10*pi*x) + x + 3*noise * (noiseLevel/num.noise) *rnorm(n)}

varyingFreq <- function(x, noise, noiseLevel, num.noise, n){y= sin(5*pi*x*(1+x)) + x + 3*noise * (noiseLevel/num.noise) *rnorm(n)}

circle <- function(x, noise, noiseLevel, num.noise, n){y=(2*rbinom(n,1,0.5)-1) * (sqrt(1 - (2*x - 1)^2)) + noise/4*noiseLevel/num.noise *rnorm(n)}
xShaped <- function(x, noise, noiseLevel, num.noise, n){y=((4*(x-.5)^2 + (noiseLevel/num.noise) * rnorm(n)) * sample( c(-1,1), size=n, replace=T ) )}
