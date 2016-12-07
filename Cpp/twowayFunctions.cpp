#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector linearCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return x + noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector quadraticCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return ((4*(x-.5)) * (4*(x-.5))) +  noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector cubicCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return 128 * pow(x-1/3, 3) - 48 * pow(x-1/3, 3) - 12*(x-1/3)+10* noise  * (noiseLevel/numNoise) *rnorm(n);
}

// [[Rcpp::export]]
NumericVector qrootCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return log(x) + 2 * (noise * (noiseLevel/numNoise) * rnorm(n));
}

// [[Rcpp::export]]
NumericVector exponentialCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return 128 * pow(x, 1/4) + noise * (noiseLevel/numNoise) * rnorm(n) ;
}

// [[Rcpp::export]]
NumericVector logECpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return exp(pow(x, 2)) + (1.5 *noise * (noiseLevel/numNoise) * rnorm(n));
}

// [[Rcpp::export]]
NumericVector sigmoidCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  return (1/(1 + exp(10*(0.5 - x)))) + (noise * (noiseLevel/numNoise) * rnorm(n) );
}

// [[Rcpp::export]]
NumericVector stepCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  NumericVector v(x.size());
  for(int i = 0; i < v.size(); i++) {
    if(x[i] > 0.5) {
      v[i] = 1;
    }
  }
  
  return v + (noise*5*noiseLevel/numNoise *rnorm(n));
}

// [[Rcpp::export]]
NumericVector spikeCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
  NumericVector v(x.size());
  for(int i = 0; i < v.size(); i++) {
    if (x[i] < 0.05) {
      v = 4;
    } 
    else if(x[i] < 0.1) {
      v = -18*x + 1.9;
    } 
    else {
      v <- -x[i]/9 + 1/9;
    }  
  }
  
  return v + noise * 5 * noiseLevel/numNoise * rnorm(n);
}

// // [[Rcpp::export]]
// NumericVector sinLowCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return sin(4*pi*x) + 2*noise * (noiseLevel/numNoise) * rnorm(n);
// }
// 
// // [[Rcpp::export]]
// NumericVector sinHighCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return sin(16*pi*x) + 2*noise * (noiseLevel/numNoise) * rnorm(n);
// }
// 
// // [[Rcpp::export]]
// NumericVector linearPeriodicCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return sin(10*pi*x) + x + 3 * noise * (noiseLevel/numNoise) * rnorm(n);
// }
// 
// // [[Rcpp::export]]
// NumericVector varyingFreqCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return sin(5*pi*x*(1+x)) + x + 3 * noise * (noiseLevel/numNoise) * rnorm(n);
// }
// 
// // [[Rcpp::export]]
// NumericVector circleCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return (2*rbinom(n,1,0.5)-1) * (sqrt(1 - pow(2*x - 1),2))) + noise/4*noiseLevel/numNoise *rnorm(n);
// }
// 
// // [[Rcpp::export]]
// NumericVector xShapedCpp(NumericVector x, Int32 noise, float noiseLevel, Int32 numNoise, Int32 n) { 
//   return ((4 * pow(x-.5,2) + (noiseLevel/numNoise) * rnorm(n)) * sample( c(-1,1), size=n, replace=T ) );
// }
// xShaped <- function(x, noise, noiseLevel, num.noise, n){y=((4*(x-.5)^2 + (noiseLevel/num.noise) * rnorm(n)) * sample( c(-1,1), size=n, replace=T ) )}
