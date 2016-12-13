//#include <Rcpp.h>
#include <RcppArmadilloExtensions/sample.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//define pi
double pi = 3.141592653589793238462643383280;

// [[Rcpp::export]]
NumericVector linearCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return x + noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector quadraticCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return  4 * pow(x-.5, 2) +  noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector cubicCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return (128 * pow(x - 1/3.0, 3)) - (48 * pow(x-1/3.0, 3)) - (12 * (x-1/3.0)) + 10 * noise  * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector qrootCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return pow(x, 1/4.0) + noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector exponentialCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return exp(pow(x, 2)) + (1.5 *noise * (noiseLevel/numNoise) * rnorm(n));
}

// [[Rcpp::export]]
NumericVector logECpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return log(x) + 2 * (noise * (noiseLevel/numNoise) * rnorm(n));
}

// [[Rcpp::export]]
NumericVector sigmoidCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  return (1/(1 + exp(10*(0.5 - x)))) + (noise * (noiseLevel/numNoise) * rnorm(n) );
}

// [[Rcpp::export]]
NumericVector stepCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  NumericVector v(x.size());
  for(int i = 0; i < v.size(); i++) {
    if(x[i] > 0.5) {
      v[i] = 1;
    }
  }
  
  return v + (noise*5*noiseLevel/numNoise *rnorm(n));
}

// [[Rcpp::export]]
NumericVector spikeCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) { 
  NumericVector v(x.size());
  for(int i = 0; i < v.size(); i++) {
    if (x[i] < 0.05) {
      v[i] = 4.0;
    } 
    else if(x[i] < 0.1) {
      v[i] = -18.0 * x[i] + 1.9;
    }
    else {
      v[i] = -x[i]/9.0 + 1.0/9.0;
    }
  }
  
  return v + noise * 5 * noiseLevel/numNoise * rnorm(n);
}


// [[Rcpp::export]]
NumericVector sinLowCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  return sin(4*pi*x) + 2*noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector sinHighCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  return sin(16*pi*x) + 2*noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector linearPeriodicCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  return sin(10*pi*x) + x + 3 * noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector varyingFreqCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  return sin(5*pi*x*(1+x)) + x + 3 * noise * (noiseLevel/numNoise) * rnorm(n);
}

// [[Rcpp::export]]
NumericVector circleCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  return (2 * rbinom(n,1,0.5) -1.0) * sqrt(1 - pow(2.0 * x - 1.0,2)) + noise/4.0*noiseLevel/numNoise *rnorm(n);
}

// [[Rcpp::export]]
NumericVector xShapedCpp(NumericVector x, int noise, float noiseLevel, int numNoise, int n) {
  NumericVector vec(2);
  vec[0] = -1;
  vec[1] = 1;
  return (4 * pow(x -0.5, 2) + (noiseLevel/numNoise) * rnorm(n)) *  RcppArmadillo::sample( vec, n, true);
}
