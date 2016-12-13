#' Computes the 'A' estimate of association between two vectors.
myMA <- function(x, y) ma(data.frame(x,y))$A