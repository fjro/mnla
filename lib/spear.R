#' Simple wrapper to get Spearman correlation
spear <- function(x, y) (cor(x,y,method='spearman'))^2