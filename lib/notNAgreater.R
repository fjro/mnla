## We write the following function because MIC glitches a small percentage of the time, and we do not wish to average over those trials
notNA.greater <- function(a,b){
  ind <- which(!is.na(a))
  pow <- sum(a[ind] > b)/length(ind)
  return(pow)
}