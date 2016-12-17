#3d surface of a single association measure for a single function type.
plotSurface <- function(res, measure, type, distribution) {
  powerEstimate <- res[res$measure == measure & res$Function == type & res$Distribution == distribution, c(1,2,5)]
  powerEstimate <- as.data.frame(spread(powerEstimate, noiseLevel, power))
  rownames(powerEstimate) <- powerEstimate[,1]
  powerEstimate<- as.matrix(powerEstimate[,-1])
  p <- plot_ly(z = powerEstimate, 
               x = as.numeric(colnames(powerEstimate)), 
               y = as.numeric(rownames(powerEstimate)), 
               type = "surface")%>% 
    layout(title = paste("Power of", measure, "for", type),
           scene = list(
             xaxis = list(title = "Noise"), 
             yaxis = list(title = "n"), 
             zaxis = list(title = "Power")))
  p
}
