library(ProjectTemplate)
load.project()
library(nlf)


#install.packages('minerva')
#library(minerva)
#test parameter

rm(linearCpp)
types <- c("quadraticCpp", "cubicCpp", "qrootCpp")
noise <- 3
numNoise <- 30

measures <- c(r2, spear, dcor)#, myMine, myMA)
measures <- c(r2, spear)
measureNames <- c('Pearson', 'Spearman')

system.time(res <- estimatePower(types, measures, measureNames, nsim=500, runif, noise=3, numNoise, sizes=c(50, 100, 250, 500)))
ggplot(res, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_grid(n~Function)+
  theme(legend.position="bottom")

#3d surface 
library(plotly)
powerEstimate <- res[res$measure == 'Pearson' & res$Function == "cubicCpp",c(1,2,5)]
powerEstimate <- spread(powerEstimate, noiseLevel, power)
rownames(powerEstimate) <- powerEstimate[,1]
powerEstimate<- as.matrix(powerEstimate[,-1])
p <- plot_ly(z = powerEstimate, 
             x = as.numeric(colnames(powerEstimate)), 
             y = as.numeric(rownames(powerEstimate)), 
             type = "surface")%>% 
  layout(title = "Power: Pearson Correlation",
         scene = list(
           xaxis = list(title = "Noise"), 
           yaxis = list(title = "n"), 
           zaxis = list(title = "Power")))
p

