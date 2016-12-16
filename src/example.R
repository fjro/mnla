library(ProjectTemplate)
load.project()

#get all function types from the nlf package
types <- ls(getNamespace("nlf"), all.names=F)

#define the association measures to use
measures <- c(r2, spear)
measureNames <- c('Pearson', 'Spearman')

#run the simulation
system.time(res <- estimatePower(types, 
                                 measures, 
                                 measureNames, 
                                 nsim=500, 
                                 runif, 
                                 noise=3, 
                                 noiseLevels = 1:10, 
                                 sizes=c(50, 100, 250, 500),
                                 ncores="all",
                                 dp1=0,
                                 dp2=1))

#2D scatter plots of power vs noise for all associations and function types
ggplot(res, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_grid(Function~n)+
  theme(legend.position="bottom")

#an interactive surface
plotSurface(res, "Pearson", "sigmoid")