#get all function types from the nlf package
types <- ls(getNamespace("nlf"), all.names=F)

#define the association measures to use
measures <- c(r2, spear, dcor, myMA, myMine)
measureNames <- c('A', 'MIC')
sizes=c(10,20,30,40,50,75,100,125,150,200,250,300,350,400,500,750,1000)

#run the simulation for the ~Uniform case
system.time(res <- estimatePower(types, 
                                 measures, 
                                 measureNames, 
                                 nsim=500, 
                                 runif, 
                                 noise=3, 
                                 noiseLevels = 1:30, 
                                 sizes=sizes))
res$Distribution <- "Uniform"


#2D scatter plots of power vs noise for all associations and function types
ggplot(res, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_grid(n~Function)+
  theme(legend.position="bottom")

#now repeat with a skewed disrbutions ~Uniform case
system.time(skewed <- estimatePower(types, 
                                 measures, 
                                 measureNames, 
                                 nsim=500, 
                                 function(n) rbeta(n, 2, 5), 
                                 noise=3, 
                                 noiseLevels = 1:30,  
                                 sizes = sizes))
skewed$Distribution <- "Beta"

#2D scatter plots of power vs noise for all associations and function types
ggplot(skewed, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_grid(n~Function)+
  theme(legend.position="bottom")

powerResults <- rbind(res, skewed)
write_feather(powerResults, "data/powerResults.feather")
              
ggplot(powerResults[powerResults$Distribution == "Beta" & powerResults$n == 350,], aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_wrap(~Function)+
  theme(legend.position="bottom")

plotSurface(powerResults, "Pearson", "sigmoid", "Uniform")
