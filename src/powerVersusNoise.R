#get all function types from the nlf package
types <- ls(getNamespace("nlf"), all.names=F)

#define the association measures to use
measures <- c(r2, spear)
measureNames <- c('Pearson', 'Spearman')
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

#2D scatter plots of power vs noise for all associations and function types
ggplot(res, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_wrap(~Function)+
  theme(legend.position="bottom")

#now repeat with a skewed disrbutions ~Uniform case
system.time(skewed <- estimatePower(types, 
                                 measures, 
                                 measureNames, 
                                 nsim=500, 
                                 rbeta, 
                                 noise=3, 
                                 noiseLevels = 1:30,  
                                 sizes = sizes))

#2D scatter plots of power vs noise for all associations and function types
ggplot(skewed, aes(noiseLevel, power, colour=measure)) +
  geom_line(size=1.1) +
  facet_wrap(~Function)+
  theme(legend.position="bottom")