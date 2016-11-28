library(ggplot2)
library(reshape2)
library(energy)
library(minerva)
library(matie)


#TODO: simulate performance of penalised spline and HHG

## We write the following function because MIC glitches a small percentage of the time, and we do not wish to average over those trials
notNA.greater <- function(a,b){
  ind <- which(!is.na(a))
  pow <- sum(a[ind] > b)/length(ind)
  return(pow)
}


calculateAssociations <- function(i, n, distribution, twf, nl, l, num.noise, associations, nullScenario = T, ...) {
  #sample from the given distribution
  x <- distribution(n,...)
  
  #evaluate the function 
  y <- twf(x, nl, l, num.noise, n)
  
  # We resimulate x so that we have the null scenario
  if (nullScenario) {
    x <- distribution(n,...)  
  }
  
  # calculate the association
  sapply(associations, function (f) f(x, y))
}
x
aa <- array(NA, c(2, 2, 3))
functions <- list("linear"=linear)

#run the noise simulation using a beta(2,5) and plot the results
set.seed(1)
associations <- c(cor, dcor)
distribution <- runif
nl = 2
l = 1
nullAssociations <-sapply(1:nsim, calculateAssociations, n, distribution, functions[[1]], nl, l, num.noise, associations)

powerVersusNoise(associations, functions, runif, nsim = 5, n=10, num.noise = 3)
attr(mean, 'names')
match.call(associations)
mget(associations)
fname <- function(f) substitute(f)
fname(associations[1])
sapply(associations, fname)
#' A function to simulate power while varying the amount of noise.
#' 
#' @param functions A list of functional forms to be evaluated
#' @param distribution The source distribtion, uniform by default
#' @param nsim The number of datasets used to estimate the rejection reject regions for an alternative with level alpha
#' @param num.noise Number of different noise levels used
#' @param nl The total level of noise
#' @param n Number of data points per simulation
powerVersusNoise <- function(associations,
                             functions, 
                             distribution=runif, 
                             nsim = 500, 
                             alpha=0.05, 
                             nl=3, 
                             num.noise=30, 
                             n=320, ...) {
  # Vectors holding the null and alternative "associations" for each of the nsim null datasets at a given noise level
  #val.cor=val.dcor=val.mine=val.A=val.spear=rep(NA,nsim)        
  #val.cor2=val.dcor2=val.mine2=val.A2=val.spear2= rep(NA,nsim)        
  #nullAssociations <- array(NA, c(nsim, length(associations)))
  #alternateAssociation <- array(NA, c(nsim, length(associations)))
  
  # Arrays holding the estimated power for each of the association types, 
  #for each functional with each noise level
  #power.cor=power.dcor=power.mine=power.A=power.spear= array(NA, c(length(functions),num.noise))                
  power <- array(NA, c(length(functions),num.noise, length(associations)))
  cat("dim(power) = ", dim(power), "\n")
  cat("power = ", power, "\n")
  #Loop through the noise level and functional form; 
  #each time we estimate a null distribution based on the marginals of the data, 
  #and then use that null distribution to estimate power
  for(l in 1:num.noise)
  {
    for(typ in 1:length(functions))
    {
      ## This next loop simulates data under the null with the correct marginals (x is uniform, and y is a function of a uniform with gaussian noise)
      
      cat(sprintf("Noise Null %s; Type %s\n",  l, typ))
      
      nullAssociations <-sapply(1:nsim, calculateAssociations, n, distribution, functions[[typ]], nl, l, num.noise, associations)
      cuts <- apply(nullA, 1, quantile, 1 - 0.05)
      # for(ii in 1:nsim) #replace loop with matrix
      # {
      #   #sample from the given distribution
      #   x <- distribution(n,...)
      #   
      #   #evaluate the function 
      #   y <- functions[[typ]](x, nl, l, num.noise, n)
      #   
      #   # We resimulate x so that we have the null scenario
      #   x <- distribution(n,...)
      #   
      #   # calculate the association
      #   nullAssociations[ii,] <- sapply(associations, function (f) f(x, y))
      #   #val.cor[ii]=(cor(x,y))^2            
      #   #val.dcor[ii]=dcor(x,y)            
      #   #val.mine[ii]=mine(x,y)$MIC            
      #   #val.A[ii]=ma(data.frame(x,y))$A             
      #   #val.spear[ii]=(cor(x,y,method='spearman'))^2            
      # }
      
      # remove the mic trials which glitch
      #val.mine <- val.mine[which(!is.na(val.mine))]                 
      
      # Next we calculate our rejection cutoffs
      #cuts <- apply(nullAssociations, 2, quantile, 1 - alpha)
      
      # cut.cor <- quantile(val.cor,1-alpha)
      # cut.dcor <- quantile(val.dcor,1-alpha)
      # cut.mine <- quantile(val.mine,1-alpha)
      # cut.A <- quantile(val.A,1-alpha)
      # cut.spear <- quantile(val.spear,1-alpha)
      
      ## Next we simulate the data again, this time under the alternative
      cat(sprintf("Noise Alt %s; Type %s;\n",  l, typ))
      # for(ii in 1:nsim)
      # {
      #   #the dependent data
      #   x <- distribution(n,...)
      #   y <- functions[[typ]](x, nl, l, num.noise, n)
      #   
      #   # calculate the associations
      #   alternateAssociation[ii,] <-  sapply(associations, function (f) f(x, y))
      #   # val.cor2[ii]=(cor(x,y))^2
      #   # val.dcor2[ii]=dcor(x,y)
      #   # val.mine2[ii]=mine(x,y)$MIC
      #   # val.A2[ii]=ma(data.frame(x,y))$A
      #   # val.spear2[ii]=(cor(x,y,method='spearman'))^2
      # }
      alternateAssociation <- sapply(1:nsim, calculateAssociations, n, distribution, functions[[typ]], nl, l, num.noise, associations)
      
      ## Now we estimate the power as the number of alternative statistics exceeding our estimated cutoffs
      power[typ,l,] <- diag(sapply(cuts, function(c) apply(alternateAssociation, 1, function(x) sum(x > c)/nsim)))
      
      # power.cor[typ,l] <- sum(val.cor2 > cut.cor)/nsim
      # power.dcor[typ,l] <- sum(val.dcor2 > cut.dcor)/nsim
      # power.mine[typ,l] <- notNA.greater(val.mine2, cut.mine)
      # power.A[typ,l] <- sum(val.A2 > cut.A)/nsim
      # power.spear[typ,l] <- sum(val.spear2 > cut.spear)/nsim
    }
  }
  
  cat(power)
  #assemble the results
  powerdf <- adply(power, 3)
  # powerdf <- data.frame(power.cor)
  # powerdf <- t(powerdf)
  # colnames(powerdf) <- names(functions)
  # 
  # results <- melt(powerdf)
  # results <- cbind(results,rep('Pearson',num.noise))
  # results <- cbind(results,(1:num.noise)/10)
  # colnames(results) <- c('var','Form','Power','Statistic','Noise')
  # results <- combineNoiseResults(results,power.dcor,'dcor',num.noise)
  # results <- combineNoiseResults(results,power.mine,'MIC',num.noise)
  # results <- combineNoiseResults(results,power.A,'A',num.noise)
  # results <- combineNoiseResults(results,power.spear,'Spearman',num.noise)
  # 
  # #reorder based on the functional form
  # results$Form <- reorder(results$Form, new.order=names(functions))
  # results <- results[ order(results$Form), ]
  # 
  # results
}
?adply
library(plyr)
adply(aa, 3)
dim(array(NA, c(4,5)))
#a function to simulate power while varying the amount of noise
#functions = a list of functional forms to be evaluated
#sizes = a vector of sample sizes to be evaluated
#distribution = the source distribtion, uniform by default
#nsim = the number of datasets used to estimate the rejection reject regions for an alternative with level alpha
#nl = the total level of noise
powerVersusSampleSize <- function(functions, sizes, distribution=runif, nsim = 500, alpha=0.05, nl=3, ...)
{
  val.cor=val.dcor=val.mine=val.A=val.spear=rep(NA,nsim)        
  val.cor2=val.dcor2=val.mine2=val.A2=val.spear2= rep(NA,nsim)        
  
  power.cor=power.dcor=power.mine=power.A=power.spear= array(NA, c(length(functions),length(sizes)))               
  
  for(k in 1:length(sizes))
  {
    n = sizes[k]
    for(typ in 1:length(functions))
    {
      for(ii in 1:nsim)
      {
        #sample from the given distribution
        x <- distribution(n, ...)
        y <- functions[[typ]](x, nl, 30/nl, 30, n)
        x <- distribution(n, ...)
        
        # calculate the association
        val.cor[ii]=(cor(x,y))^2          
        val.dcor[ii]=dcor(x,y)             
        val.mine[ii]=mine(x,y)$MIC           
        val.A[ii]=ma(data.frame(x,y))$A            
        val.spear[ii]=(cor(x,y,method='spearman'))^2             
      }
      
      val.mine <- val.mine[which(!is.na(val.mine))]                
      
      ## Next we calculate our rejection cutoffs
      cut.cor=quantile(val.cor,1-alpha)
      cut.dcor=quantile(val.dcor,1-alpha)
      cut.mine=quantile(val.mine,1-alpha)
      cut.A=quantile(val.A,1-alpha)
      cut.spear=quantile(val.spear,1-alpha)
      
      ## Next we simulate the data again, this time under the alternative
      for(ii in 1:nsim)
      {
        x = distribution(n, ...)
        y=functions[[typ]](x, nl, 30/nl, 30, n)
        val.cor2[ii]=(cor(x,y))^2
        val.dcor2[ii]=dcor(x,y)
        val.mine2[ii]=mine(x,y)$MIC
        val.A2[ii]=ma(data.frame(x,y))$A
        val.spear2[ii]=(cor(x,y,method='spearman'))^2
      }
      
      ## Now we estimate the power as the number of alternative statistics exceeding our estimated cutoffs
      power.cor[typ,k] <- sum(val.cor2 > cut.cor)/nsim
      power.dcor[typ,k] <- sum(val.dcor2 > cut.dcor)/nsim
      power.mine[typ,k] <- notNA.greater(val.mine2, cut.mine)
      power.A[typ,k] <- sum(val.A2 > cut.A)/nsim
      power.spear[typ,k] <- sum(val.spear2 > cut.spear)/nsim
    }
  }
  
  #assemble the results
  powerdf= data.frame(power.cor)
  powerdf = t(powerdf)
  colnames(powerdf) = names(functions)
  
  results = melt(powerdf)
  results = cbind(results,rep('Pearson',length(sizes)))
  results = cbind(results,sizes)
  colnames(results) = c('var','Form','Power','Statistic','Size')
  results = combineSizeResults(results,power.dcor,'dcor',sizes)
  results = combineSizeResults(results,power.mine,'MIC',sizes)
  results = combineSizeResults(results,power.A,'A',sizes)
  results = combineSizeResults(results,power.spear,'Spearman',sizes)
  
  #reorder based on the functional form
  results$Form <- reorder(results$Form, new.order=names(functions))
  results <- results[ order(results$Form), ]
  
  results
}


#a utility function to build up the noise results
combineNoiseResults <- function(df, x, stat, num.noise)
{
  tf <- data.frame(x)
  tf <- t(tf)
  colnames(tf) <- names(functions)
  tf <- melt(tf)
  tf <- cbind(tf,rep(stat,num.noise))
  tf <- cbind(tf,(1:num.noise)/10)
  colnames(tf) <- c('var','Form','Power','Statistic','Noise')
  df <- rbind(df,tf)
  df
}

#a utility function to build up the sample size results
combineSizeResults <- function(df, x, stat, sizes)
{
  tf <- data.frame(x)
  tf <- t(tf)
  colnames(tf) <- names(functions)
  tf <- melt(tf)
  tf <- cbind(tf,rep(stat,length(sizes)))
  tf <- cbind(tf,sizes)
  colnames(tf) <- c('var','Form','Power','Statistic','Size')
  df <- rbind(df,tf)
  df
}

#add the desired functions to a list for evaluation during simmulations
functions <- list("linear"=linear,"Quadratic"=quadratic,"Cubic"=cubic, "Fourth Root" = qroot, "Exponential" = exponential2, 
                  "Natural Log" = logE, "Sigmoid" = sigmoid, "Step"=step, "Spike" = spike,
                  "Sine: Low"= sinLow, "Sine: High" = sinHigh, "Linear+Periodic" = linearPeriodic, "Varying Frequency" = varyingFreq,
                  "Circle" = circle, "X" = xShaped)

functions <- list("linear"=linear)

#run the noise simulation using a beta(2,5) and plot the results
set.seed(1)
associations <- c(cor, dcor)
noiseResults <- powerVersusNoise(associations, functions, rbeta, shape1=2, shape2=5, nsim = 5, n=10, num.noise = 10)
ggplot(noiseResults, aes(x=Noise, y=Power,group=Statistic,colour=Statistic)) +
  geom_line(size=1.1) + 
  facet_wrap(~ Form, ncol=3) + 
  theme(legend.position="bottom")  

#run the sample size simulations and plot the results
sampleSizes <- c(10,20,30,40,50,75,100,125,150,200,250,300,350,400,500,750,1000)
set.seed(1)
sizeResults <- powerVersusSampleSize(functions, sampleSizes, rbeta, shape1=2, shape2=5)
ggplot(sizeResults, aes(x=Size, y=Power,group=Statistic,colour=Statistic)) +
  geom_line(size=1.1) + facet_wrap(~ Form, ncol=3) + theme(legend.position="bottom")  


write.csv(ff,'powerNoise320Beta25.csv')

#plot the function forms
n=320
noise=1
num.noise=1
l=0.1

#builds up the frame
appendForm <- function(df, fun, type, noiseLevel)
{
  y=fun(x, 3, noiseLevel, 30)
  ndf=data.frame(x,y, rep(type,n),rep(noiseLevel,n))
  colnames(ndf)[3:4]=c('Form','Noise')
  df = rbind(df, ndf)
  df
}

#define the frame
x <- runif(n)
df <- data.frame(x,linear(x,3,0.1,30),rep('Linear',n), rep(0.1,n))
colnames(df) <- c('x', 'y', 'Form','Noise')

#build it up
df = appendForm(df,linear,'Linear',1)
df = appendForm(df,linear,'Linear',10)
df = appendForm(df,linear,'Linear',20)
df = appendForm(df,linear,'Linear',30)

df = appendForm(df,parabolic,'Quadratic',0.1)
df = appendForm(df,parabolic,'Quadratic',1)
df = appendForm(df,parabolic,'Quadratic',10)
df = appendForm(df,parabolic,'Quadratic',20)
df = appendForm(df,parabolic,'Quadratic',30)

df = appendForm(df,cubic,'Cubic',0.1)
df = appendForm(df,cubic,'Cubic',1)
df = appendForm(df,cubic,'Cubic',10)
df = appendForm(df,cubic,'Cubic',20)
df = appendForm(df,cubic,'Cubic',30)

df = appendForm(df,qroot,'X^(1/4)',0.1)
df = appendForm(df,qroot,'X^(1/4)',1)
df = appendForm(df,qroot,'X^(1/4)',10)
df = appendForm(df,qroot,'X^(1/4)',20)
df = appendForm(df,qroot,'X^(1/4)',30)

df = appendForm(df,exponential2,'Exponential',0.1)
df = appendForm(df,exponential2,'Exponential',1)
df = appendForm(df,exponential2,'Exponential',10)
df = appendForm(df,exponential2,'Exponential',20)
df = appendForm(df,exponential2,'Exponential',30)

df = appendForm(df,logE,'Log',0.1)
df = appendForm(df,logE,'Log',1)
df = appendForm(df,logE,'Log',10)
df = appendForm(df,logE,'Log',20)
df = appendForm(df,logE,'Log',30)

df = appendForm(df,sigmoid,'Sigmoid',0.1)
df = appendForm(df,sigmoid,'Sigmoid',1)
df = appendForm(df,sigmoid,'Sigmoid',10)
df = appendForm(df,sigmoid,'Sigmoid',20)
df = appendForm(df,sigmoid,'Sigmoid',30)

df = appendForm(df,step,'Step Function',0.1)
df = appendForm(df,step,'Step Function',1)
df = appendForm(df,step,'Step Function',10)
df = appendForm(df,step,'Step Function',20)
df = appendForm(df,step,'Step Function',30)

df = appendForm(df,spike,'Spike',0.1)
df = appendForm(df,spike,'Spike',1)
df = appendForm(df,spike,'Spike',10)
df = appendForm(df,spike,'Spike',20)
df = appendForm(df,spike,'Spike',30)

df = appendForm(df,sin1,'Sine: Low',0.1)
df = appendForm(df,sin1,'Sine: Low',1)
df = appendForm(df,sin1,'Sine: Low',10)
df = appendForm(df,sin1,'Sine: Low',20)
df = appendForm(df,sin1,'Sine: Low',30)

df = appendForm(df,sin2,'Sine: High',0.1)
df = appendForm(df,sin2,'Sine: High',1)
df = appendForm(df,sin2,'Sine: High',10)
df = appendForm(df,sin2,'Sine: High',20)
df = appendForm(df,sin2,'Sine: High',30)

df = appendForm(df,linearPeriodic,'Linear+Periodic',0.1)
df = appendForm(df,linearPeriodic,'Linear+Periodic',1)
df = appendForm(df,linearPeriodic,'Linear+Periodic',10)
df = appendForm(df,linearPeriodic,'Linear+Periodic',20)
df = appendForm(df,linearPeriodic,'Linear+Periodic',30)

df = appendForm(df,varyingFreq,'Varying Frequency',0.1)
df = appendForm(df,varyingFreq,'Varying Frequency',1)
df = appendForm(df,varyingFreq,'Varying Frequency',10)
df = appendForm(df,varyingFreq,'Varying Frequency',20)
df = appendForm(df,varyingFreq,'Varying Frequency',30)

df = appendForm(df,circle,'Circle',0.1)
df = appendForm(df,circle,'Circle',1)
df = appendForm(df,circle,'Circle',10)
df = appendForm(df,circle,'Circle',20)
df = appendForm(df,circle,'Circle',30)

df = appendForm(df,xShaped,'X',0.1)
df = appendForm(df,xShaped,'X',1)
df = appendForm(df,xShaped,'X',10)
df = appendForm(df,xShaped,'X',20)
df = appendForm(df,xShaped,'X',30)
df$Noise = df$Noise/10

ggplot(df, aes(x=x, y=y, colour=Noise)) +
  geom_point(alpha=0.2, size=1) + 
  facet_wrap(~ Form,scales="free_y", ncol=3) + 
  theme_bw() + 
  theme(legend.position = "bottom")

ggplot(df[df$Noise <=0.01,], aes(x=x, y=y)) +
  geom_point(colour=blues9) + 
  facet_wrap(~ Form,scales="free_y", ncol=3) + 
  theme_bw() + 
  theme(legend.position = "bottom")

