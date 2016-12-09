#' A function to simulate power while varying the amount of noise.
#' 
#' @param functions A list of functional forms to be evaluated
#' @param distribution The source distribtion, uniform by default
#' @param nsim The number of datasets used to estimate the rejection reject regions for an alternative with level alpha
#' @param numNoise Number of different noise levels used
#' @param nl The total level of noise
#' @param n Number of data points per simulation
powerVersusNoise <- function(#measures,
                             functions, 
                             distribution=runif, 
                             nsim = 500, 
                             alpha=0.05, 
                             nl=3, 
                             numNoise=30, 
                             n=320, ...) {
  # Vectors holding the null and alternative "associations" for each of the nsim null datasets at a given noise level
  val.cor=val.dcor=val.mine=val.A=val.spear=rep(NA,nsim)  
  val.cor2=val.dcor2=val.mine2=val.A2=val.spear2= rep(NA,nsim)        
  # h0 <- matrix(nsim, length(measures))
  # h1 <- matrix(nsim, length(measures))
  
  # Arrays holding the estimated power for each of the association types, 
  #for each functional with each noise level
  power.cor=power.dcor=power.mine=power.A=power.spear= array(NA, c(length(functions),numNoise))                
  cat("dim power.cor = ", dim(power.cor), "\n")
  #Loop through the noise level and functional form; 
  #each time we estimate a null distribution based on the marginals of the data, 
  #and then use that null distribution to estimate power
  for(l in 1:numNoise) {
    for(typ in 1:length(functions)) {
      ## This next loop simulates data under the null with the correct marginals (x is uniform, and y is a function of a uniform with gaussian noise)
      
      cat(sprintf("Noise Null %s; Type %s\n",  l, typ))
      for(ii in 1:nsim)
      {
        #sample from the given distribution
        x <- distribution(n,...)
        
        #evaluate the function 
        y <- functions[[typ]](x, nl, l, numNoise, n)
        # We resimulate x so that we have the null scenario
        x <- distribution(n,...)
        # # calculate the association
        #ho[ii,] <- sapply(measures, function(f) f(x, y))
        val.cor[ii]=(cor(x,y))^2
        
        val.dcor[ii]=dcor(x,y)
        val.mine[ii]=mine(x,y)$MIC
        val.A[ii]=ma(data.frame(x,y))$A
        val.spear[ii]=(cor(x,y,method='spearman'))^2
      }
      # remove the mic trials which glitch
      # val.mine <- val.mine[which(!is.na(val.mine))]                 
      
      # Next we calculate our rejection cutoffs
      #cuts <- apply(h0, 2, quantile, probs = 1 - alpha)
      cut.cor <- quantile(val.cor,1-alpha)
      cut.dcor <- quantile(val.dcor,1-alpha)
      cut.mine <- quantile(val.mine,1-alpha)
      cut.A <- quantile(val.A,1-alpha)
      cut.spear <- quantile(val.spear,1-alpha)
      
      ## Next we simulate the data again, this time under the alternative
      cat(sprintf("Noise Alt %s; Type %s;\n",  l, typ))
      for(ii in 1:nsim)
      {
        #the dependent data
        x <- distribution(n,...)
        y <- functions[[typ]](x, nl, l, numNoise, n)
        
        # calculate the associations
        #h1[ii] <- sapply(measures, function(f) f(x, y))
        val.cor2[ii]=(cor(x,y))^2
        val.dcor2[ii]=dcor(x,y)
        val.mine2[ii]=mine(x,y)$MIC
        val.A2[ii]=ma(data.frame(x,y))$A
        val.spear2[ii]=(cor(x,y,method='spearman'))^2
      }
      
      
      ## Now we estimate the power as the number of alternative statistics exceeding our estimated cutoffs
      
      power.cor[typ,l] <- sum(val.cor2 > cut.cor)/nsim
      power.dcor[typ,l] <- sum(val.dcor2 > cut.dcor)/nsim
      power.mine[typ,l] <- notNA.greater(val.mine2, cut.mine)
      power.A[typ,l] <- sum(val.A2 > cut.A)/nsim
      power.spear[typ,l] <- sum(val.spear2 > cut.spear)/nsim
    }
  }
  
  #assemble the results
  powerdf <- data.frame(power.cor)
  powerdf <- t(powerdf)
  colnames(powerdf) <- names(functions)
  
  results <- melt(powerdf)
  results <- cbind(results,rep('Pearson',numNoise))
  results <- cbind(results,(1:numNoise)/10)
  colnames(results) <- c('var','Form','Power','Statistic','Noise')
  results <- combineNoiseResults(results,power.dcor,'dcor',numNoise)
  results <- combineNoiseResults(results,power.mine,'MIC',numNoise)
  results <- combineNoiseResults(results,power.A,'A',numNoise)
  results <- combineNoiseResults(results,power.spear,'Spearman',numNoise)
  
  #reorder based on the functional form
  results$Form <- reorder(results$Form, new.order=names(functions))
  results <- results[ order(results$Form), ]
  
  results
}


#a utility function to build up the noise results
combineNoiseResults <- function(df, x, stat, numNoise)
{
  tf <- data.frame(x)
  tf <- t(tf)
  colnames(tf) <- names(functions)
  tf <- melt(tf)
  tf <- cbind(tf,rep(stat,numNoise))
  tf <- cbind(tf,(1:numNoise)/10)
  colnames(tf) <- c('var','Form','Power','Statistic','Noise')
  rbind(df,tf)
}
