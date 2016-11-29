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