# tpvn <- function( 
#   nsim = 500, 
#   alpha=0.05, 
#   noise=3, 
#   noiseLevel=10,
#   numNoise=30, 
#   n=320, ...) {
#   # Vectors holding the null and alternative "associations" for each of the nsim null datasets at a given noise level
#   val.cor=val.dcor=val.A=val.spear=val.mine=rep(NA,nsim)  
#   val.cor2=val.dcor2=val.A2=val.spear2=val.mine2= rep(NA,nsim)        
#   # h0 <- matrix(nsim, length(measures))
#   # h1 <- matrix(nsim, length(measures))
#   
#   # Arrays holding the estimated power for each of the association types, 
#   #for each functional with each noise level
#   power.cor=power.dcor=power.mine=power.A=power.spear = NA                
# 
#       for(ii in 1:nsim)
#       {
#         #sample from the given distribution
#         #set.seed(1)
#         x <- runif(n)
#         
#         #evaluate the function 
#         y <- linearCpp(x, noise, noiseLevel, numNoise, n)
#         # We resimulate x so that we have the null scenario
#        # set.seed(1)
#         x <- runif(n)
#         # # calculate the association
#         #ho[ii,] <- sapply(measures, function(f) f(x, y))
#         val.cor[ii]=(cor(x,y))^2
#         val.mine[ii]=mine(x,y)$MIC
#         val.dcor[ii]=dcor(x,y)
#         val.A[ii]=ma(data.frame(x,y))$A
#         val.spear[ii]=(cor(x,y,method='spearman'))^2
#       }
#       cat("\nmean h0: ", mean(val.cor), mean(val.spear), mean(val.dcor), mean(val.mine), mean(val.A), sep=", ")
#       # remove the mic trials which glitch
#       # val.mine <- val.mine[which(!is.na(val.mine))]                 
#       
#       # Next we calculate our rejection cutoffs
#       #cuts <- apply(h0, 2, quantile, probs = 1 - alpha)
#       cut.cor <- quantile(val.cor,1-alpha)
#       cut.mine <- quantile(val.mine,1-alpha)
#       cut.dcor <- quantile(val.dcor,1-alpha)
#       cut.A <- quantile(val.A,1-alpha)
#       cut.spear <- quantile(val.spear,1-alpha)
#       cat("\ncuts = ", cut.cor, cut.spear, cut.dcor, cut.A, sep=", ")
#       ## Next we simulate the data again, this time under the alternative
#       for(ii in 1:nsim)
#       {
#         #the dependent data
#         #set.seed(1)
#         x <- runif(n)
#         y <- linearCpp(x, noise, noiseLevel, numNoise, n)
#         
#         # calculate the associations
#         #h1[ii] <- sapply(measures, function(f) f(x, y))
#         val.cor2[ii]=(cor(x,y))^2
#         val.mine2[ii]=mine(x,y)$MIC
#         val.dcor2[ii]=dcor(x,y)
#         val.A2[ii]=ma(data.frame(x,y))$A
#         val.spear2[ii]=(cor(x,y,method='spearman'))^2
#       }
#       cat("\nmean hA: ", mean(val.cor2), mean(val.spear2), mean(val.dcor2), mean(val.mine2), mean(val.A2), "\n", sep=", ")
#       
#       ## Now we estimate the power as the number of alternative statistics exceeding our estimated cutoffs
#       
#       power.cor <- sum(val.cor2 > cut.cor)/nsim
#       power.spear <- sum(val.spear2 > cut.spear)/nsim
#       power.dcor <- sum(val.dcor2 > cut.dcor)/nsim
#       power.A <- sum(val.A2 > cut.A)/nsim
#       power.mine <- sum(val.mine2 > cut.mine)/nsim
#       
#   #}
#   c(power.cor, power.spear, power.dcor, power.mine, power.A)
# }
