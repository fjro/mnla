library('ProjectTemplate')
load.project()

#TODO: simulate performance of penalised spline and HHG


#add the desired functions to a list for evaluation during simmulations
functions <- list("linear"=linear,"Quadratic"=quadratic,"Cubic"=cubic, "Fourth Root" = qroot, "Exponential" = exponential2, 
                 "Natural Log" = logE, "Sigmoid" = sigmoid, "Step"=step, "Spike" = spike,
                 "Sine: Low"= sinLow, "Sine: High" = sinHigh, "Linear+Periodic" = linearPeriodic, "Varying Frequency" = varyingFreq,
                 "Circle" = circle, "X" = xShaped)

functions <- list("linear"=linear,"Quadratic"=quadratic)

#run the noise simulation using a beta(2,5) and plot the results
set.seed(1)
noiseResults <- powerVersusNoise(functions, rbeta, shape1=2, shape2=5)
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

