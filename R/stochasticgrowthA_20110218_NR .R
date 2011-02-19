#ECL 298-026 Class Session February 15, 2011 

#demographic stochasticity in acorn woodepeckers

#N(t) - number of female adults in year (t)
#N(t+1) = N(t)*as(t+1) + f(t+1)*js(t+1)*(0.5)*N(t) = (as(t+1) + js(t+1)*f(t+1)*0.5)*N(t)

#Data from Sebastian (stacey and tapper)
as=c(0.53,0.68,0.71,0.38,0.54,0.69,0.66,0.49,0.61)  #annual adult surviorship (female)
js=c(0.56, 0.64, 0.30, 0.40,0, 0.38, 0.18, 0.25, 0.44)  #annual juvenile surviorship
f=c(3.38 ,1.27 ,2.77 ,2.17 ,0.05 ,4.00 ,2.37 ,0.50 ,1.60 ) #annual fecundity (male and female)

lambda = as + js*f/2 #create demographic multiplier

#Examine correlation structure
#cor(lambda[-1], lambda[-9]) #calculate correlation coefficent of lambda[t] against lambda[t+1]
#acf(lambda) #shows autocorrelation by different lag times

#parameters
tf = 100 #set number of time steps
runs = 5000 #set number of repeats
N0 = 100 #set initial population level
nestingsites = 26 #max number of nexting pairs
exthreshold = 1 #quasi-extinction threshold

#set up simulation
N = matrix(0, tf, runs) #create data space
N[1,] = rep(N0,runs) #set inital conditions of all runs to N0

for(i in 2:tf) {  #run simulation
    N[i,] = sample(lambda, runs, replace=TRUE)*N[i-1,]
#  temp = pmin(sample(lambda, runs, replace=TRUE)*N[i-1,], nestingsites)
#  temp[which(temp<exthreshold)] = 0
#  N[i,] = temp
# N[i,] = pmin(sample(lambda, runs, replace=TRUE)*N[i-1,], nestingsites) #adds ceiling to population

 
}
#par(mfrow=c(1,2))
hist(log(N[tf,]), freq=FALSE, col="red", xlab="Log of Final Population",   #create histogram of final values
     ylab="Frequency", main=paste("Final Populations from ",runs," runs", sep="")) 
gaussxs = seq(min(log(N[tf,])), max(log(N[tf,])), length.out=100)  #calculate gaussian from output data
gaussysmeasured = dnorm(gaussxs, mean(log(N[tf,])), sd(log(N[tf,])))
gaussyspredicted = dnorm(gaussxs, tf*mean(log(lambda))+log(N0), sqrt(var(log(lambda))*tf))
lines(gaussxs,gaussysmeasured, col="blue")
lines(gaussxs,gaussyspredicted, col="green")
legend("topleft",c("Measured", "Calculated"), lty=c(1,1), col=c("blue","green"))

#matplot(N,type="l")
#Assignment - implement code, plot histogram of log(N[tf,])
#Superimpose appropriate gaussian curve (from log of lambda, see reading).  
#include density dependence.
#Plot histogram of extinction times. Might use sign(), or rowsums()
