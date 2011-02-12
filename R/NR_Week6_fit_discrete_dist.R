#ECL 298 HW Week 6: Fitting Negative Binomial
#Noam Ross, February 11, 2011

fitdiscdist <- function(data, distpick, guesspars) {
# This function fits a discrete probability distribution to a distribution of 
# data, using a maximum-likelihood method.
# Arguments:
#   data: a vector of integers representing the data to be fit
#   distpick: the distribution to use to fit, e.g. dpois, dnbinom
#   guesspars: a set of initial guesses for the distribution parameters, in the form
#     of a vector with names
  optim.out = optim(guesspars, negloglike, method="BFGS",data=data, distpick=distpick) #run optimize function on negloglike function,
  AIC = 2*length(guesspars) + 2*optim.out$value #calculate information criterion
  out = list(pars=optim.out$par, likelihood=(-optim.out$value), AIC=AIC) #package outputs as list
  return(out) #output the list
}

negloglike <- function(pars, data, distpick) {
# This function calculates the negative log likelihood of data under a given function and parameters
# Arguments:
#   data: a vector of integers representing the data to be fit
#   distpick: the distribution to use to fit, e.g. dpois, dnbinom
#   pars: a set of initial guesses for the distribution parameters, in the form
#     of a list with names
  callparms = c(list(x=data,log=TRUE), as.list(pars))  #create a unified list of arguments
  loglikelihoods = do.call(distpick, callparms) #call the distribution function with those arguments
  out = -sum(loglikelihoods)   #sum the loglikelihoods
  return(out) #output the optimizing value

}

#Work session to fit data
data=c(33,rep(0,25),rep(1,3),rep(2,2),rep(2,3),8,10,rep(0,19),12) #create the data to fit
nbprobguess = 1-(mean(data)/var(data)) # set initial guess for probability for neg-binomfit
nbsizeguess = mean(data)*(1-nbprobguess)/nbprobguess #set initial guess for size for neg-binomfit
nbiguess = c(prob=nbprobguess, size=nbsizeguess) #list of initial guesses for fit of neg-binomial distribution.  These are calulated 

nbinomrun = fitdiscdist(data,dnbinom,nbiguess) #fit the data to a negative binomial distribution
poisrun = fitdiscdist(data,dpois,mean(data)) #fit the data to a poisson distribution

hist(data,n=24,col="red",freq=FALSE, main="Poisson and Negative Binomial Fits to SARS Data",
     xlab="Number of Infections",ylab="Proportion of Cases")
pys = dpois(0:33,lambda=poisrun$par)
dnys = dnbinom(0:33,size=nbinomrun$par['size'],prob=nbinomrun$par['prob'])
xs = 0:33
lines(xs,pys,col="green",lwd=2)
lines(xs,dnys,col="blue",lwd=2)
legend(15,0.6,c(paste("Poisson (AIC = ",round(poisrun$AIC,2),")"),
       paste("Neg-Binom (AIC = ",round(nbinomrun$AIC,2),")")), col=c("green","blue"), 
       lty=c(1,1))
       