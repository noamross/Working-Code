#Maximum Likelihood Estimation of Poisson function against Data

data=c(33,rep(0,25),rep(1,3),rep(2,2),rep(2,3),8,10,rep(0,19),12)

negpoisloglike = function(lambda, data) {
#Computes the negative log-likelihood of the inputted data under a poisson
#distribution
#data is a vector of integers
#lambda is the mean value of the poission distribution
  loglikelihoods = dpois(data,lambda,log=TRUE)  #calculate log likelihoods for each value
  out = -sum(loglikelihoods)  #calculate the negative sum of log likelihoods
  return(out)
}

pois.fit = optim(mean(data),negpoisloglike,method="BFGS",data=data)  #find optimal fit

hist(data,n=24,col="red",freq=FALSE)
ys = dpois(0:33,lambda=pois.fit$par)
xs = 0:33
points(xs,ys,type="b",lwd=4)


Task - repeat with negative binomial.  
