##########################################################
# simple metropolis algorithm for estimating normal mean
# we'll extend it to estimating variance next
# the data
n <- 30
y <- rnorm( n , mean=7 , sd=0.5 )

# priors; these are uninformative
prior.mu <- function(theta) dnorm( theta , mean=7 , sd=1000 , log=TRUE )
# informative prior -- uncomment the line below to use informative prior
#prior.mu <- function(theta) dnorm( theta , mean=8 , sd=1/100 , log=TRUE )

# initial guesses
k.mu <- mean(y)
k.sigma <- sd(y)

num.samples <- 20000
sample.mu <- rep(0,num.samples)
step <- 1/10
for ( i in 1:num.samples ) {
    # record history
    sample.mu[i] <- k.mu

    # propose jump
    prop.mu <- k.mu + rnorm( 1 , mean=0 , sd=step)
    
    # compute prob of accepting
    pr.prop <- sum( dnorm( y , mean=prop.mu , sd=k.sigma , log=TRUE ) ) + prior.mu(prop.mu)
    pr.here <- sum( dnorm( y , mean=k.mu , sd=k.sigma , log=TRUE ) ) + prior.mu(k.mu)
    pr.accept <- exp( pr.prop - pr.here )
    
    # jump!
    k.mu <- ifelse( runif(1) < pr.accept , prop.mu , k.mu )
}

# plot the sampling chain
par(mfrow=c(3,1))
plot( sample.mu , type="l" , main="mu" , col="slateblue" )
lines( c(0,num.samples) , c(mean( sample.mu),mean( sample.mu)) , col="red" )

# remove the "burn in" samples at the start
sample.mu <- sample.mu[-(1:1000)]

# plot estimated density
plot( density(sample.mu) , main="mu posterior" , lwd=2 , col="slateblue" )

# overlay "true" posterior, using bayes' directly
prior.mean <- 7
prior.sd <- 1000
#prior.mean <- 8
#prior.sd <- 1/100
posterior.mu <- function(x) dnorm( x , mean= ( prior.mean/prior.sd^2 + sum(y)/k.sigma^2 ) / (1/prior.sd^2 + length(y)/k.sigma^2) , sd=sqrt(1/(1/prior.sd^2 + length(y)/k.sigma^2)) )
curve( posterior.mu(x) ,from=2,to=9,n=2000,col="orange",lwd=2,add=TRUE)

# cut bayesian credible intervals at 95% level (similar to confidence intervals)
quantile( sample.mu , probs=c(0.025,0.975) )
# plot credible interval on posterior
ci <- quantile( sample.mu , probs=c(0.025,0.975) )
lines( c(ci[1],ci[1]) , c(0,100) , col="red" , lty=2 )
lines( c(ci[2],ci[2]) , c(0,100) , col="red" , lty=2 )

# check autocorrelation
acf( sample.mu )