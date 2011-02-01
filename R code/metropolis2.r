##########################################################
# metropolis for estimating mu and sigma at same time
# the data
n <- 30
y <- rnorm( n , mean=7 , sd=0.5 )

# priors; these are uninformative
prior.mu <- function(theta) dnorm( theta , mean=7 , sd=1000 , log=TRUE )
prior.sigma <- function(theta) dgamma( theta , shape=0.001 , rate=0.001 , log=TRUE )

# initial guesses
k.mu <- mean(y)
k.sigma <- sd(y)

num.samples <- 20000
sample.mu <- rep(0,num.samples)
sample.sigma <- rep(0,num.samples)
step.mu <- 1/10
step.sigma <- 1/10
for ( i in 1:num.samples ) {
    # record history
    sample.mu[i] <- k.mu
    sample.sigma[i] <- k.sigma

    # propose jump
    prop.mu <- k.mu + rnorm( 1 , mean=0 , sd=step.mu )
    prop.sigma <- k.sigma + rnorm( 1 , mean=0 , sd=step.sigma )
    prop.sigma <- max( 0.001 , prop.sigma ) # must ensure positive variance
    
    # compute prob of accepting proposed mu
    pr.prop <- sum( dnorm( y , mean=prop.mu , sd=k.sigma , log=TRUE ) ) + prior.mu(prop.mu) + prior.sigma(k.sigma)
    pr.here <- sum( dnorm( y , mean=k.mu , sd=k.sigma , log=TRUE ) ) + prior.mu(k.mu) + prior.sigma(k.sigma)
    pr.accept <- exp( pr.prop - pr.here )
    k.mu <- ifelse( runif(1) < pr.accept , prop.mu , k.mu )
    
    # compute prob of accepting proposed sigma
    pr.prop <- sum( dnorm( y , mean=k.mu , sd=prop.sigma , log=TRUE ) ) + prior.mu(k.mu) + prior.sigma(prop.sigma)
    pr.accept <- exp( pr.prop - pr.here )
    k.sigma <- ifelse( runif(1) < pr.accept , prop.sigma , k.sigma )
    
}

# plot chains
par(mfrow=c(2,2))
plot( sample.mu , col="slateblue" , main="samples of mu" , type="l" )
plot( sample.sigma , col="slateblue" , main="samples of sigma" , type="l" )

# remove the "burn in" samples at the start
sample.mu <- sample.mu[-(1:5000)]
sample.sigma <- sample.sigma[-(1:5000)]

# plot posterior densities
plot( density(sample.mu) , col="slateblue" , lwd=2 , main="posterior mu" )
plot( density(sample.sigma) , col="slateblue" , lwd=2 , main="posterior sigma" )

# illustrate checking of autocorrelation
par(mfrow=c(3,1))
acf( sample.mu )
acf( sample.sigma )
ccf( sample.mu , sample.sigma ) # cross-correlation