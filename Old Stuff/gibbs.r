##########################################################
# simple gibbs sampler
# estimates both mu and sigma
# the data
n <- 30
y <- rnorm( n , mean=7 , sd=1/2 )

# priors; these are uninformative
mu0 <- 7
sigma0 <- 1000
shape0 <- 0.001
rate0 <- 0.001

# initial guesses
k.mu <- mean(y)
k.sigma <- sd(y)

num.samples <- 20000
sample.mu <- rep(0,num.samples)
sample.sigma <- rep(0,num.samples)
for ( i in 1:num.samples ) {
    # record history
    sample.mu[i] <- k.mu
    sample.sigma[i] <- k.sigma
    
    # sample parameters
    # sample sigma, conditioned on mu
    k.tau <- rgamma( 1 , shape=shape0 + length(y)/2 , rate=rate0 + sum( (y-k.mu)^2 )/2 )
    # sample mu, conditioned on sigma
    k.sigma <- sqrt(1/k.tau)
    k.mu <- rnorm( 1 , mean= ( mu0/sigma0^2 + sum(y)/k.sigma^2 ) / (1/sigma0^2 + length(y)/k.sigma^2) , sd=sqrt(1/(1/sigma0^2 + length(y)/k.sigma^2)) )
    
}

# plot chains
par(mfrow=c(2,2))
plot( sample.mu , col="slateblue" , main="samples of mu" , type="l" )
plot( sample.sigma , col="slateblue" , main="samples of sigma" , type="l" )

# remove the "burn in" samples at the start
sample.mu <- sample.mu[-(1:1000)]
sample.sigma <- sample.sigma[-(1:1000)]

# plot posterior densities
plot( density(sample.mu) , col="slateblue" , lwd=2 , main="posterior mu" )
plot( density(sample.sigma) , col="slateblue" , lwd=2 , main="posterior sigma" )

# checking of autocorrelation
par(mfrow=c(3,1))
acf( sample.mu )
acf( sample.sigma )
ccf( sample.mu , sample.sigma )

# another way to look at gibbs sampler: bivariate random walk
plot( sample.mu ~ sample.sigma , cex=0.2 , col="slateblue" )

# animate the random walk
plot( sample.mu[1] ~ sample.sigma[1] , col="slateblue" , ylim=c(min(sample.mu),max(sample.mu)) , xlim=c(min(sample.sigma),max(sample.sigma)) )
for ( i in 1:length(sample.mu) ) {
    lines( c(sample.sigma[i],sample.sigma[i+1]) , c(sample.mu[i],sample.mu[i]) , col="slateblue" )
    lines( c(sample.sigma[i+1],sample.sigma[i+1]) , c(sample.mu[i],sample.mu[i+1]) , col="slateblue" )
}