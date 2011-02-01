##########################################################
# show updating process iteratively
# just paste all of this code into R to execute it

# generate data
n <- 20
y <- rnorm( n , mean=7 , sd=0.5 )
# assign prior and compute posterior as we add each y value to observations
prior.mu <- 3
k.sigma <- sd(y)
prior.sigma <- 10000
par(mfcol=c(3,1))
for ( i in 1:length(y) ) {
    # plot prior
    curve( dnorm(x,mean=prior.mu,sd=prior.sigma) ,from=2,to=8,n=1000,col="slateblue",lwd=2 , main="prior")
    # plot y values sampled so far
    plot( y[1:i] , cex=2 , xlim=c(1,length(y)) )
    points( i , y[i] , col="red" , pch=16 , cex=2 )
    lines( c(0,length(y)) , c(mean(y[1:i]),mean(y[1:i])) , lty=2 , col="slateblue" )
    # plot posterior so far
    yy <- y[1:i]
    posterior.mu <- function(x) dnorm( x , mean= ( prior.mu/prior.sigma^2 + sum(yy)/k.sigma^2 ) / (1/prior.sigma^2 + length(yy)/k.sigma^2) , sd=sqrt(1/(1/prior.sigma^2 + length(yy)/k.sigma^2)) )
    curve( posterior.mu(x) ,from=2,to=8,n=1000,col="slateblue",lwd=2,main="posterior")
    lines( c(mean(y[1:i]),mean(y[1:i])) , c(0,1000) , lty=2 , col="slateblue" )
    # wait for input to redraw
    readline()
}