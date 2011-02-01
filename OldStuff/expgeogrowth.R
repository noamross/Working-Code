# our goal - plot exponential vs. geometric growth
rm(list=ls()) #clear workspace

#define exponential growth function
#expGrowth calcultes population size n
# given intrinsic growth rate r
# time t
#and initital pop size n0
expGrowth <- function(r, t, n0) {
	n <- n0*exp(r*t) #calculate exponential growth
	return(n) # return n at each time t
	}
#define geomentric growth function
#geoGrwoth calculates pop size N
#given intrinisic growth rate R
#time t
#and initial pop size N0
geoGrowth <- function(R, t, N0) {
	N <- N0*(R^t) #calculate geometric growth
	return(N) #return N at each time t
	}
#define parameters
r <- 2 # growth rate
deltat <- 0.1 # time steps
t <- seq(0,1, by=deltat) # time series
n0 <- 1 #initial poplation size
#run both functions
nexp <- expGrowth(r,t,n0) #exponential growth
ngeo <- geoGrowth(1+deltat*r,t,n0) #geometric growth

#plot both functions
plot(t,nexp, type="l", col="blue", xlab="Time", ylab="Population size") #Set up plot and exponential curve
lines(t, ngeo, type="l", col="red") #add geometric curve
legend("topleft", legend=c("Exponential", "Geometric"), lty=c(1,1), col=c("blue", "red"))