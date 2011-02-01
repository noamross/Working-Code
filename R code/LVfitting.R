library(deSolve)

LotVolt = function(t,n,parms) {
	with(parms,{
		dn = r*n*(1-LV%*%n)
		return(list(dn))
		})
	}
	
LVmse = function(parms) {
	out = as.data.frame(lsoda(n0, times, LotVolt, parms)) # run ode
	mse = mean((out[,2:3]-nTrue)^2) # calculate mean squared error between simulated and original data
	return(mse) # return mean squared error
	}

nTrue = as.matrix(read.table("paramecium2.txt"))
s = 2 #number of species
r = c(0.7816, 0.6283) #vector of species intrinsic growths
K = c(559.686, 202.4931) #vector of species carrying capacities
n0 = c(n=nTrue[1,])
alpha0 = diag(1,s,s)  #alpha is matrix of species competition coefficients, ones on diagonal
a12guess = 0.5
a21guess = 0.5
alpha0[2,1] = a12guess
alpha0[1,2] = a21guess
tf = dim(nTrue)[1] # time
times = 1:tf # vector of times to run over
LV0 = diag(1/K)%*%alpha #modified lotka-volterra matrix is species competitions divided by carrying capacities

optimOut = optim(LV0, LVmse) # give optim initial guess and function
parms = optimOut$par # extract parameters
nSim = as.data.frame(lsoda(n0,times,logInt,parms)) # run ode to get simulated population