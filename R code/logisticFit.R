# class 4: data-fitting to a continuous-time model
# here we'll fit the paramecium data to a logistic model, then you'll fit the competition paramters

# make sure the library is loaded
library(deSolve) # loads the ODE numerical integration package

# the approach: minimize square deviations
# run the logistic model under a given r and K, find the squared deviations from the data
# use optim to find the least squared deviation

# so we need the logistic model ready to run with lsoda:
logInt = function(t, n, parms) {
	with(as.list(parms), { # extract parameters from parms vector
		dn = r*n*(K-n)/K # logistic dn/dt
		return(list(dn)) # return dn/dt as a list
		})
	}

# plus a function to find the square deviations
logIntMin = function(parms) {
	out = as.data.frame(lsoda(n0, times, logInt, parms)) # run ode
	mse = mean((out$n-nTrue)^2) # calculate mean squared error between simulated and original data
	return(mse) # return mean squared error
	}

# parameters and data
nTrue = as.matrix(read.table("paramecium1.txt")) # actual data
n0 = c(n=nTrue[1]) # initial population size - us first data point, make sure to label
tf = length(nTrue) # time
times = 1:tf # vector of times to run over
rguess = 1 # guess for growth rate
Kguess = 500 # guess for carrying capacity
parms0 = c(r=rguess, K=Kguess)

# find fit and run simulations
optimOut = optim(parms0, logIntMin) # give optim initial guess and function
parms = optimOut$par # extract parameters
nSim = as.data.frame(lsoda(n0,times,logInt,parms)) # run ode to get simulated population

# plot results - actual and simulated data
plot(times, nTrue, type="p", xlab="Time", ylab="Abundance")
lines(nSim$time, nSim$n)

# task: fitting paramecium data to Lotka-Volterra competition model
# you get r's and K's - this is in the "parameciumrk.txt" file - laoding this under the default for read.table will lead to column names V1, V2, etc. so the evnetual labels for the r's and K's will be r.V1, r.V2, etc.; same for n's
# you'll have to find the alpha's to the data in the "paramecium2.txt" file
# note that this means you have to reconstruct the parameters vector within optim - start with parmsrk=r and K, then do parms = c(parmsrk, a=a) to add a's within minimizing function
