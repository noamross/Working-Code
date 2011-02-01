# Numerically integrating continuous-time functions always
#  takes an extra "library" of R functions: deSolve
# To get this library, go to the menu for
#  Packages & Data -> Packate Installer
#  then search for deSolve, select it,
#  and click "Install Selected"

#----------------------------------
# Once you've downloaded the deSolve libarary
#  via the Packages & Data menu,
#  you also have to load it each time
#  you start up R and plan to use it.
# You load it with this command:
library(deSolve) # loads the ODE numerical integration package

#----------------------------------
# functions

# First define the function you're going to numerically integrate
# In this case, the Lotka-Volterra competition model
#  all functions plugged into the numerical integration function 
#  have to have the same format:
#  fnName = function(t, n, parms) {...}
# See that inputs are first time t,
#  then state variable vector n,
#  then parameter values parms in a vector
#   where they're labeled by their names
# To create a vector of parameter values with corresponding names, 
#  you can use 
v=c(x=1, y=4, z=0) 
v
#  this creates a vector v
#   with the first entry (1) labeled as x
#   the second entry (4) labeled as y
#   and the third entry (0) labeled as z
# Then you can use the with(as.list(parms), {..})
#  to run any commands using those parameters, 
# For example, with the v vector above:
#  first type in 
print(x) #, you'll get an error, 
#  but if you do 
with(as.list(v), {print(x)}) #, it should work
# Note also that if you do 
c(a=1, b=c(2,3))
#  R automatically makes up names b1 and b2

# --------------
# in a script...
library(deSolve) # loads the ODE numerical integration package

# so let's make our function with our list of parameters
# logInt takes in time t, population size n, and the vector of parameters parms to numerically integrate the logistic function with lsoda
logInt = function(t, n, parms) {
	with(as.list(parms), { # extract parameters from parms vector
		dn = r*n*(K-n)/K # logistic dn/dt
		return(list(dn)) # return dn/dt as a list
		})
	}

# -----------------
# parameters
r = 0.5 # intrinsic growth rate
K = 100 # carrying capacity
parms = c(r=r, K=K) # named vector of parameters
tf = 20 # end time
times = 1:tf # vector of times to run over
n0 = c(n=2) # initial population size - having the n= label means that the output of lsoda will label the population size as n

# ----------------
# run model
# use the lsoda function - inputs are:
#  1. initial conditions
#  2. time vector to run over
#  3. function to integrate
#  4. parameter vector.
# Then convert the output to a "data frame" with as.data.frame
#   This lets you access pieces by labels as well as column/row numbers
out = as.data.frame(lsoda(n0, times, logInt, parms))

out$time # check out the output
out$n 

plot(out$time, out$n, type="l", xlab="Time", ylab="Population size") # plot the output


# Task: Lotka-Volterra competition
# dn1/dt = r1*n1*(1-n1/K1-a12*n2/K1)
# dn2/dt = r2*n2*(1-n2/K2-a21*n1/K2)
# parameter values:
# r = c(0.8, 0.6) # population growth rates
# K = c(600, 200) # carrying capacities
# a = 0.5*c(K[1]/K[2], K[2]/K[1]) # competition coefficients (this will be in a parameter space with coexistence)
# run for 20 time units, starting with two individuals in each population 
# plot populations over time (both species on same plot)