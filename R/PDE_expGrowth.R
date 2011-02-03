# Modified from the R help file on ode.band:
  # The Aphid model from Soetaert and Herman, 2008. 
  # A practical guide to ecological modelling. 
  # Using R as a simulation platform. Springer.
  
# 1-D diffusion model with exponential growth
  
# integrator to use (ode.band) is in deSolve library
library(deSolve)

#==================#
# Model equations  #
#==================#
# Note that this is the same structure as put into lsoda and ode:
#   function inputs are (1) time, (2) population size, and (3) parameter vector
# To take partial n with respect to x (dn/dx), use diff(n)/delx
#   and have to put boundary conditions on either side of n
# Function integrating:
#   dn/dt=D*d^2n/dx^2+r*N
#   diffusion plus exponential growth

  Aphid = function(t,APHIDS,parameters)
   {
   	with(as.list(parameters), { # take out parameter values from list
      	# absorbing boundary case (zeros on outside of n put into dn/dx)
      	dAPHIDS = D*diff(diff(c(0,APHIDS,0))/deltax)/delx+APHIDS*r
      	
      	# reflecting boundary case (n[1] and n[end] on outside of n put into dn/dz)
      	#dAPHIDS = D*diff(diff(c(APHIDS[1],APHIDS,APHIDS[numboxes]))/deltax)/delx+APHIDS*r
	    
	    # the output
	    list(dAPHIDS)
      })
    }
    
#==================#
# Model application#
#==================#
# the model parameters:
D = 0.3 # m2/day diffusion rate
r = 0.01 # /day net growth rate
delx = 1 # m thickness of boxes
numboxes = 60 # length of habitat modeling over
deltax = c(delx/2,rep(delx,numboxes-1),delx/2) # delta-x's for calculating d2n/dx2
  
# create parameters vector
parms = c(D=D, r=r, delx=delx, numboxes=numboxes, deltax=deltax)
  
# distance of boxes on plant, m, 1 m intervals
Distance = seq(from=delx/2,by=delx,length.out=numboxes) # labeling ea. box by its midpoint
  
# Initial conditions:  # ind/m2   
# aphids present only on two central boxes 
APHIDS = rep(0,times=numboxes)      
APHIDS[30:31] = 1
state = c(APHIDS=APHIDS) # initialise state variables 
                    
# RUNNING the model:   
times = seq(0,200,by=1)   # output wanted at these time intervals           

# out.band is numerical integrator
# inputs are initial conditions (state), time vector want output at (times), function integrating (Aphid), parameters vector (parms), and number of species (nspec)
out = ode.band(state,times,Aphid,parms,nspec=1)  
  
#==================#
# Plotting output  #
#==================#
# the data in 'out' consist of: 1st col times, 2-end: the density (time along rows, space along columns)
# select the density data
DENSITY = out[,2:(numboxes+1)]
# countour plot: time along x axis, space along y axis, color represents population density
filled.contour(x=times,y=Distance,DENSITY,color= topo.colors,
 xlab="time, days", ylab= "Distance on plant, m",
 main="Aphid density on a row of plants")


