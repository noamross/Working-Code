#Noam Ross, ECL298 Week 2 Exercise, 1/11/11
#Create a bifurcation diagram of the Ricker model

rm(list=ls()) #clear workspace

#define parameters
Rlow = 1.5 #set of low R value
Rhigh = 3.6 #set high R value
Rsteps = 250 #set number of steps in R
K = 100  #carrying capacity
Tmax = 500 #length of time to run model
Steadytime = 100 #number of time steps from end to plot at steady state
n1 = 1 #starting condition

Ricker <- function(n,r,k) { #define Ricker n(t+1) for n with r and k parameters
	ntplus1 = n*exp(r*(1-(n/k)))
	return(ntplus1)
	}

R = seq(Rlow,Rhigh,length=Rsteps)
n = matrix(0,length(R),Tmax) #define output matrix
n[,1] = n1 #set first column of matrix as starting conditions
T = 1:Tmax #create time vector

for(t in 2:Tmax) { 				#run Ricker function to Tmax, filling n
	n[,t] = Ricker(n[,t-1],R,K)
	}
	
steadystate = n[,(Tmax-Steadytime+1):Tmax]  #create matrix with data from steady-state period
matplot(R,steadystate, xlab="R value", ylab = "Steady State Population",col="blue", pch=16, cex=.4, main="Bifurfaction Diagram of Ricker Model") #plot bifurcation diagram
mtext(paste("Time steps ", Tmax-Steadytime, "-", Tmax," of ", Tmax), side=3) #annotate with time step info
