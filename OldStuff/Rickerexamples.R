#Noam Ross, ECL298 Exercise Week 2 
#Plot Ricker Model Behavior at multiple values of R

rm(list=ls()) #clear workspace

#define parameters
R = c(1.5, 2.3, 2.6, 3.0) #set of R values to plot
K = 100  #carrying capacity
Tmax = 100 #length of time to run model
n1 = c(1, 1, 1, 1) #starting conditions

Ricker <- function(n,r,k) { #define Ricker n(t+1) for n with r and k parameters
	ntplus1 = n*exp(r*(1-(n/k)))
	return(ntplus1)
	}

n = matrix(0,length(R),Tmax) #define output matrix
n[,1] = n1 #set first column of matrix as starting conditions
T = 1:Tmax #create time vector

for(t in 2:Tmax) { 				#run Ricker function to Tmax, filling n
	n[,t] = Ricker(n[,t-1],R,K)
	}
	
par(mfrow=c(2,2)) # create a 2x2 graphics space that will be filled by rows

plot(T,n[1,],type="l",xlab = "Time Step", ylab="Population")  #plot all four plots
plot(T,n[2,],type="l",xlab = "Time Step", ylab="Population")
plot(T,n[3,],type="l",xlab = "Time Step", ylab="Population")
plot(T,n[4,],type="l",xlab = "Time Step", ylab="Population")

