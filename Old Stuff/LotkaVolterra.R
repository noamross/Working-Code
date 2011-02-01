

#parameters
s = 2 #number of species
r = c(0.8, 0.6) #vector of species intrinsic growths
K = c(600, 200) #vector of species carrying capacities
n0 = c(1,1)
alpha = diag(1,s,s)  #alpha is matrix of species competition coefficients, ones on diagonal
alpha_21 = 0.5*K[1]/K[2]
alpha_12 = 0.5*K[2]/K[1]
alpha[2,1] = alpha_12
alpha[1,2] = alpha_21
LV = diag(1/K)%*%alpha #modified lotka-volterra matrix is species competitions divided by carrying capacities
tf=100 #number of time steps
times = 1:tf
parms = list(r=r,K=K,LV=LV)

LotVolt = function(t,n,parms) {
	with(parms,{
		dn = r*n*(1-LV%*%n)
		return(list(dn))
		})
	}
	
nout = as.data.frame(lsoda(n0,times,LotVolt,parms))
plot(nout[,1],nout[,2],type="l")
lines(nout[,1],nout[,3])