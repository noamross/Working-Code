#

rm(list=ls()) #clear workspace

#set parameters
lambda = 20 #population growth coefficient
a = 1 #intraspecific competition term
d = 0.04 #dispersal coefficient
p = 2 #number of patches
#N0 = runif(p)*10 #initial conditions
tf = 500 #number of time steps
N0s = seq(0,lambda/exp(1),length.out=400) #vector of initial conditions to test


#create dispersal matrix
D = matrix(d/(p-1),p,p) #create disperal matrix with uniform dispersal
diag(D) = 1-d

N = matrix(0,tf,p) #create variable space for individual run outputs
final.Ns = array(0, dim = c(rep(length(N0s), p), p))

ricker.growth = function(N, lambda, a) { #density dependent growth function
	N2 = lambda*N*exp(-a*N)
	return(N2)
	}


for(k in 1:length(N0s)) {
  for(j in 1:length(N0s)) {
    N[1,] = c(N0s[j], N0s[k]) #set initial conditions from N0s

    for(i in 1:(tf-1)) { #for each time i...
    	N[i+1,] = D%*%ricker.growth(N[i,], lambda, a) #calculate next values of N
    	}
    	
    final.Ns[k,j,] = N[tf,]
    }
  }

save(final.Ns, file="Data/HastingsModelOut.rdata")
image(N0s,N0s,log(abs(final.Ns[,,1] - final.Ns[,,2])), col=rainbow(256),
      xlab="Initial Population at Patch 1",
      ylab="Inital Population at Patch 2",
      main="Log Population Differences of Two Patches following Hastings 1990")
      

