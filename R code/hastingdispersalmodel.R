#set parameters
lambda = 20 #population growth coefficient
a = 1 #intraspecific competition term
d = 0.05 #dispersal coefficient
p = 10 #number of patches
N0 = runif(p)*10 #initial conditions
tf = 100 #number of time steps

#create dispersal matrix
D = matrix(d/(p-1),p,p) #create disperal matrix with uniform dispersal
diag(D) = 1-d

N=matrix(0,tf,p) #create variable space
N[1,] = N0 #set initial conditions

ricker.growth = function(N, lambda, a) { #density dependent growth function
	N2 = lambda*N*exp(-a*N)
	return(N2)
	}
	
for(i in 1:(tf-1)) { #for each time i...
	N[i+1,] = D%*%ricker.growth(N[i,], lambda, a) #calculate next values of N
	}
	
matplot(N,type="l") #plot results for all patches
plot(rowMeans(N), type="b") #plot results for average of patches