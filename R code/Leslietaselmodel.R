# class 3 exercise: Run Leslie stage-structured growth model

#clear workspace
rm(list=ls()) #clear workspace

#define Leslie matrix from file
L = as.matrix(read.table("teasel_stage.txt"))

#define parameters
tf = 100 #number of time steps
n0 = c(2,0,0,0,0,0,0) #set initial conditions

#create empty matrix for output
N = matrix(0,length(n0),tf)

#initialize conditions
N[,1] = n0

#run for loop to fill matrix
for(t in 1:(tf-1)) {
	N[,t+1] = L%*%N[,t]
	}
	
#calculate population sum totals
Ntot = colSums(N)

#calculate leading eigenvalue
ev = eigen(L)
eigL = max(Re(ev$values))

#create linear growth model
n0L = sum(n0)
times = 0:(tf-1)
NL = n0L*(eigL^(times))

#plot both growth trajectories on log scale

plot(1:tf,log(Ntot))
lines(1:tf,log(NL))

#estimate eigenvalue from ratio of populations in Leslie model
eigest = Ntot[2:tf]/Ntot[1:(tf-1)]

#plot estimate vs. real eigenvalue
plot(1:(tf-1),eigest,type="l")
lines(1:(tf-1),rep(eigL,1,tf-1))
