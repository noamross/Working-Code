# class 3 code: linear matrix models

# at the prompt: eigenvalues and eigenvectors
# Leslie matrix: example one to analyze
L = rbind(c(1/2, 1, 3/4), c(2/3, 0, 0), c(0, 1/3, 0))
L
# matrix multiplication: use %*%
# for example, give initial population size
n0 = c(2,0,0)
# the population at the next time step is
L%*%n0
# so to run in discrete time we'd use a for loop
tf = 10
Nt = matrix(0,nrow(L),tf) # initialize matrix
Nt[,1] = n0 # put in initial pop sizes
for(t in 1:(tf-1)) {Nt[,t+1]=L%*%Nt[,t]}
Nt
# use colSums to get total population size over time
colSums(Nt)
# eigen: both eigenvalues and eigenvectors (can use option only.values=TRUE for eigenvalues only)
ev = eigen(L)
ev$values # eigenvalues
ev$vectors # eivenvectors
# remember leading eigenvalue gives eventual growth rate, corresponding eigenvector gives stable age distribution
# use max to find leading eigenvalue: 
max(Re(ev$values)) # leading eigenvalue
# but need entry number to know corresponding eigenvector
levIdx = which.max(Re(ev$values)) # entry of leading eigenvalue
levIdx
leval = Re(ev$values[levIdx]) # leading eigenvalue
levec = Re(ev$vectors[,levIdx]) # corresponding eigenvector
leval
levec

