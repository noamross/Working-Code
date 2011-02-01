#Noam Ross ECL 298 Homework #1

#Plot Nt vs. Nt+t for both Ricker and Discrete Logistic Curves
# Ricker is N(t+1) = N(t)exp(r(1-N(t)/K))
# Discrete Logistic is N(t+1) = rN(t)(1-N(t)/K) + N(t)

rm(list=ls()) #clear workspace

#Define next population size via Ricker Growth Function given
#intrinsic growth rate R
#Current population N
#Carrying capacity K
RickerGrowth <- function(R, N, K) {
	Nnext <- N*exp(R*(1-N/K)) 
	return(Nnext)
	}
	
#Define next population size via Discrete Logistic Growth given
#intrinsic growth rate R
#Current population N
#Carrying capacity K
LogisticGrowth <- function(R, N, K) {
	Nnext <- R*N*(1-N/K) + N
	return(Nnext)
	}
	
#Define parameters
R <- 1 #intrinsic growth rate
N <- 0:30 #population range
K <- 10 #carrying capacity

#calculate functions
Rickercurve <- RickerGrowth(R, N, K) #calculate N+1 across range of N in for Ricker
Logisticcurve <- LogisticGrowth(R, N, K) #calculate N+1 across ranage of N for Logistic

#Plot curves in a file
pdf(file="LogRickerPlot.pdf")
plot(N,Logisticcurve, type="l", col="blue", xlab="Population Size N(t)", ylab="Population size N(t+1)",  main="Logistic v. Ricker Growth in Discrete Time") #Set up plot and Logistic curve
lines(N, Rickercurve, type="l", col="red") #add Ricker curve
legend("bottomleft", legend=c("Discrete Logistic", "Ricker"), lty=c(1,1), col=c("blue", "red"))
mtext(paste("Parameters: R = ", R, ", K = ", K), side=3) #annotate plot with parameters concatenated into a strong
dev.off() #close the pdf
