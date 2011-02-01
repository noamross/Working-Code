#Leslie Matrix Analysis Library
#Noam Ross, 1/19/2011

#This library contains functions to analyze population growth in Leslie matrices.  Input is in the form of a matrix L, which must be nXn square with all positive values

#Les_lambda returns the real component of the dominant eigenvector of the Leslie matrix

Les_lambda <- function(L) {
	lambda <- max(Re(eigen(L)$values))
	return(lambda)
	}
	
#Les_vector returns the normalized dominant eigenvector, or stable age distribution, of the Leslie matrix

Les_vector <- function(L) {
	v <- Re(eigen(L)$vectors[,1])
	v <- v/sum(v)
	return(v)
	}
	
#Les_repvec returns the normalized left eigenvector, or the reproductive sensitivities of the age classes

Les_repvec <-function(L) {
	v <- Re(eigen(L)$vectors[,1])		#first calculate the right eigenvector
	v <- v/sum(v)
	w <- Re(eigen(t(L))$vectors[,1])		#calculate the dominant eigenvector of transpose(L)
	w <- w/sum(v*w)						#normalize
	return(w)					
	}					
	
#Les_sens returns a matrix of the sensitivities of the elements of L.  If zeros=TRUE, it will return zero values for zero matrix elements.  The default will calculate them normally

Les_sens <-function(L, zeros=FALSE) {
	v <- Re(eigen(L)$vectors[,1])			#first calculate the right eigenvector
	v <- v/sum(v)							#normalize
	w <- Re(eigen(t(L))$vectors[,1])		#calculate the dominant eigenvector of transpose(L)
	w <- w/sum(v*w)							#normalize
	S <- outer(w,v)							#calculate the sensitivity matrix
	if(zeros==TRUE) {S <- S*sign(L)}		#If required, zero sensitivities for zero elements
	return(S)
	}
	
#Les_elas returns a matrix of the elasticities of the elements of L.  Note that all zero matrix elements will by definition have zero elasticity

Les_elas <- function(L) {
	lambda <- max(Re(eigen(L)$values))		#calculate the dominant eigenvalue
	v <- Re(eigen(L)$vectors[,1])			#first calculate the right eigenvector
	v <- v/sum(v)							#normalize
	w <- Re(eigen(t(L))$vectors[,1])		#calculate the dominant eigenvector of transpose(L)
	w <- w/sum(v*w)							#normalize
	S <- outer(w,v)							#calculate the sensitivity matrix
	E <- S*L/lambda							#convert sensitivities to elasticities
	return(E)
	}