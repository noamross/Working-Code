# class 3 exercise #2: Leslie stage-structured growth model sensitivities

#clear workspace
rm(list=ls()) #clear workspace

#define Leslie matrix
L = rbind(c(1/2, 1, 3/4), c(2/3, 0, 0), c(0, 1/3, 0))

#get right eigenvector
v = Re(eigen(L)$vectors[,1])
v = v/sum(v)

#calculate left eignenvector (reproductive sensitivities)
w = Re(eigen(t(L))$vectors[,1])
w = w/sum(v*w)

#get library Biodem
#library(Biodem)
#mtx.exp(L,20)%*%c(100,0,0) #mtx.exp is a matrix to the power (20), returns after time 20.
#sum(w*c(100,0,0))*Re(eigen(L)$values[1])^20*v #calculate value at time 20 by using the w corrective value

#create sensitivity matrix by outer product of w and v
S = outer(w,v)
barplot(S,beside=TRUE) #plot S values in grouped bars
barplot(S*sign(L),beside=TRUE) #use sign(L) to eliminate zeros

#for assignment, create barplot of elasticities of Teasel leslie matrix
#Eij (elasticity) = Sij*Lij/lambda
