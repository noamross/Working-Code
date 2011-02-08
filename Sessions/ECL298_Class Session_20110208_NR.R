#In-class worksession February 8, 2011

flips = rbinom(100,24000,0.5)
flips

test = dbinom(12012,24000,0.5)
test

test2 = dbinom(11988:12012,24000,0.5)
test2
sum(test2)

data=c(33,rep(0,25),rep(1,3),rep(2,2),rep(2,3),8,10,rep(0,19),12)
hist(data,n=34)

lambda = 1.5
N = 100

poisd = dpois(1:10,lambda)
binomd = dbinom(1:10,N,lambda/N)

par(mfrow = c(3, 3))
for(i in seq(2,18,by=2)) {
  barplot(rbind(dpois(1:10,lambda),dbinom(1:10,i,lambda/i)),
          beside=TRUE, main=paste("N =",i), ylim=c(0,0.5))
}


Initial=c(5, 5, 10, 10, 15, 15, 20, 20, 30, 30, 50, 50, 75, 75, 100, 100)
Killed=c(1, 2, 5, 6, 10, 9, 7, 10, 11, 15, 5, 21, 32, 18, 25, 35)

#HW model (based on Type 2 functional response)
# total number killed(N) = aN/(1 + ahN), a=encounter rate, h = handling time
#each tadpole killed wtg probability of a/(1+ahN).
#So for given trials, p is above, trials = N, for a given density initial N, biomial dist
#So plot most likely model given Initial and Killed data