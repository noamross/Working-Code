# our goal - plot exponential vs. geometric growth

#define exponential growth function
#expGrowth calcultes population size n
# given intrinsic growth rate r
# time t
#and initital pop size n0
expGrowth <- function(r, t, n0) {
	n <- n0*exp(r*t) #calculate exponential growth
	return(n) # return n at each time t
	}


#define geomentric growth function

#define parameters

#plot both functions