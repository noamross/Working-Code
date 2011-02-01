#Population growth and dispersal over continuous space

#define parameters

#define initial conditions

#define growth function

#define kernel function

#create loop of updates
	#first plot initial conditions
	#update values

N(t+1,x) = int( K(y-x)f(N(t,y)) ) over all x, dy

#to discretize
	#set boundaries at +-xlr
	#set number of patches/intervals in the spcace
	#specify initial condition vector
	
	#discretizing k = (1/sqrt(2*pi*sigma))*exp(-x^2/sig^2)
		#normalize k vector sums to 1.
		
	#updating
		#update by growth and reproduction
		#use a fft to update for N and K, multiply
		#to get updated N, do fft-1