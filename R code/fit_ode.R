#Here I've attempted to create a general function to fit data to a system of ODEs.  It works for this week's exercise, but some to dos for the future:
	#Modify to allow input data time points to be varied - not just consecutive integers
	#pre-process outputs for easy plotting
	#allow weighting of sum-of-squares by variable or other measures of error
	#test with some other systems of ODEs, more unknown variables
	#Optimize for computational efficiency


#fit_ode fits a system of ordinary differential equations to real data
#input is in the form of
	#odefunc - the system of ODEs to fit to the data, in the form of a function that will input to lsoda
	#parms - the parameters for odefunc, in the form of a list of scalars, vectors, and matrices, with string placeholders in the place unknown variables
	#guesses - a vector with names identical to the string placeholder in the parameters, and initial guesses for their values
	#data - the data to fit to the ODEs, in the form of a matrix with the number of columns equal to variables in the ODE

fit_ode = function(odefunc, parms, guesses, data) {
	require(deSolve)		#load library
	tf = dim(data)[1]		#set length of time equal to that in data
	times = 1:tf			#create times vector
	n0 = data[1,]			#set initial conditions to match data for 
	optimOut = optim(guesses, ode_error, parms=parms, n0=n0, times=times, odefunc=odefunc,data=data)	# call optimize function using initial guesses and the function ode_error, passing through parameters for ode_error
	return(optimOut)		#return optimOut
	}

#ode_error calculates the sum of squares error by fitting the data to the model.  Inputs:
	#inputvars - theunknown variables to optimize, with names
	#parms - the list of all parameters, with unknowns replaced by stings of their names
	#n0 - the initial conditions for the model
	#times - a list of times to evaluate at
	#odefunc - the system of ODEs to fit
	#data - the data to fit to the ODEs, in the form of a matrix with the number of columns equal to variables in the ODE
ode_error = function(inputvars, parms, n0, times, odefunc, data) {
	parmsin = numeric.list(replace.variables(inputvars, parms))  #replace the placeholders in parms with the inputvars
	lsoda = lsoda(n0, times, odefunc, parmsin) #solve odefunc at times using n0 and parmsin
	err = sumsq(data,lsoda[,2:ncol(lsoda)]) #calculate the sum of squares error between the data and the output
	return(err) #return the error
	}

#sumsq return the sum of squares error between two data sets that must be identical size matrices or vectors
sumsq = function(y,yhat) {
	out = sum((y-yhat)^2)
	return(out)
	}

#findinlist returns the location of of a string value in a list. It will return only the first value.  Output is in the form of a 2-element vecor, the first value being the index of element in the list and the second the index within the element

findinlist = function(value,list) { 
	for(i in 1:length(list)) {   #for each element in the list
		loc = pmatch(value,list[[i]]) #look for the sting value in that element
		if(is.na(loc) == FALSE) {out = c(i, loc); break} #if found, return both the list element and internal element number, then stop looking
		}
	return(out)
	}
		
#numeric.list converts all values in a list to numeric values
numeric.list = function(list) {	
	outlist = as.list(rep(0,length(list)))	#creates the output list
	names(outlist) = names(list)			#assigns the same names to output as input list
	for(i in 1:length(list)) {				#for each element out input list...
		outlist[[i]] = as.numeric(list[[i]])	#convert the element to numeric
		if(is.matrix(list[[i]])) {dim(outlist[[i]]) = dim(list[[i]])}  #if the element is a matrix, match dimensions of matrices
		}
	return(outlist)
	}

#replace.variables replaces string variables in a list with numeric variables from a vector with the string variables as names
replace.variables = function(variables, list) {
		names = names(variables)	#extract the names of the variables to replace
		for(i in 1:length(variables)) { #for each variable
		loc = findinlist(names[i],list)  #get the location of the variable in the list
		list[[loc[1]]][loc[2]] = variables[i] #and replace it with the appropriate variable
		}
	return(list)
	}

#a Lotka-Volterra function designed for input to lsoda.  "parms" must be an input with vectors r and K and matrix alphas, all named
LotVolt = function(t,n,parms) {
	with(parms,{
		LV = diag(1/K)%*%alphas  #calculate the interaction matrix from K and alphas
		dn = r*n*(1-LV%*%n)		 #calculate the change in population sizes
		return(list(dn))
		})
	}
	
#a logistic function designed for input to lsoda. 
logInt = function(t, n, parms) {
	with(as.list(parms), { # extract parameters from parms vector
		dn = r*n*(K-n)/K # logistic dn/dt
		return(list(dn)) # return dn/dt as a list
		})
	}
