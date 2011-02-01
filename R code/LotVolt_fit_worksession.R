rm(list=ls()) #clear workspace

source("fit_ode.R") #load functions


#set parameters
r = c(0.7816, 0.6283)  #r and K are given
K = c(559.686, 202.4931)
alphas = rbind(c(1,"a21"),c("a12",1)) #create alpha matrix with unknowns
parms = list(r=r,K=K,alphas=alphas)  #bind into named list
guesses = c(a21=0.5, a12=0.5)	#create named vector of guesses
nTrue = as.matrix(read.table("paramecium2.txt")) #load data

#run fitting function
fit = fit_ode(LotVolt,parms,guesses,nTrue) 

#calculate some values for plotting
times = 1:dim(nTrue)[1]   #times vector
parms2 = numeric.list(replace.variables(fit$par, parms)) #create parameter list with fitted outputs
nSim = lsoda(nTrue[1,],times,LotVolt,parms2) #solve the ODEs with fitted values

#plots
plot(times,nTrue[,1], main="Lotka-Volterra ODE Model fit to Data",xlab="time",ylab="Population")  #plot the real data
points(times,nTrue[,2])
lines(times,nSim[,2])	#add the model fit lines
lines(times,nSim[,3])
legend("topleft",c("Data","Model Fit"),lty=c("blank","solid"),pch=c(1,NA_integer_)) #create a legend
text(3.3,300,bquote(alpha[21] == .(fit$par[1])))  #annotate with alpha values
text(3.3,280,bquote(alpha[12] == .(fit$par[2])))