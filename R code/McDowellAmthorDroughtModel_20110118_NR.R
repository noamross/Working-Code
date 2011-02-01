#Test of tree mortality model from McDowell (2011)
#Noam Ross 1/18/2011

#clear workspace
rm(list=ls()) #clear workspace

#Parameterize
tf = 140 #time steps of model, here days since drought
b = 15 #half-life of plant growth rate decay in drought (days)
p = 21 #half-life of plant photosynthesis rate decay during drought (days)
m = 80 #half-life of maintenance respiration decay during drought (days)
g = 0.5 #ratio of growth respiration to growth
dt = 1 #size of time steps (days)

#Initialize
S0 = 40 #Initial non-structural carbohydrate content, g/kg
P0 = 4 #Initial rate of photosynthesis, g/kg-d
B0 = 2 #Initial rate of growth, g/kg-d
M0 = 1 #Initial rate of maintenance respiration, g/kg-d

#Create empty variable vectors of length tf
S = rep(0,tf); S[1] = S0
P = rep(0,tf); P[1] = P0
B = rep(0,tf); B[1] = B0
M = rep(0,tf); M[1] = M0
G = rep(0,tf); G[1] = g*B0

#Run Model
for (t in 1:tf) {								
	S[t+1] = S[t]+dt*(P[t]-B[t]-G[t]-M[t])		#Carbohydrate content is mass balance
	P[t+1] = P0*p^4/(p^4 + t^4)					#Photosynthesis and growth decay with time
	B[t+1] = B0*b^4/(b^4 + t^4)	
	G[t+1] = g*B[t]								#Growth respiration is a fraction of growth
	M[t+1] = min(M0*m^4/(m^4 + t^4),S[t+1])	#Maintenance decays with time but can't exceed the store of carbohydrate
	#M[t+1] = min(M0*(1+ (0.3*t^4/(40^4 + t^4))),S[t+1])
								
	}
	
#Plot
time = 1:(tf+1) #create time vector
plot(time, S, axes=F, type="l", main="McDowell/Amthor Carbon Starvation Model (2011)",xlab="Time of Drought (days)", ylab="Plant Carbohydrates Stores (kg/g)") #create plot of S without axes
axis(2,pretty(range(S),6))  #define left axis
par(new=T) #overwrite plot with next plot without clearing
plot(time,P,axes=F, type="l",col="green",ylab="",xlab="") #create plot of P without axes
axis(4,pretty(range(P),6)) #define right axis
lines(time,B,col="blue") #add remaining physiological data
lines(time,M,col="red")
lines(time,G,col="orange")
axis(1,pretty(range(time),7)) #define X axis
mtext("Physiological Rates (g/(kg-d))",side=4,padj=-2) #add text to right axis
