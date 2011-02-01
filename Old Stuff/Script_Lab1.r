####################  Script for CPB Workshop: Hierarchical Modeling in Ecology Resources Lab 2: 'Maximum-likelihood for hierarchical modeling' 


### Import the data

d<-read.csv(file.choose())

### Which is actually doing this (assuming you have put your file in the same place as me)

d<-read.csv("/Users/Parry/Desktop/BaboonForaging.csv")

### Load packages to be used

library(lme4)

library(bbmle)

### Lets look at data 

dim(d)

head(d,n=10)

### Lets look at the response

hist(d$Foraging,breaks=10,ylim=c(0,500),col="red",main="Frequency Distribution of Female Presentation",xlab="Presentations per Sample")
box() 

### Are there any missing values

which(is.na(d$Foraging))

### We are doing a proportional analysis, so have to combine 'successes' (d$Foraging) and 'failures' (d$Sample_Length-d$Foraging)
Forage<-cbind(d$Foraging,d$Sample_Length-d$Foraging)

# Lets write a little logit function to transform model output
logit<-function(x) {exp(x)/(1+exp(x))}

# Prepare explanatory variables

DayLength<-ifelse(d$Season=="Short",0,1)
unique(DayLength)
MateGuarded<-ifelse(d$Consort=="No",0,1)
unique(MateGuarded)

# Fixed-effects only
model1<-glm(Forage~DayLength,binomial,data=d)
summary(model1)

# Transform coefficients
logit(coef(model1)[1])
logit(coef(model1)[1]+coef(model1)[2])

# Look at the actual data

XX<-d$Foraging[d$Season=="Long"]/d$Sample_Length[d$Season=="Long"]
YY<-d$Foraging[d$Season=="Short"]/d$Sample_Length[d$Season=="Short"]
b<-c(mean(YY),mean(XX))
SeasonNames<-c("Short","Long")
barplot(b,xlim=c(0,2.5),width=1,ylim=c(0,1),ylab="Probability",names.arg=SeasonNames,col="blue")
box()


##########

# Mixed-effects: Random intercepts only
model2<-glmer(Forage~DayLength+(1|MaleID),binomial,data=d)
summary(model2)

# Extract each male's deviation from the sample mean
ranef(model2)

# Plot those deivates
dotplot(ranef(model2))

# Intercept for the sample
fixef(model2)[1]

# Each male's individual intercept
coef(model2)[[1]][1]

# Plot each male's back-transformed intercept

y<-logit(coef(model2)[[1]][1])
x<-c(seq(1:19))
plot(y[,1]~x,ylim=c(0.2,1),ylab="Baseline Probability",xlab="MaleID",pch=16)
abline(logit(fixef(model2)[1]),0, col="red",lty=2,pch=16)


# Plot of the predictions across seasons for each Male
a<-logit(coef(model2)[[1]][1])
b<-logit(coef(model2)[[1]][1]+coef(model2)[[1]][2])
ab<-c(a[,1],b[,1])
short<-rep("Seas 1",19)
long<-rep("Seas 2",19)
dlength<-c(short,long)
male<-c(seq(1:19))
trellis.device(color=F)
my.panel <- function(x,y) {
    panel.barchart(x,y,width=1,horizontal=FALSE,col="blue")
}
barchart(ab ~ dlength | male  ,panel=my.panel)

# Plot individual intercepts (untransformed) with within-subject variance
par(mar=c(3,3,2,1))# Trim margin around plot [b,l,t,r]
par(tcl=0.35)# Switch tick marks to insides of axes
par(mgp=c(1.5,0.2,0))# Set margin lines; default c(3,1,0) [title,labels,line]
par(xaxs="r",yaxs="r")# Extend axis limits by 4% ("i" does no extension)
par(lwd=1.5)
par(mfrow=c(1,1))
y<-coef(model2)$MaleID[[1]]
x<-c(seq(1:18),1)
plot(y~x,ylim=c(-0.2,0.8),ylab="Intercept",xlab="MaleID",pch=16)
abline(fixef(model2)[1],0, col="red",lty=2,pch=16) 
z<-se.ranef(model2)$MaleID[,1]
ypositivevar<-y+z
ynegativevar<-y-z
segments(x,ypositivevar,x,ynegativevar,lty=1)


##########

# Mixed-effects: Random intercepts and slopes 
model3<-glmer(Forage~DayLength+(1+DayLength|MaleID),binomial,data=d)
summary(model3)

# Look at fixed effects from previous model
fixef(model2)
se.fixef(model2)

# Plot of the predictions across seasons for each Male
a<-logit(coef(model3)[[1]][1]))
b<-logit(coef(model3)[[1]][1]+coef(model3)[[1]][2]))
ab<-c(a[,1],b[,1])
short<-rep("Seas1",19)
long<-rep("Seas2",19)
dlength<-c(short,long)
male<-c(seq(1:19))
trellis.device(color=F)
my.panel <- function(x,y) {
    panel.barchart(x,y,width=1,horizontal=FALSE,col="blue")
}
barchart(ab ~ dlength | male  , width=1,panel=my.panel,pch=16)

# Plot the correlation between Male Intercepts and Male Slopes
a<-coef(model3)[[1]][1]
b<-coef(model3)[[1]][2]
plot(a[,1]~b[,1],xlab="Male Intercepts", ylab="Male Slopes",pch=16)
abline(lm(a[,1]~b[,1]))

# Check the correlation
cor(a[,1],b[,1])


##########

# Mixed-effects: Troops and males have varying intercepts, but only Male responses to season vary
model4<-glmer(Forage~DayLength+(1|TroopID)+(1+Season|MaleID),binomial,data=d)
summary(model4)


##########

# Mixed-effects: Years and males have varying intercepts, but only Male responses to season vary
model5<-glmer(Forage~DayLength+(1|Year)+(1+Season|MaleID),binomial,data=d)
summary(model5)


##### Some extra code


# Example code for individual intercepts
par(mar=c(3,3,2,1))# Trim margin around plot [b,l,t,r]
par(tcl=0.35)# Switch tick marks to insides of axes
par(mgp=c(1.5,0.2,0))# Set margin lines; default c(3,1,0) [title,labels,line]
par(xaxs="r",yaxs="r")# Extend axis limits by 4% ("i" does no extension)
par(lwd=1.5)
par(mfrow=c(1,1))
y<-coef(model3)$MaleID[[1]]
x<-c(seq(1:18),1)
plot(y~x,ylim=c(-1,1),ylab="Intercept",xlab="MaleID",pch=16)
abline(fixef(model2)[1],0, col="red",lty=2,pch=16) 
z<-se.ranef(model3)$MaleID[,1]
ypositivevar<-y+z
ynegativevar<-y-z
segments(x,ypositivevar,x,ynegativevar,lty=1)



