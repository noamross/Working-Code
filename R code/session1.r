# non-hierarchical simulated data example
# y_i ~ N( kalpha + kbeta*x_i , ksigma )
kalpha <- 7
kbeta <- 0.75
ksigma <- 3
x <- rnorm( 30 , mean=10 , sd=5 )
y <- rnorm( 30 , mean=kalpha+kbeta*x , sd=ksigma )
plot( y ~ x )
abline( a=kalpha , b=kbeta , col="red" )
d <- data.frame( x=x , y=y )

# variable intercepts
a <- 7
b <- 0.75
n <- 100
xij <- rnorm( n , mean=10 , sd=5 )
aj <- rnorm( 5 , mean=a , sd=7 )
j <- rep( 1:5, times=n/5 )
yij <- rnorm( n , mean=aj[j]+b*xij , sd=3 )
d <- data.frame( y=yij , x=xij , j=j , a=aj )

# plot as a single regression
plot( yij ~ xij , col=rainbow(5)[j] , pch=16 , bg="black" )
abline( lm(yij~xij) )

# plot with multiple intercepts
plot( yij ~ xij , col=rainbow(5)[gi] , pch=16 )
for ( i in 1:5 ) {
    abline( a=aj[i] , b=b , col=rainbow(5)[i] )
}

# variable slopes
a <- 7
b <- 2
n <- 100
xij <- rnorm( n , mean=10 , sd=5 )
bj <- rnorm( 5 , mean=b , sd=2 )
gi <- rep( 1:5, times=n/5 )
yij <- rnorm( n , mean=a + bj[gi]*xij , sd=10 )
d <- data.frame( y=yij , x=xij , g=gi , a=aj )

# plot as a single regression
plot( yij ~ xij , col=rainbow(5)[gi] , pch=16 , bg="black" )
abline( lm(yij~xij) )

# plot with multiple intercepts
plot( yij ~ xij , col=rainbow(5)[gi] , pch=16 )
for ( i in 1:5 ) {
    abline( a=a , b=bj[i] , col=rainbow(5)[i] )
}

# variable slopes and intercepts
a <- 7
b <- 2
n <- 100
xij <- rnorm( n , mean=10 , sd=5 )
aj <- rnorm( 5 , mean=a , sd=25 )
bj <- rnorm( 5 , mean=b , sd=2 )
gi <- rep( 1:5, times=n/5 )
yij <- rnorm( n , mean=aj[gi] + bj[gi]*xij , sd=10 )
d <- data.frame( y=yij , x=xij , g=gi , a=aj )

# plot as a single regression
plot( yij ~ xij , col=rainbow(5)[gi] , pch=16 , bg="black" )
abline( lm(yij~xij) )

# plot with multiple intercepts
plot( yij ~ xij , col=rainbow(5)[gi] , pch=16 )
for ( i in 1:5 ) {
    abline( a=aj[i] , b=bj[i] , col=rainbow(5)[i] )
}

# fit examples
library(lme4)
library(bbmle)
m0 <- lm( y ~ x , data=d )
# random intercepts
m1 <- lmer( y ~ (1|g) + x , data=d , REML=FALSE )\
# random slopes, constant intercepts
m2 <- lmer( y ~ x + (0+x|g) , data=d , REML=FALSE )
# random slopes and intercepts, but uncorrelated with one another
m3 <- lmer( y ~ (1|g) + x + (0+x|g) , data=d , REML=FALSE )
# correlated random slopes and intercepts
m4 <- lmer( y ~ x + (1+x|g) , data=d , REML=TRUE )

AICtab(logLik(m0),logLik(m1),logLik(m2),logLik(m3),logLik(m4),base=TRUE,weights=TRUE)

# big multi-level example
library(nlme)
library(lattice)
library(lme4)

data(MathAchieve)
data(MathAchSchool)

attach(MathAchieve)
mses <- tapply(SES,School,mean)
detach(MathAchieve)

Bryk <- as.data.frame(MathAchieve[, c("School","SES","MathAch")])
names(Bryk) <- c("school","ses","mathach")
Bryk$meanses <- mses[as.character(Bryk$school)]
Bryk$cses <- Bryk$ses - Bryk$meanses
sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool)
Bryk$sector <- sector[as.character(Bryk$school)]

d <- Bryk

#> str(Bryk)
#'data.frame':	7185 obs. of  6 variables:
# $ school : Ord.factor w/ 160 levels "8367"<"8854"<..: 59 59 59 59 59 59 59 59 59 59 ...
# $ ses    : num  -1.528 -0.588 -0.528 -0.668 -0.158 ...
# $ mathach: num  5.88 19.71 20.35 8.78 17.9 ...
# $ meanses: num [1:7185(1d)] -0.434 -0.434 -0.434 -0.434 -0.434 ...
#  ..- attr(*, "dimnames")=List of 1
#  .. ..$ : chr  "1224" "1224" "1224" "1224" ...
# $ sector : Factor w/ 2 levels "Public","Catholic": 1 1 1 1 1 1 1 1 1 1 ...
# $ cses   : num [1:7185(1d)] -1.0936 -0.1536 -0.0936 -0.2336 0.2764 ...
#  ..- attr(*, "dimnames")=List of 1
#  .. ..$ : chr  "1224" "1224" "1224" "1224" ...

# is math achievement related to ses?
# does it vary by catholic/public? (fixed effect)
# does it vary by across schools within cath/pub? (random effect)

# plot trellis to show variation in slope by school
cat <- sample(unique(d$school[d$sector=="Catholic"]), 20)
Cat.20 <- groupedData(mathach ~ ses | school, data=d[is.element(d$school,cat),])

pub <- sample(unique(d$school[d$sector=="Public"]), 20)
Pub.20 <- groupedData(mathach ~ ses | school, data=d[is.element(d$school,pub),])

trellis.device(color=F)
my.panel <- function(x,y) {
    panel.xyplot(x,y)
    panel.lmline(x,y,lty=1,col="red")
}
xyplot(mathach ~ ses | school , data=Cat.20 , main="Catholic" , panel=my.panel)
xyplot(mathach ~ ses | school , data=Pub.20 , main="Public" , panel=my.panel)


# run lm on each school within sector
cat.list <- lmList(mathach ~ ses | school , subset=sector=="Catholic",data=d)
pub.list <- lmList(mathach ~ ses | school , subset=sector=="Public",data=d)
plot(intervals(cat.list),main="Catholic")
plot(intervals(pub.list),main="Public")
allcoefs <- data.frame( ses=c(coef(cat.list)$ses,coef(pub.list)$ses) , sector=c(rep("cat",length(coef(cat.list)$ses)),rep("pub",length(coef(pub.list)$ses))) )
densityplot( ~ ses , groups=sector , data=allcoefs , auto.key=TRUE )

# fit

# pure fixed effect approach doesn't fit!!
mX <- lm( mathach ~ school , data=d )

# basic fixed model, without school fixed effects
m0 <- lm( mathach ~ meanses*cses + cses*sector , data=d )

# random approaches
m1 <- lmer( mathach ~ meanses*cses + cses*sector + (1|school) , data=d )
m2 <- lmer( mathach ~ meanses*cses + cses*sector + (1 + cses|school) , data=d )
m3 <- lmer( mathach ~ meanses*cses + cses*sector + (0 + cses|school) , data=d )
m4 <- lmer( mathach ~ meanses*cses + cses*sector + (1|school) + (0 + cses|school) , data=d )
m5 <- lmer( mathach ~ meanses*cses + cses*sector + (1|sector/school) , data=d )
m5.2 <- lmer( mathach ~ meanses*cses + cses*sector + (1|sector) + (1|school) , data=d )

# compare
# note that models fit via lmer() need you to wrap them in logLik(). This is because AICtab and lmer are from different packages and don't always play well together. Not a big deal, if you do as I did below.
library(bbmle)
AICtab(m0,logLik(m1),logLik(m2),logLik(m3),logLik(m4),logLik(m5),base=TRUE,weights=TRUE)

# plot varying slopes of best model
a <- coef(m1)$school$"(Intercept)"
b <- coef(m1)$school$cses
plot( 1 ~ 1 , col="white" , ylim=c(min(d$mathach),max(d$mathach)) , xlim=c(min(d$cses),max(d$cses)) , xlab="cses" , ylab="mathach" )
for ( i in 1:length(a) ) abline( a[i] , b[i] , lty=1 , col=sample(c("gray","black"),1) )

# plot varying slopes of 2nd best model
a <- coef(m4)$school$"(Intercept)"
b <- coef(m4)$school$cses
plot( 1 ~ 1 , col="white" , ylim=c(min(d$mathach),max(d$mathach)) , xlim=c(min(d$cses),max(d$cses)) , xlab="cses" , ylab="mathach" )
for ( i in 1:length(a) ) abline( a[i] , b[i] , lty=1 , col=sample(c("gray","black"),1) )


# oxford boys growth example---a time series
data(Oxboys)
d2 <- Oxboys

plot( d2$height ~ d2$age )
for ( i in 1:length(levels(d2$Subject)) )
    lines( d2$age[d2$Subject==levels(d2$Subject)[i]] , d2$height[d2$Subject==levels(d2$Subject)[i]] )
    
oxboysg <- groupedData( height ~ age | Subject, data=d2 )
trellis.device(color=FALSE)
my.panel <- function(x,y) {
    panel.xyplot(x,y)
    panel.lmline(x,y,lty=1,col="red")
}
xyplot( height ~ age | Subject , data=oxboysg , panel=my.panel)

m0 <- lm( height ~ age , data=d2 )
m1 <- lm( height ~ age + Subject , data=d2 )
m2 <- lm( height ~ age * Subject , data=d2 )

AICctab(m0,m1,m2,base=TRUE,weights=TRUE,nobs=nrow(d2))

m3 <- lmer( height ~ (1|Subject) , data=d2 )
m4 <- lmer( height ~ age + (1|Subject) , data=d2 )
m5 <- lmer( height ~ age + (1+age|Subject) , data=d2 )

AICctab( logLik(m3) , logLik(m4) , logLik(m5) , m0 , m1 , m2 , base=TRUE , weights=TRUE , nobs=nrow(d2) )

# plot varying slopes of m5
a <- coef(m5)$Subject$"(Intercept)"
b <- coef(m5)$Subject$age
plot( 1 ~ 1 , col="white" , ylim=c(min(d2$height),max(d2$height)) , xlim=c(min(d2$age),max(d2$age)) , xlab="age" , ylab="height" )
for ( i in 1:length(a) ) abline( a[i] , b[i] , lty=1 , col=sample(c("gray","black"),1) )