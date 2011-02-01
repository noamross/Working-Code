# 1. setting default parameter values in functions (if didn't cover in lecture 2):
# take
expGrowth = function(r,t,n0) {n = n0*exp(r*t)}
# so far we've told you to call this with something like
expGrowth(2,1,1)
# note that you can have the parameters out of order if you label them:
expGrowth(t=1,n0=1,r=2)
# if you've defined any of these already, this might look something like
t = 1
expGrowth(t=t,n0=1,r=2)
# you don't have to use the same label
x = 1
expGrowth(t=x,n0=1,r=2)
# also, you can define the function with defaults for as many parameters as you care to:
expGrowth = function(r=2,t=1,n0=1) {n = n0*exp(r*t)}
# now all of these give the same value:
expGrowth(2,1,1)
expGrowth(t=1,n0=1,r=2)
expGrowth()
# and you can deviate from the defaults for as many paramters as you choose, e.g.
expGrowth(n0=2,r=4)

# 2. outputting more than one variable
# start with idea that can label entries to vectors
v = c(a=1, b=2)
v 
# when convert this to list, can call by labels
v = as.list(c(a=1, b=2))
v
v$a
with(as.list(v),{print(a)})
output2 = function(x) {a=1:x; b=1:(2*x); return(list(a=a,b=b))}
y = output2(5)
y$a
y$b
