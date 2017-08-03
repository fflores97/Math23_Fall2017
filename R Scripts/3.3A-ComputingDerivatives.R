#Math 23 Script 3.3A-ComputingDerivatives.R

#Last modified November 9, 2014 by Paul Bamberg

#install.packages("numDeriv")
library(numDeriv)

#Topic 1 - Testing for differentiability

#A scalar-valued function is differentiable if the directional derivative
#is a linear function of vector v. This will always be the case if all the 
#partial derivatives are continuous.

f1 <- function(v) (sin(v[1])*v[2]^2)/(1+sum(v^2))  #a well-behaved function
v0 <- c(2,3)    #where to test for differentiability
vv <- c(1.3, -0.4)  #vector for directional derivative
g <- function(t) f1(v0+t*vv) - f1(v0)
grad(g,0)            #the directional derivative
sum(grad(f1, v0)*vv) #calculated from the partial derivatives using linearity

#A nondifferentiable function will fail this test 
f2 <- function(v) ifelse((v[1]^2+v[2]^2) == 0, 0,(v[1]+v[2])*(v[1]-v[2])^2/(v[1]^2+v[2]^2))#an ill-behaved function
v0 <- c(0,0)    #where to test for differentiability
vv <- c(1.7, -0.4)  #vector for directional derivative
#vv <- c(1.7,0)  #vector for directional derivative
g <- function(t) f2(v0+t*vv) - f2(v)
grad(g,0)            #the directional derivative
sum(grad(f2, v0)*vv) #calculated from the partial derivatives using linearity
#The partial derivatives (Jacobian, gradient) almost always give the wrong answer.
vv <- runif(2, min = 1, max = 3)  #use a random direction vector
g <- function(t) f2(v0+t*vv) - f2(v)
grad(g,0)            #the directional derivative
sum(grad(f2, v0)*vv) #calculated from the partial derivatives using linearity

#Topic 2 - Illustrating the derivative rules
g1 <- function(v)  c(v[1]^2 +cos(v[2]), exp(v[1])*v[2])
g2 <- function(v)  c(sqrt(v[1]) * sin(v[2]),v[1]^3/(1+v[2]^2))
gsum <- function(v) g1(v)+g2(v)
gprod <- function(v) f1(v)*g1(v)
g1.1 <- function(v) g1(v)[1]         #just the first component
gprod1 <- function(v)  f1(v)*g1.1(v)   #just the first component of gprod
gcomp <- function(v) g1(g2(v))
gdot <- function(v) sum(g1(v)*g2(v))

#Check the sum rule
v0 <- c(1.7, 2.2)   #where to evaluate the derivatives
jacobian(gsum,v0)
jacobian(g1,v0)+jacobian(g2,v0)   #should be the same

#Check the chain rule
v0 <- c(1.2, 1.9)   #where to evaluate the derivatives
jacobian(gcomp,v0)
jacobian(g1,g2(v0))%*%jacobian(g2,v0)   #should be the same

#Check the product rule for scalar-valued functions.
#In this case we can compare the derivatives as functions.
v0 <- c(1.2, 1.9)   #where to evaluate the derivatives
grad(gprod1,v0)
f1(v0)*grad(g1.1,v0)+grad(f1,v0)*g1.1(v0)   #should be the same

#Check the product rule for scalar-valued times vector-valued.
#In the general case we must evaluate on a specified increment vector.
v0 <- c(1.2, 1.9)   #where to evaluate the derivatives
vv<- c(0.3,0.7)     #increment vector
jacobian(gprod,v0)%*%vv
f1(v0)*jacobian(g1,v0)%*%vv+(jacobian(f1,v0)%*%vv)*g1(v0)   #should be the same
f1(v0)*grad(g1.1,v0)%*%vv+grad(f1,v0)%*%vv*g1.1(v0)   #same as first component

#Check the dot product rule
v0 <- c(1.2, 1.9)   #where to evaluate the derivatives
vv<- c(0.3,0.7)     #increment vector
jacobian(gdot,v0)%*%vv
sum((jacobian(g1,v0)%*%vv)*g2(v0))+sum(g1(v0)*(jacobian(g2,v0)%*%vv))   #should be the same






