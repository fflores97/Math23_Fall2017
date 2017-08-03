#Math 23 Script 3.5C-IntegralVecField.R

#Last modified December 1, 2014 by Paul Bamberg
#install.packages("colorRamps")
library(colorRamps)
library(numDeriv)
#Topic 1 - A parametrized path
#Choose a parameter interval [a,b] and a parametrization function g.
#Choose the vector field.

g1 <- function(t) tan(t)
g2 <- function(t) 1/cos(t)
paramF <- function(t) c(g1(t), g2(t))
a <- -pi/6; b <- pi/4
F1 <- function(x,y) x^2+x/y
F2 <- function(x,y) -x*y+y/2
paramY <- 0.5 #where to plot the parameter space

#Approximating the integral along the curve

plot(NULL, xlim = c(a-0.1,b+0.3), ylim = c(paramY-0.1,1.6), asp = 1)
N <- 40    #number of segments
xSeq <- seq(a, b, length.out =N+1 )
colSeq <-  magenta2green(N)
scale <- 0.1    #for vector field arrows
rSum = 0
for (i in 1:N) {
  t0 <- xSeq[i]; t1 <- xSeq[i+1]
  arrows(t0,  paramY , t1 , paramY, col = colSeq[i], length = 0.08)
  x0 <- g1(t0); y0 <- g2(t0);x1 <- g1(t1); y1 <- g2(t1)
  arrows(x0, y0 , x1, y1,  col = colSeq[i], length = 0.08)
  hx <-F1(x0,y0); hy <-F2(x0,y0)
  arrows(x0, y0, x0+scale*hx, y0+scale*hy,length = 0.08)
  rSum <- rSum + hx*(x1-x0) +hy*(y1-y0)
}
rSum    #approximate value of the line integral

#Approximating the integral in parameter space
rpSum = 0
incr <- (b-a)/N   #fixed increment between parameter values
for (i in 1:N) {
  t0 <- xSeq[i]
  field <- c(F1(g1(t0),g2(t0)), F2(g1(t0),g2(t0)))  #Vector field at the parametrized point
  curveVec <- jacobian(paramF,t0)*incr
  rpSum = rpSum + sum(field*curveVec)
}
rpSum

#We get a better approximation by using the midpoint of each interval
rpSum =0 
incr <- (b-a)/N   #fixed increment between parameter values
for (i in 1:N) {
  t0 <- xSeq[i]+incr/2
  field <- c(F1(g1(t0),g2(t0)), F2(g1(t0),g2(t0)))  #Vector field at the parametrized point
  curveVec <- jacobian(paramF,t0)*incr
  rpSum = rpSum + sum(field*curveVec)
}
rpSum
  
#Evaluation as an integral in parameter space
integrand <- function(t) F1(g1(t),g2(t))*grad(g1,t)+  F2(g1(t),g2(t))*grad(g2,t)
integrate(integrand, a, b) #remarkably close agreement!

#Topic 2 - integrating a vector field that is a gradient
f <- function(x,y) -x^3+y^3+x*y+x-4*y+5
F1 <- function(x,y) -3*x^2+y+1
F2 <- function(x,y) 3*y^2+x-4
#We can do the approximation along the curve
plot(NULL, xlim = c(a-0.1,b+0.1), ylim = c(paramY-0.1,2), asp = 1)
scale <- 0.1    #for vector field arrows
rSum = 0
for (i in 1:N) {
  t0 <- xSeq[i]; t1 <- xSeq[i+1]
  arrows(t0,  paramY , t1 , paramY, col = colSeq[i], length = 0.08)
  x0 <- g1(t0); y0 <- g2(t0);x1 <- g1(t1); y1 <- g2(t1)
  arrows(x0, y0 , x1, y1,  col = colSeq[i], length = 0.08)
  hx <-F1(x0,y0); hy <-F2(x0,y0)
  arrows(x0, y0, x0+scale*hx, y0+scale*hy,length = 0.08)
  rSum <- rSum + hx*(x1-x0) +hy*(y1-y0)
}
rSum    #approximate value of the line integral

#We get a better approximation by using the midpoint of each interval
rpSum =0 
incr <- (b-a)/N   #fixed increment between parameter values
for (i in 1:N) {
  t0 <- xSeq[i]+incr/2
  field <- c(F1(g1(t0),g2(t0)), F2(g1(t0),g2(t0)))  #Vector field at the parametrized point
  curveVec <- jacobian(paramF,t0)*incr
  rpSum = rpSum + sum(field*curveVec)
}
rpSum

#Evaluation as an integral in parameter space
integrand <- function(t) F1(g1(t),g2(t))*grad(g1,t)+  F2(g1(t),g2(t))*grad(g2,t)
integrate(integrand, a, b) #remarkably close agreement!

#In this case there is an easier way to get the answer
f(g1(b),g2(b))
f(g1(a),g2(a))
f(g1(b),g2(b))-f(g1(a),g2(a))

#Add the contour lines of f to the plot
x <- seq(-1, 1.5, 0.1)
y <- seq(0, 2, 0.1)
z <- outer(x,y,f)    #matrix of function values
contour(x,y,z, levels = seq(1, 5,0.5),  add = TRUE)


