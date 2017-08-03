#Math 23 Script 3.4B-Manifolds2D.R

#Last modified November 15, 2014 by Paul Bamberg
library(numDeriv)
library(plotrix)    #for circles

#Topic 1 - A one-dimensional submanifold of R^2 -- the unit circle
par(mar=c(2,2,1,1))

#Let's plot the unit circle in three different ways
#1. As a union of function graphs
plot(NULL, xlim = c(-1.2,1.2),  ylim = c(-1.2,1.2), pch = ".", asp = 1) #empty plot
#Near x = 0, y = 1, y is a smooth function of x
x <- seq(-0.8, 0.8, 0.01)   #sequence of x values
y <- sqrt(1 - x^2)         #corresponding sequence of y values
points(x, y, pch = ".", type = "l", col = "red")
#Near x = 0, y = -1, y is a different smooth function of x
y <- -sqrt(1 - x^2)         #corresponding sequence of y values
points(x, y, pch = ".", type = "l", col = "orange")
#Near x = 1, y = 0, x is a smooth function of y
y <- seq(-0.6, 0.6, 0.01)   #sequence of y values
x <- sqrt(1 - y^2)         #corresponding sequence of x values
points(x, y, pch = ".", type = "l", col = "blue")
#Near x = -1, y = 0, x is a different smooth function of y
y <- seq(-0.6, 0.6, 0.01)   #sequence of y values
x <- -sqrt(1 - y^2)         #corresponding sequence of x values
points(x, y, pch = ".", type = "l", col = "green")

#The tangent space at (-0.6. 0.8) is the graph of the derivative of the function
abline(h = 0.8, lty = 2); abline(v = -0.6, lty = 2)
f <- function(x) sqrt(1 - x^2)
Df <- function(x) 0.8 + grad(f, -0.6)*(x+0.6)
curve(Df(x), col = "red", add = TRUE, lty = 2)

#If we write x as a function of y we get the same tangent space
g <- function(y) sqrt(1 - y^2)
Dg <- function(y) -0.6 + grad(g, 0.8)*(x-0.8)
curve(Df(x), col = "red", add = TRUE)


#2. In terms of a parametrization
plot(NULL, xlim = c(-1.2,1.2),  ylim = c(-1.2,1.2), pch = ".", asp = 1) #empty plot
t <- seq(0, 2*pi, 0.01)   #sequence of parameter values
x <- cos(t)     #corresponding sequence of x values
y <- sin(t)     #corresponding sequence of y values
points(x, y, pch = ".", type = "l", col = "red")     #we get the entire circle

#The tangent space is the image of the derivative of the parametrization function
t0 <- acos(-0.6)     #parameter value for (-0.6,0.8)
abline(h = 0.8, lty = 2); abline(v = -0.6, lty = 2)
paramF <- function(t) c(cos(t), sin(t))  #vector-valued function
DparamF <- jacobian(paramF, t0); DparamF   #direction vector
t <- seq(-1,1,.01)   #sequence of parameter values
points(-0.6+ DparamF[1]*t, 0.8 + DparamF[2]*t, pch = ".", type = "l")

#3. As a locus
F <- function(x,y) x^2 +y^2 - 1
#Here is a slightly shorter way to plot a contour line
x <- y <- seq(-1.1,1.1,0.01)
#You may remember using outer() to generate a multiplication table for a finite field.
z <- outer(x,y,F); head(z)
#As required, z is a matrix giving the value of F for each pair of y and z values
contour(x=x, y=x, z=z, levels=0, asp = 1)
v0 <- c(-0.6, 0.8)     #the point where we want the tangent space
F <- function(v) v[1]^2 +v[2]^2 - 1   #rewrite as a vector function
DF <- jacobian(F, v0); DF
abline(h = 0.8, lty = 2); abline(v = -0.6, lty = 2)
#The tangent space is the kernel of DF
#y.dot is the active variable, and x.dot - -A^-1By.dot, where A and B are 1x1 matrices
slope <- -(1/DF[1])*DF[2];slope
y.dot <- seq(-1.1,1.1,0.01)
x.dot <- slope*y.dot
points(-0.6 + x.dot, 0.8 + y.dot, pch = ".", type = "l") #same tangent space

#Topic 2 - Interesting examples from the textbook
#Example 3.1.11 x^8 +2x^3 + y + y^5 = c.
F <- function(x,y) x^8 +2*x^3 + y + y^5
#The test for a smooth manifold is that the Jacobian matrix is never [0 0]
#Just plot (or inspect) the partial derivatives
D1F <- function(x) 8*x^7 + 6*x
curve(D1F(x), from = -1, to = 1)   #is zero for x = 0
D2F <- function(y) 1 + 5*y^4
curve(D2F(y), from = -1, to = 1, xname = "y", add = TRUE)   #is never zero
abline (h = 0, col = "green")

#We know that the locus is a smooth curve but have no clue what is looks like
#Just plot contour lines for various values of c.
x <- y <- seq(-1.1,1.1,0.01)
z <- outer(x,y,F)
#As required, z is a matrix giving the value of F for each pair of y and z values
contour(x=x, y=x, z=z)    #all contour lines are smooth curves

#Example 3.1.12 x^4 + y^4 +x^2 - y^2 = c
F <- function(x,y) x^4 + y^4 +x^2 - y^2
#Again plot (or inspect) the partial derivatives
D1F <- function(x) 4*x^3 + 2*x
curve(D1F(x), from = -1, to = 1)   
abline (h = 0, col = "green")
D2F <- function(y) 3*y^3 - 2*y
curve(D2F(y), from = -1, to = 1, xname = "y", add = TRUE)   #is zero for two values of y
F(0,0)       #c = 0 will not give a smooth manifold
F(0,1/sqrt(2))       #c = -0.25 will not give a smooth manifold

#Plot the contour lines to see what goes wrong.
x <- y <- seq(-1.1,1.1,0.01)
z <- outer(x,y,F)
contour(x=x, y=x, z=z)    #all contour lines for c > 0 are smooth curves
#At x=y=0 we cannot solve for x in terms of y or vice versa
#Let's check negative values of c
contour(x=x, y=x, z=z, levels = c(-0.05, -0.1, -0.15, -0.2, -0.24, -0.249, -0.25))
#The manifold is disconnected , which is OK,
#but for c = -0.25 it degenerates into two isolated points (not shown)
#That is a zero-dimensional manifold

#Topic 3 - Parametrized curves in R^2
#The derivative of the parametrization function must be one-to-one.
#It will fail the test only if it is the zero vector.

#An example:
g1 <- function(t) t + exp(t)
g2 <- function(t) t + exp(2*t)
Dg1 <- function(t) 1 +exp(t)
Dg2 <- function(t) 1 +2*exp(2*t)
curve(Dg1(x), from = -3, to = 1, ylim = c(-3,3))
curve(Dg2(x), from = -3, to = 1, ylim = c(-3,3), col = "red", add = TRUE)
abline(h = 0, col = "green")
#Neither component of the derivative is ever zero.
#we expect to see a smooth parametrized curve.
t <- seq(-3,2,0.01)   #parameter values
plot(NULL, xlim = c(-3,10),  ylim = c(-25,60), pch = ".") #empty plot
points(g1(t), g2(t), pch = ".", type = "l")

#Let's find the tangent line for t = 1.5
x1 <- g1(1.5); y1 <- g2(1.5); x1; y1
abline(v = x1, col = "blue", lty = 2)
abline(h = y1, col = "blue", lty = 2)
Dx1 <- Dg1(1.5); Dy1 <- Dg2(1.5); Dx1; Dy1
#So the tangent space is the subspace with direction vector (5.48, 41.17)
slope <- Dy1/Dx1; slope
#If we want the tangent line, we need the y-intercept
intercept <- y1 - slope*x1; intercept
abline(v = 0, col = "red", lty = 2)
abline(h = intercept, col = "red", lty = 2)
#Now we can plot the tangent line.
abline(intercept, slope, col = "red")

#While we can easily generate lots of points on this manifold, it is difficult
#to test whether a given point lies on the curve.
#Try x = 4, y = 10
abline(v = 4, col = "green", lty = 2)
abline(h = 10, col = "green", lty = 2)
#Even when you find a point on the curve, you still need Newton's method to 
#get the parameter value.

#Now make some small changes so that we no longer have a manifold.
g1 <- function(t) -t + exp(t)
g2 <- function(t) -2*t + exp(2*t)
Dg1 <- function(t) -1 +exp(t)
Dg2 <- function(t) -2 +2*exp(2*t)
curve(Dg1(x), from = -3, to = 1, ylim = c(-3,3))
curve(Dg2(x), from = -3, to = 1, ylim = c(-3,3), col = "red", add = TRUE)
abline(h = 0, col = "green")
#Both components of the derivative are zero when t = 0.
#We expect to see problems with the curve.
t <- seq(-3,2,0.01)   #parameter values
plot(NULL, xlim = c(-3,10),  ylim = c(-25,60), pch = ".") #empty plot
points(g1(t), g2(t), pch = ".", type = "l")
#Let's zero in on the suspect point
t <- seq(-0.6,0,0.001)   #parameter values
plot(NULL, xlim = c(0.8,1.2),  ylim = c(1,1.5), pch = ".") #empty plot
points(g1(t), g2(t), pch = ".", type = "l", col = "red")
t <- seq(0,0.6,0.001)   #parameter values
points(g1(t), g2(t), pch = ".", type = "l", col = "blue")

#Here is another way that we can use smooth functions and still not have a smooth manifold
g1 <- function(t) sin(t)
g2 <- function(t) sin(2*t)
t <- seq(0,2*pi,0.01)   #parameter values
plot(NULL, xlim = c(-1,1 ),  ylim = c(-1,1), pch = ".") #empty plot
points(g1(t), g2(t), pch = ".", type = "l")
#The problem is that the parametrization is not one-to-one
#At the origin, we cannot express either variable as a function of the other.

#Topic 4 - A two-dimensional manifold in R^2
#This is just an open set.
#As a function graph or as a locus it is uninteresting.
#This is example 2.10.6, with a slightly different interpretation.
plot(NULL, xlim = c(-3,11),  ylim = c(-7,7), asp = 1) #empty plot
draw.circle(0,0, radius = 3)
draw.circle(10,0, radius = 1)
#A point of the manifold is the midpoint of any segment joining the circles
vLeft <- function(theta) c(3*cos(theta), 3*sin(theta))
vRight <- function(phi) c(10 + cos(phi), sin(phi))
#Generate a random point
angles <- runif(2, min= 0, max = 2*pi); angles
segments(vLeft(angles[1])[1], vLeft(angles[1])[2], vRight(angles[2])[1], vRight(angles[2])[2], col = "gray")
points((vLeft(angles[1])[1]+vRight(angles[2])[1])/2,(vLeft(angles[1])[2]+vRight(angles[2])[2])/2)

#Now let's plot lots of points on the manifold
N <- 1000
for (i in 1:N) {
  angles <- runif(2, min= 0, max = 2*pi); angles
  points((vLeft(angles[1])[1]+vRight(angles[2])[1])/2,
         (vLeft(angles[1])[2]+vRight(angles[2])[2])/2, pch = 20)
}

#Clearly the manifold is the region between two circles.
#Does it include its boundary?
g <- function(v) c((3*cos(v[1])+cos(v[2]))/2, (3*sin(v[1])+sin(v[2]))/2)
#Look at what happens when the angles theta and phi are equal
u <- 1
segments(vLeft(u)[1], vLeft(u)[2], vRight(u)[1], vRight(u)[2], col = "red")
points((vLeft(u)[1]+vRight(u)[1])/2,(vLeft(u)[2]+vRight(u)[2])/2, pch = 20, col = "red")
Dg <- jacobian(g, c(u,u)); Dg; round(det(Dg))   #the matrix is not invertible

#Look at what happens when the angles theta and phi differ by pi
u <- 2.5
segments(vLeft(u)[1], vLeft(u)[2], vRight(u+pi)[1], vRight(u+pi)[2], col = "blue")
points((vLeft(u)[1]+vRight(u+pi)[1])/2,(vLeft(u)[2]+vRight(u+pi)[2])/2, pch = 20, col = "blue")
Dg <- jacobian(g, c(u,u)); Dg; round(det(Dg))   #the matrix is not invertible

#So when the point is on the boundary of the region the derivative is not 1-to-1.
#To have a manifold, we must exclude these boundary points.
#In other words, the region between the circles must be open.

#Topic 5 - A zero-dimensional manifold in R^2
#Here is a locus function
F<- function(x,y) x^2*(x-1)^2 + y^2*(y-1)^2
x <- y <- seq(-0.5,1.5,0.01)
z <- outer(x,y,F)
contour(x=x, y=x, z=z, levels = c(0.1, 0.05, 0.02, 0.01, 0.001, 0.0001))
#The equation F(x,y) = 0 describes a zero-dimensional manifold of four isolated points
F <- function(v) v[1]^2 *(v[1]-1)^2 + v[2]^2*(v[2]-1)^2   #rewrite as a vector function
v0 <- c(1,0)   #one of the points
DF <- jacobian(F, v0); round(DF, digits = 6)
#So the derivative falis the "onto" test at every point on the manifold.

F<- function(x,y) x*(x-1)*y*(y-1)
x <- y <- seq(-0.5,1.5,0.01)
z <- outer(x,y,F)
contour(x=x, y=x, z=z, levels = c(0.5, 0.2, 0.1, 0.0624, 0.05, 0.01, 0))
#The equation F(x,y) = 0 does not describe a smooth 1-dimensional manifold.
F <- function(v) v[1]*(v[1]-1)*v[2]*(v[2]-1)   #rewrite as a vector function
v0 <- c(1,0)   #one of the points where contour lines cross.
DF <- jacobian(F, v0); round(DF, digits = 6); #DF is [0 0]
#So the derivative fails the "onto" test at the crossing points.
#Eliminate them and we have a disconnected 1-dimensional manifold.


