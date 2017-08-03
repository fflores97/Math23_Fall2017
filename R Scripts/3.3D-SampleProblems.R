#Math 23 Script 3.3D-SampleProblems.R

#Last modified Nov. 13, 2014 by Paul Bamberg

library(numDeriv)

#Problem 1
#v[1] is latitude, v[2] is longitude
paramF <- function(v) c(cos(v[1])*cos(v[2]), cos(v[1])*sin(v[2]), sin(v[1]))
v0 <- c(asin(3/5), pi/2); v0 #latitude and longitude in radians of Bayingol, China
round(paramF(v0), digits = 6) #Cartesian coordinates of Bayingol
paramJ <- jacobian(paramF, v0); round(paramJ, digits = 6)
#Use the Jacobian to find a best affine approximation
paramF(v0) + paramJ%*%c(-0.01, 0.02) #move a little bit south and east


#Problem 2
A <- matrix(c(1,0,1,1),2); A
H <- matrix(c(0,-.02,.01,0),2); H  #choose h = .01, k = -.02
S <- function(v) v%*%v    #the matrix-squaring function
S(A)      #check that it works
DS <- function(M1,M2)  M1%*%M2 + M2%*%M1   #the derivative
DS(A,H)    #should be a good approximation to the change in S
S(A+H)-S(A)   #and it is!
rem <- S(A+H)-S(A)-DS(A,H); rem   #the remainder is small
rem/sqrt(sum(H^2))    #even when divided by the length of matrix H

#Problem 3b
f <- function(v) -log(sqrt(v[1]^2+v[2]^2)) #a function just of r
#This is the electric potential of a long, thin charged rod along the z axis
par(mar=(c(2,2,1,1)))   #small margins maximize graphing space

#Define a range of values for x and y
x <- seq(from = -2, to = 2.2, by = 0.15)
y <- seq(from = -2, to = 2.2, by = 0.15)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x)) 
contour( x, y, z, asp = 1)    #shows contour lines
v0 <- c(0.6, 0.8)   #a point on the contour line for f = 0
E <- -jacobian(f, v0); E #electric field is minus the gradient
w <- c(0.8,-0.6) #make a vector from y and -x
arrows(v0[1], v0[2], v0[1] +w[1], v0[2] +w[2], col = "red" ) #tangent to contour line
arrows(v0[1], v0[2], v0[1] +E[1], v0[2] +E[2], col = "blue" ) #orthogonal to contour line
round(sum(c(0.8,-0.6)*partials),digits = 6)
#So the partial differential equation says that E is perpendicular to the equipotential lines.

#Problem 5 - the monkey saddle
f <- function(v) (3*v[1]^2*v[2] - v[2]^3)/(v[1]^2+v[2]^2)
#Plot the contour lines for this crazy function
#Define a range of values for x and y
x <- seq(from = -2, to = 2.2, by = 0.15)
y <- seq(from = -2, to = 2.2, by = 0.15)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x)) 
contour( x, y, z, asp = 1)    #shows contour lines

#Let's calculate the directional derivative as a function of direction
dd <- function(theta) {
  g <- function(t) f(c(t*cos(theta), t*sin(theta)))
  grad(g,0)
}
dd(0)    #zero along the x axis
dd(2*pi/3) #zero on the 120 degree line
ddVec <- Vectorize(dd)     #necessary so that curve will work
curve(ddVec, from = 0, to= 2*pi)
abline(v = c(pi/2, 7*pi/6, 11*pi/6), col = "red")
#There are places for both legs and the tail to slope down the saddle

#The first partial derivative is zero at the origin
grad(f, c(0, 0)) 
grad(f, c(0.01, 0.01)) #but not nearby
grad(f, c(0.001, 0.001)) #getting closer does not help
                     
                     

