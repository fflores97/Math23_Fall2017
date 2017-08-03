#Math 23 Script 3.3C-InverseFunction.R

#Last modified November 9, 2014 by Paul Bamberg

#install.packages("numDeriv")
library(numDeriv)     #for the Jacobian function

#Topic 1 - A parametrization function and its inverse
#Here is the well-known parametrization function for polar coordinates

paramF <- function(v) c(v[1]*cos(v[2]), v[1]*sin(v[2]))
paramF(c(5, 37*pi/180))   #Cartesian coordinates for r = 5, theta = 37 degrees

#In this case you can do algebra and find a formula for the inverse function
coordF <- function(v) c(sqrt(v[1]^2+v[2]^2),atan2(v[2],v[1]))
coordF(c(4,3))     #the angle comes out in radians
coordF(paramF(c(5, 37*pi/180)))    #the composition is the identity function
paramF(coordF(c(3, 4)))            #in either order

#The inverse function theorem lets us compute the derivative of the inverse function
#without having a formula for the function itself.
v0 <- c(5, atan(3/4)); v0    #a pair of polar coodinates
paramJ <- jacobian(paramF, v0); paramJ
invJ <- solve(paramJ); invJ
#To check, we can calculate the derivative of the inverse function
coordJ <- jacobian(coordF, c(4,3)); coordJ

#Topic 2 - Visualizing coordinates by means of a contour plot

#It is enlightening to do a contour plot for both coordinate functions
par(mar=(c(2,2,1,1)))   #small margins maximize graphing space
r <- function(v) sqrt(v[1]^2+v[2]^2) #first polar coordinate
r(c(4,3))                          #check that it works
#Define a range of values for x and y
x <- seq(from = 2, to = 6, by = 0.6)
y <- seq(from = 1, to = 5, by = 0.2)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,r),length(x)) 
contour( x, y, z, asp = 1)    #shows contour lines

#Now do the same for the second polar coordinate function
theta <- function(v) atan2(v[2],v[1]) #second polar coordinate
theta(c(4,3))                          #check that it works
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,theta),length(x)) 
contour( x, y, z, asp = 1, add = TRUE, col = "red")    #shows contour lines

#Consider a point and an increment vector
v0 <- c(4,3); h <- c(0.2,0.5)
arrows(v0[1], v0[2], v0[1]+h[1], v0[2]+h[2], col = "green")
#The derivative of the inverse function lets us compute
#the best affine approximation to the polar coordinates of v0+h.
coordF(v0) + coordJ%*%h   #checks against the contour lines

#Topic 3 - An example that is economic, not geometric
#This is sample problem 7.
f <- function(v) c(v[1] +sqrt(v[1])*v[2], v[1]^1.5 + sqrt(v[2]))
f(c(4,9))   #it works: H(appiness)=22, Cost= 11

#Define a range of values for x and y
x <- seq(from = 2, to = 6, by = 0.6)
y <- seq(from = 7, to = 11, by = 0.2)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the happiness function to each pair of values to make a matrix of values
H <- function(v) f(v)[1]
z <- matrix(apply(pairs,1,H),length(x)) #apply the happiness function
contour( x, y, z, asp = 1, nlevels = 20)    #shows contour lines

#Now do the same for the cost function
C <- function(v) f(v)[2]
z <- matrix(apply(pairs,1,C),length(x)) 
contour( x, y, z, asp = 1, add = TRUE, col = "red")    #shows contour lines
#In general the contour lines do not cross at right angles.
#If they were parallel, we would have a serious problem!
#That is the case where the Jacobian matrix is not invertible.

#The challenge in the problem is to make H = 19, C = 10
#We can read a pretty good approximation off the contour plot
abline(h = 7.9, col = "green")
abline(v = 3.72, col = "green")
#This approximation could then be improved by using Newton's method.

#Alternatively, we can calculate the derivative of the inverse function
#even though we cannot write a formula for the inverse function.
fJ <- jacobian(f, c(4,9));fJ
fInvJ <- solve(fJ); fInvJ
increment <- c(-3,-1)    #happiness 22->19 , cost 11->10
c(4,9)+fInvJ%*%increment #close to what we read off the contour plot

#For more than two variables the Jacobian matrix approach still works,
#even though we can no longer make and interpret a contour plot.
