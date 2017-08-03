#Math 23 Script 3.4D-CriticalPoints

#Last modified: November 18, 2014 by Paul Bamberg -- added filled.contour()
library(numDeriv)
#Topic 1 - Finding and classifying critical points

#Here is a function from Hubbard Exercise 3.6.4, intended for computerized analysis.
f <- function(x,y) -x^3+y^3+x*y+4*x-5*y    #needed for contour lines
fVec <- function(v) -v[1]^3+v[2]^3+v[1]*v[2]+4*v[1]-5*v[2] #needed for numDeriv
#We can search for maxima and minima by plotting contour lines.
y <- x <- seq(-4, 4, 0.2)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z) #colors may help identify maximum/minimum.
contour(x,y,z)
#The closed contour for value 0 is suggestive. It is centered near (-1,1).
x <- seq(-2, 1, 0.2)
y <- seq(-0.5, 2.5, 0.2)
z <- outer(x,y,f)    #matrix of function values
contour(x,y,z)
#It looks as though we have found a minimum near (-1.3, 1.5).
x <- seq(-1.6, -1, 0.02)
y <- seq(1.2, 1.7, 0.02)
z <- outer(x,y,f)    #matrix of function values
contour(x,y,z)
contour(x,y,z, levels = c(-9,-9.02, -9.04, -9.06, -9.08, -9.09, -9.1))
#Now we have a good approximation.
#Use the partial derivatives to get two equations in two unknowns
Df <- function(v) c(-3*v[1]^2+v[2]+4, 3*v[2]^2+v[1]-5)
v0 <- c(-1.35, 1.45)
Df(v0)       #pretty close to zero
grad(fVec, v0)   #in fact, we don't need to know our Math 1 stuff!

#Use Newton's method to improve the approximation
A <- jacobian(Df, v0); A
hessian(fVec,v0)  #this matrix of second derivatives can be found numerically
v1 <- v0 - solve(A)%*%Df(v0); v1; Df(v1)
f(v1[1], v1[2])   #the local minimum value of the function

#Look at the Hessian matrix of second partial derivatives at the critical point.
grad(fVec,v1)       #confirms our critical point
H <- hessian(fVec, v1); H; det(H) #positive det confirms extremum
sum(H*diag(c(1,1)))   #positive trace confirms a minimum

#We have found a local minimum. Go back and look for another extremum.
#Replot the original contour lines
y <- x <- seq(-3, 3, 0.2)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#The color display hints at a maximum near (1, -1)?
abline(v=1, col = "green"); abline(h=-1, col = "green")
x <- seq(0, 2, 0.1)
y <- seq(-2, 0, 0.1)
z <- outer(x,y,f)    #matrix of function values
contour(x,y,z)
#It looks as though we have found a maximum near (1, -1.2)
v0 <- c(1, -1.2)
Df(v0)       #pretty close to zero, though not as good as before
A <- jacobian(Df, v0); A
v1 <- v0 - solve(A)%*%Df(v0); v1; Df(v1) #one iteration of Newton's method works wonders
f(v1[1], v1[2])     #the value of the function at the local maximum.
#Check the contour lines near our maximum.
x <- seq(0.92, 1.02, 0.001)
y <- seq(-1.2, -1.1, 0.001)
z <- outer(x,y,f)    #matrix of function values
contour(x,y,z)
contour(x,y,z, levels = c(6.08, 6.081, 6.0811, 6.0812, 6.08126))

#Have a look at the Hessian matrix 
grad(fVec,v1)       #confirms our critical point
H <- hessian(fVec, v1); H; det(H) #positive det confirms extremum
sum(H*diag(c(1,1)))   #negative trace confirms a maximum

#Replot the original contour lines to look for more critical points.
y <- x <- seq(-3, 3, 0.2)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#The 0 contour gets close to itself near (1.3,1.3)
abline(h=1.3, col = "green"); abline(v=1.3, col = "green")
#Zoom in for a closer look
y <- x <- seq(1, 1.5, 0.01)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#Something is happening for values between 0.2 and 0.3.
contour(x,y,z, levels = seq(0.2, 0.3, 0.01))
#we have found a saddle point near (1.3, 1.12)
#Zoom in for a closer look.
x <- seq(1.25, 1.35, 0.01)
y <- seq(1.07, 1.17, 0.01)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#Something is happening for values between 0.262 and 0.264.
contour(x,y,z, levels = seq(0.262, 0.264, 0.0005))

#It looks as though we have found a saddle point near (1.31, 1.11)
v0 <- c(1.31, 1.11)
Df(v0)       #pretty close to zero.
A <- jacobian(Df, v0); A
v1 <- v0 - solve(A)%*%Df(v0); v1; Df(v1) #one iteration of Newton's method works wonders
f(v1[1], v1[2])      #this is the value of the function at the saddle point.
#Notice that there are larger and smaller function values nearby.
#This is not an extremum!

#Have a look at the Hessian matrix 
grad(fVec,v1)       #confirms our critical point
H <- hessian(fVec, v1); H; det(H) #negative determinant confirms saddle point.

#If we set both partial derivatives to zero we could get a 4th degree equation for x.
#So perhaps there is a fourth critical point.
#Replot the original contour lines
y <- x <- seq(-3, 3, 0.2)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#The 0 contour gets closest to itself near (-1,-1)
abline(v=-1, col = "green"); abline(h=-1, col = "green")
#Zoom in for a closer look
y <- x <- seq(-2, -0, 0.01)
z <- outer(x,y,f)    #matrix of function values
filled.contour(x,y,z)
contour(x,y,z)
#Something is happening for values between 2 and 3.
contour(x,y,z, levels = seq(2, 3, 0.1))
#We have found a saddle point near (-1, -1.5)
#This is probably close enough so that Newton's method will work.

v0 <- c(-1, -1.5)
Df(v0)       #not too far from zero.
A <- jacobian(Df, v0); A
v1 <- v0 - solve(A)%*%Df(v0); v1; Df(v1) #one iteration of Newton's method works wonders
f(v1[1], v1[2])      #this is the value of the function at the saddle point.

#Have a look at the Hessian matrix 
grad(fVec,v1)       #confirms our critical point
H <- hessian(fVec, v1); H; det(H) #negative determinant confirms saddle point.





