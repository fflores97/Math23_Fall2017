#Math 23 Script 3.3B-NewtonsMethod.R

#Last modified: November 9, 2014 by Paul Bamberg
#install.packages("numDeriv")
library(numDeriv)    #for the grad() function
par(mar = c(2,2,1,1))  #maximize space for graphs
#Topic 1 - Single variable

#An easy case -- the square root of 2
#Define a function to use in an equation f(x) = 0
f <- function(x) x^2 - 2
#Graph the function on a interval where it has a root
curve(f(x), from = 1, to = 2)
abline(h=0, col = "green")

#Find a value that makes f close to zero  and evaluate f and f' there
x0 <- 1.2; f(x0); grad(f, x0)
#Add the tangent line to the plot
curve(f(x0) + grad(f, x0)*(x-x0), col = "red", add = TRUE)
#Solve a linear approximation to find where the tangent line is zero
x1 <- x0 - f(x0)/grad(f, x0); abline(v = x1, col = "red"); f(x1)
#Repeat to improve the approximation
x2 <- x1 - f(x1)/grad(f, x1); abline(v = x2, col = "red", lty = 2); f(x2)
x3 <- x2 - f(x2)/grad(f, x2); abline(v = x3, col = "red", lty = 3); f(x3)
x3
#A slightly harder case: solving tan(x) = x near 5*pi/2
f <- function(x) tan(x) - x
#Graph the function on a interval where it has a root
curve(f(x), from = 0 , to = 8, ylim = c(-10,10))
abline(h=0, col = "green")
#Let's go for the root between 4 and 5
curve(f(x), from = 4 , to = 5, ylim = c(-1 ,1 ))
abline(h=0, col = "green")

#Find a value that makes f close to zero  and evaluate f and f' there
x0 <- 4.44; f(x0); grad(f, x0)
#Add the tangent line to the plot
curve(f(x0) + grad(f, x0)*(x-x0), col = "red", add = TRUE)
#Solve a linear approximation to find where the tangent line is zero
x1 <- x0 - f(x0)/grad(f, x0); abline(v = x1, col = "red"); f(x1)
#Repeat to improve the approximation
x2 <- x1 - f(x1)/grad(f, x1); abline(v = x2, col = "red", lty = 2); f(x2)
x3 <- x2 - f(x2)/grad(f, x2); abline(v = x3, col = "red", lty = 3); f(x3)

#In this case using a poor initial guess would have led to disaster.
curve(f(x), from = 3 , to = 8, ylim = c(-10,10))
abline(h=0, col = "green")
x0 <- 3.9; f(x0); grad(f, x0)
#Add the tangent line to the plot
curve(f(x0) + grad(f, x0)*(x-x0), col = "red", add = TRUE)
#Solve a linear approximation to find where the tangent line is zero
x1 <- x0 - f(x0)/grad(f, x0); abline(v = x1, col = "red"); f(x1)
#Repeat to improve the approximation
x2 <- x1 - f(x1)/grad(f, x1); abline(v = x2, col = "red", lty = 2); f(x2)
x3 <- x2 - f(x2)/grad(f, x2); abline(v = x3, col = "red", lty = 3); f(x3)
#These approximations are not going to converge to anything!

#Topic 2 - 2 equations, 2 unknowns
#log x + log y = 3
#x^2 - y = 1
#Define a function from R^2 to R^2 to use in f(v) = 0
f <-function(v) c(log(v[1])+log(v[2])-3, v[1]^2 - v[2] -1)
#Make a good initial guess
v0 = c(3,9); f(v0)
#Calculate the 2 x 2 Jacobian matrix
A <- jacobian(f, v0); A
#Invert the Jacobian
AInv =solve(A); AInv
#Use the same update formula as in the single-variable case
v1 <- v0 - AInv%*%f(v0); v1; f(v1)
#Repeat to improve the approximation
A <- jacobian(f, v1)
v2 <- v1 - solve(A)%*%f(v1); v2; f(v2)

A <- jacobian(f, v2)
v3 <- v2 - solve(A)%*%f(v2); f(v3)
v3     #this is a very good approximate solution to our original equations
#Once the process gets going, it superconverges.
#The number of zeroes after the decimal point doubles with each iteration.

#Topic 3 - Three equations in three unknowns.

#x + 2y + 3z = 10
#x^2 + y^2 + 2z^2 = 12
#2x^3 +  y^3 + z^3 = 14

#Create the function f
f <- function(v) c(v[1] + 2*v[2] + 3*v[3] - 10, v[1]^2 + v[2]^2 + 2*v[3]^2 - 12, 2*v[1]^3 +  v[2]^3 + v[3]^3 -14 )
#Make an initial guess
v0 <- c(1,1,2); f(v0)     #not very good, but perhaps good enough
A <- jacobian(f, v0); A
#Invert the Jacobian
AInv =solve(A); AInv
#Use the same update formula 
v1 <- v0 - AInv%*%f(v0); v1; f(v1)
#Repeat to improve the approximation
A <- jacobian(f, v1)
v2 <- v1 - solve(A)%*%f(v1); v2; f(v2)

A <- jacobian(f, v2)
v3 <- v2 - solve(A)%*%f(v2); v3; f(v3)

  