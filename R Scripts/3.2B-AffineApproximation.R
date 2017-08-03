#Math 23 Script 3.2A-AffineApproximation.R

#Last modified November 2, 2014 by Paul Bamberg

#install.packages("numDeriv")  #uncomment this line the first time you run the script
library(numDeriv)

#Topic 1 - The tangent-line approximation for a single variable
#The R function that does the job is named grad(),
#even if we are differentiating a function from R to R.

grad(sin, pi/4)  #a single value of the derivative

curve(sin(x), from = 0, to = 2*pi)
curve(grad(sin, x), col = "red", add = TRUE) #create the derivative function

#We can illustrate the tangent-line approximation to a function
f <- function(x) sin(x^2.5)    #a somewhat messy function
curve(f(x), from = 0, to = 1)
x0 <- 0.5     #where to do the approximation
value <- f(x0)         #function value at x0
slope <- grad(f, x0)   #derivative at x0
tanline <- function(x) value + slope*(x-x0)  #the tangent-line approximation
curve(tanline(x), col = "red", add = TRUE)

#The error in the tangent-line approximation goes to zero faster than x - x0
f(x0+0.2)-tanline(x0+0.2)
f(x0+0.1)-tanline(x0+0.1)    #smaller by a factor of about 4
f(x0+0.02)-tanline(x0+0.02)  #smaller by a factor of about 100

#Topic 2 - Displaying a contour plot for a function
f <- function(v) sqrt(v[1])*log(1+v[2]) #function from R^2 to R
f(c(1.5, 2.5))                          #check that it works
#Define a range of values for x and y
x <- seq(from = 1, to = 3, by = 0.1)
y <- seq(from = 1, to = 3, by = 0.1)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x));z
filled.contour(x, y, z)   #pretty colors)
contour( x, y, z, asp = 1)    #shows contour lines

#Now we can calculate the gradient
x0 <-1.6; y0 <- 2.5
Df <- grad(f, c(x0,y0));Df       #a vector with the two partial derivatives
arrows(x0, y0 ,x0+Df[1], y0+Df[2])
#The gradient vector is along the direction of greatest rate of increase

#Using the gradient to do an affine approximation
v <- c(0.4, 0.1)      #an increment vector
x0 <-2.5; y0 <- 2
arrows(x0, y0 ,x0+v[1], y0+v[2], col = "red")    #display original and new point of evaluation
exact <- f(c(x0+v[1], y0+v[2])); exact       #the exact value
Df <- grad(f, c(x0,y0));Df       #a vector with the two partial derivatives
approx <- f(c(x0,y0))+sum(Df*v); approx         #the affine approximation
exact - approx               #the error in the approximation
#Repeat with only half of the increment
h <- v/2
f(c(x0+h[1], y0+h[2]))-(f(c(x0,y0))+sum(Df*h))

#We can do the same calculation in single-variable calculus
#Define a function on the line whose direction vector is v
g <- function(t) f(c(x0+t*v[1], y0+t*v[2]))
#Calculate the directional derivative
grad(g,0)        #the derivative with respect to t
sum(Df*v)        #Using the gradient (Jacobian) gives the same answer

#Topic 3 - The gradient as a vector field

g <- function(v) v[1]/(v[1]^2+v[2]^2) #function from R^2 to R
g(c(1.5, 2.5))                          #check that it works
#Define a range of values for x and y
x <- seq(from = -1, to = 1, by = 0.1)
y <- seq(from = -1, to = 1, by = 0.1)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,g),length(x))
contour( x, y, z, asp = 1, nlevels = 60)    #shows contour lines

#Now we can plot the gradient vector
h = 0.03   #scale the vector
for (x in c(-1,-0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8, 1)) {
  for (y in c(-1, -0.8, -0.6, -0.4, -0.2, 0.2, 0.4, 0.6, 0.8, 1)) {
    Df <- grad(g, c(x,y))       #a vector with the two partial derivatives
    arrows(x , y  ,x +h*Df[1], y +h*Df[2], col = "red", length = 0.05 )
  }
}

#The gradient vector is along the direction of greatest rate of increase


#Topic 4 - Plotting some pathological functions

f <- function(v) ifelse((v[1]^2+v[2]^2)==0, 0, v[1]*v[2]/(v[1]^2+v[2]^2))
f(c(0,0)); f(c(1,1))                      
x <- seq(from = -1, to = 1, by = 0.1)
y <- seq(from = -1, to = 1, by = 0.1)
pairs <- expand.grid(x,y)   #generate all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x))
contour( x, y, z, asp = 1)    #shows contour lines
#The value of the function depends only on the direction from the origin
filled.contour( x, y, z, asp = 1)    #show with colors

#Example 1.5.25 from Hubbard
f <- function(v) ifelse((v[1]^2+v[2]^2)==0, 0, abs(v[2])*exp(-abs(v[2])/v[1]^2)/(v[1]^2))
x <- seq(from = -1, to = 1, by = 0.1)
y <- seq(from = -1, to = 1, by = 0.1)
pairs <- expand.grid(x,y)   #generate all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x))
contour( x, y, z, asp = 1)    #shows contour lines
f(c(.2, .04))                 #value is 1/e
f(c(.02, .0004))              #value is 1/e

#To try to see the bizarre behavior we get close to the origin
x <- seq(from = -.1, to = .1, by = 0.01)
y <- seq(from = -.01, to = .01, by = 0.001)
pairs <- expand.grid(x,y)   #generate all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x))
contour( x, y, z)    #shows contour lines  -- looks much the same







