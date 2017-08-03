#Math 23 Script 3.2C-Sample Problems.R

#Topic 1 - Problem 4
F <- function(x,y,z) x*y*z/(x^2+y^2+z^2)
#Generate three coordinates, all less than a in magnitude
a <- 0.1
v <- runif(3, min = -a, max = a);v
F(v[1], v[2], v[3])
#Now do this 1000 times
N <- 1000; Fvals <- numeric(N)
for (i in 1:N) {
  v <- runif(3, min = -a, max = a);v
  Fvals[i] <- F(v[1], v[2], v[3])
}
hist(Fvals)
max(abs(Fvals))  #can be made less than any epsilon > 0 by choosing small a

g <- function(x,y,z) (x*y+x*z+y*z)/(x^2+y^2+z^2)
a <- 0.1
v <- runif(3, min = -a, max = a);v
g(v[1], v[2], v[3])
#Now do this 1000 times
N <- 1000; gvals <- numeric(N)
for (i in 1:N) {
  v <- runif(3, min = -a, max = a);v
  gvals[i] <- g(v[1], v[2], v[3])
}
hist(gvals)
max(abs(gvals))  #always close to 1, no matter how small a is made

#Topic 2 - Problem 6 (stolen from script 3.2B)
library(numDeriv)
f <- function(v) sqrt(v[1]*v[2]^3) #function from R^2 to R
f(c(4, 1))                          #check that it works
#Define a range of values for x and y
x <- seq(from = 3, to = 5, by = 0.1)
y <- seq(from = 0, to = 2, by = 0.1)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,f),length(x)) 
filled.contour(x, y, z)   #pretty colors)
contour( x, y, z, asp = 1)    #shows contour lines

#Now we can calculate the gradient
x0 <- 4; y0 <- 1
Df <- grad(f, c(x0,y0));Df       #a vector with the two partial derivatives
arrows(x0, y0 ,x0+0.1*Df[1], y0+0.1*Df[2])  #rescale the vector so it stays on screen
#The gradient vector is along the direction of greatest rate of increase

#Using the gradient to do an affine approximation
v <- c(2, 1)      #an increment vector
t <- 0.1
arrows(x0, y0 ,x0+t*v[1], y0+t*v[2], col = "red")    #display original and new point of evaluation
exact <- f(c(x0+t*v[1], y0+t*v[2])); exact       #the exact value
Df <- grad(f, c(x0,y0));Df       #a vector with the two partial derivatives
approx <- f(c(x0,y0))+sum(Df*t*v); approx         #the affine approximation
exact - approx               #the error in the approximation
#Repeat with only half of the increment
t <- t/2
f(c(x0+t*v[1], y0+t*v[2]))-(f(c(x0,y0))+sum(Df*t*v))

#We can do the same calculation in single-variable calculus
#Define a function on the line whose direction vector is v
g <- function(t) f(c(x0+2*t, y0+t))
#Calculate the directional derivative
grad(g,0)        #the derivative with respect to t
sum(Df*v)        #Using the gradient (Jacobian) gives the same answer

#Topic 3 - Problem 7 (again stolen from script 3.2B)
T <- function(v) 25 + 0.1*v[1]^2*v[2]^3 #function from R^2 to R
x0 <- 1; y0 <- 2
T(c(x0, y0))                          #check that it works
#Define a range of values for x and y
x <- seq(from = 0.5, to = 2, by = 0.1)
y <- seq(from = 1.5, to = 3, by = 0.1)
#Make a table of all pairs of values
pairs <- expand.grid(x,y)  #a list of all pairs of values
#Apply the function to each pair of values to make a matrix of values
z <- matrix(apply(pairs,1,T),length(x)) 
filled.contour(x, y, z)   #pretty colors)
contour( x, y, z, asp = 1)    #shows contour lines

#Now we can calculate the gradient
DT <- grad(T, c(x0,y0));DT       #a vector with the two partial derivatives
arrows(x0, y0 ,x0+0.1*DT[1], y0+0.1*DT[2])  #rescale the vector so it stays on screen
#The gradient vector is along the direction of greatest rate of increase

#Using the gradient to do an affine approximation
v <- DT/sqrt(sum(DT^2)); v      #a unit vector along the gradient
t <- 0.1
arrows(x0, y0 ,x0+t*v[1], y0+t*v[2], col = "red")    #display original and new point of evaluation
exact <- T(c(x0+t*v[1], y0+t*v[2])); exact       #the exact value
#Try a different unit vector
theta <- runif(1, min = 0, max = 2*pi); theta
v <- c(cos(theta),sin(theta)); v
T(c(x0+t*v[1], y0+t*v[2])); exact   #new v is not as good
#Now try it 1000 times
values <- numeric(1000)
for (i in 1:1000) {
  theta <- runif(1, min = 0, max = 2*pi)
  v <- c(cos(theta),sin(theta))
  values[i] <- T(c(x0+t*v[1], y0+t*v[2]))
}
hist(values)
max(values); exact   #gradient is best only in the limit of small increments
 
