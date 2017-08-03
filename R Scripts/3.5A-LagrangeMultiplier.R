#Math 23 Script 3.5A-LagrangeMultiplier.R

#Topic 1 - Constrained critical points in R^2

library(numDeriv)

#Start with some contour lines to show values of a constraint.

F <- function(x,y) x^0.7*y^0.3  #A Cobb-Douglas function
FVec <- function(v) v[1]^0.7*v[2]^0.3
x <- seq(0.1, 2, 0.1)
y <- seq(0.1, 2, 0.1)
z <- outer(x,y,F)
contour(x,y,z, asp  = 1, )

#Question: what point on the F = 0.8 contour line is closest to the origin?
f <- function(x,y) x^2 + y^2   #what we want to minimize

#We can solve the problem graphically by looking at contour lines of f
x <- seq(0.1, 2, 0.1)
y <- seq(0.1, 2, 0.1)
z <- outer(x,y,f)
contour(x,y,z, col = "red", add = TRUE, nlevels = 15)
#somewhere between 1.1 and 1.3 looks promising
contour(x,y,z, col = "green", levels = seq(1.1, 1.3, 0.04),  add = TRUE)

#Zoom in on the region that includes the solution
x <- seq(0.7, 1, 0.01)
y <- seq(0.5, 0.8, 0.01)
z <- outer(x,y,F)
contour(x,y,z, asp  = 1)
z <- outer(x,y,f)
contour(x,y,z, col = "red", add = TRUE)
contour(x,y,z, col = "green", levels = seq(1.15, 1.2, 0.01),  add = TRUE)

#The contour line f =1.18 looks good
z <- outer(x,y,F)
contour(x,y,z, asp  = 1, levels = 0.8)
z <- outer(x,y,f)
contour(x,y,z, col = "red", add = TRUE, levels = 1.18)

#We can read an approximate solution off the graph.
x0 = 0.90; y0 = 0.61
abline(v=x0, col = "green")
abline(h=y0, col = "green")
#We have found an approximate solution
#but it is vary hard to identify the exact point of tangency.

#Where the two contour lines are tangent, they have the same tangent space.
#The Jacobian matrices need not be equal, but they must be proportional.
DF <- function(x,y) c(0.7*x^(-0.3)*y^0.3, 0.3*x^0.7*y^(-0.7))
Df <- function(x,y) c(2*x, 2*y)
Df(x0,y0); DF(x0,y0)   #ratio is about 3 to 1
#Scale the second vector so that the first components are equal.
Df(x0,y0);DF(x0,y0)*1.8/0.623 #close but not the same.

#When we find the exact solution, Df will be proportional to DF.
#Work with the formulas for DF and Df as functions of x and y.
#A little algebra leads to the equation y =sqrt(3/7)*x.
abline(0, sqrt(3/7))

#Now we can find the exact solution.
#Use y =sqrt(3/7)*x to eliminate y from the constraint equation.
FF <- function(x) x^0.7*(sqrt(3/7)*x)^0.3 -0.8
curve(FF(x), from = 0.8, to = 1.1 )
abline(h=0, col = "green"); abline(v=x0, col = "blue")
FF(x0)
#Newton can do better
x1 = x0 - FF(x0)/grad(FF,x0); x1
#Now we can determine y1 by using the constraint
y1 <- (0.8/x1^0.7)^(1/0.3); y1

#Check this for our solution:
DF(x1,y1); Df(x1,y1)   #they look proportional
det(cbind(DF(x1,y1), Df(x1,y1))) #and they are!

#The coefficient L in Df = L*DF is the Lagrange multiplier.
L <- Df(x1,y1)[1]/DF(x1,y1)[1]; L; Df(x1,y1)[2]/DF(x1,y1)[2]

#The function g = f - L*F has an unconstrained extremum at our solution.
g <- function(x,y) f(x,y)-L*F(x,y)

x <- seq(0.85, 0.95, .01)
y <- seq(0.55, 0.65, .01)
z <- outer(x,y,g)
contour(x,y,z)    #typical behavior near a minimum.

abline(v=x1, col = "green")
abline(h=y1, col = "green")

#Finally we can display the solution on our contour-line plot.
Fval <- F(x1,y1); Fval     #the value of the constraint
fval <- f(x1,y1); fval     #squared distance from the origin

z <- outer(x,y,F)
contour(x,y,z, asp  = 1, levels = Fval)  #F = 0.8 is the constraint
z <- outer(x,y,f)
contour(x,y,z, col = "red", add = TRUE, levels = fval)
abline(v=x1, col = "green")
abline(h=y1, col = "green")

#Find a basis vector for the common tangent space.
#It is in the kernel of either of these:
DF(x1,y1); Df(x1,y1)
slope <- -DF(x1,y1)[1]/DF(x1,y1)[2]; slope

#Plot the common tangent line
x <- seq(0.86, 0.94, .001)
points(x, y1 + slope*(x-x1), col = "blue", pch =".", type = "l")

