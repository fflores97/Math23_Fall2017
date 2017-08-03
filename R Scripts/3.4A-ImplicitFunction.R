#Math 23 Script 3.4A-ImplicitFunction.R

#Last modified November 15, 2014 by Paul Bamberg
library(numDeriv)

#Topic 1 - Three variables, one constraint
#Use the equation x^3 - 3xy - log(y)^2 + xyz^2 = 10.
F <- function(v) v[1]^3 - 3*v[1]*v[2] - log(v[2])^2 +v[1]*v[2]*v[3]^2 - 10
v0 <- c(2,1,2); F(v0)     #we have a solution
#Near v0, F defines passive variable x implicitly as a function x = g(y,z)
#Use the derivative recipe.
DF <- grad(F, v0); DF
#Write DF = [A | B] where A = 13 and B = [2 8]
#The derivative of g is -A^{-1)B
Dg <- -c(DF[2], DF[3])/DF[1]; Dg
#Suppose we change y to 1.05, z to 1.97. What x satisfies the constraint?
h <- c(0.05, -0.03)    #the increment to y and z
v <- v0 + c(Dg%*%h, h); v
F(v)      #pretty close to zero

#Topic 2 - Three variables, two constraints
#Two constraints: x^2y + y^2z - xz = 11
#                 xyz - x - y -z = 0
F <- function(v) c(v[1]^2*v[2] + v[2]^2*v[3] - v[1]*v[3] - 11, v[1]*v[2]*v[3] - v[1]-v[2]-v[3] )
v0 <- c(1,2,3); F(v0)     #we have a solution
#Near v0, F defines passive variables x and y implicitly as a function of z.
#Use the same derivative recipe.
DF <- jacobian(F, v0); DF
#Write DF = [A | B] where A = 13 and B = [2 8]
A <- DF[,1:2]; B<- DF[,3]; A; B
AInv <- solve(A)
#The derivative of g is -A^{-1)B
#Suppose we change z to 3.14. What x and y satisfy the constraints?
h <- 0.14    #the increment to z
v <- v0 + c(-AInv%*%B%*%h,h); v
F(v)      #pretty close to zero

