#Math 23 Script 3.5D-SampleProblems.R

#Problem 5
library(numDeriv)

#The vector field
F1 <- function(x,y) x*y
F2 <- function(x,y) y^2

#Path 1 - the straight line
g1 <- function(t) t
g2 <- function(t) 1-t
a <- 0; b <- 1

integrand <- function(t) F1(g1(t),g2(t))*grad(g1,t)+  F2(g1(t),g2(t))*grad(g2,t)
integrate(integrand, a, b)  

#Path 2 - the circle
g1 <- function(t) sin(t)
g2 <- function(t) cos(t)
a <- 0; b <- pi/2

integrand <- function(t) F1(g1(t),g2(t))*grad(g1,t)+  F2(g1(t),g2(t))*grad(g2,t)
integrate(integrand, a, b)  
#Just what you need for your Physics 15b problem sets!