#Math 23 Script3.4E-SampleProblems.R

#Last modified November 20, 2014 by Paul Bamberg
library(lattice)
#Problem 1 done with wireframes
#Annoyingly, this only works if the passive variable is z

x <- seq(0,2.8,0.1) #near x = 2
y <- seq(0,5.8,0.1) #near y = 4
g <- expand.grid(x=x, y=y); head(g)
g$z <-  3*sqrt(9-g$x^2- g$y^2/4); head(g)
wireframe(z ~ x * y,g,  asp = c(1,1), xlim = c(0,3), ylim = c(0,6), zlim = c(0,9),
          scales = list(arrows = FALSE),     #gives tick marks on the axes
          drape = TRUE)                      #uses color 


#Problem 5
x <- seq(-1,2,0.1)
y <- seq(-1,2,0.1)
f <- function(x,y) x^2/2+y^3/3 -x*y
z <- outer(x,y,f)
contour(x,y,z)
filled.contour(x,y,z)
#It's clear there is a saddle point near (0,0), a minimum near (1,1)

#Zoom in on the saddle
y <- x <- seq(-0.1,0.1,0.01)
z <- outer(x,y,f)
contour(x,y,z)
filled.contour(x,y,z)

#Zoom in on the minimum
y <- x <- seq(0.9,1.1,0.01)
z <- outer(x,y,f)
contour(x,y,z)
filled.contour(x,y,z)
