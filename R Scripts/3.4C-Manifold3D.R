#Math 23 Script3.4C-Manifolds3D.R

#Last modified Nov. 15, 2014 by Paul Bamberg
#install.packages("lattice")  #comment this out after running it once
library(lattice)
#install.packages("scatterplot3d")  #comment this out after running it once
library(scatterplot3d)

#Topic 1 - A manifold as a function graph

#Near the North Pole z (passive) is a smooth function of x and y (active)
#Create a "data frame" (list of columns) with columns named x and y
g <- expand.grid(x=seq(from = -0.6, to =  0.6, by = 0.05),y=seq(from = -0.6, to =  0.6, by = 0.05))
head(g)
#Calculate the values in the third column
g$z <- sqrt((1-g$x^2-g$y^2))
head(g)
wireframe(z ~ x * y, g, asp = c(1,1), xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
          scales = list(arrows = FALSE),     #gives tick marks on the axes
          drape = TRUE)                      #uses color 


#To draw the entire sphere we need several function graphs
#The "groups" feature will let us draw two
#We must evaluate the square root only for positive values,
#since the square root function is not differentiable at zero.
g <- expand.grid(x=seq(from = -0.9, to =  0.9, by = 0.05),
                 y=seq(from = -0.9, to =  0.9, by = 0.05), gr = 1:2)
g$z <- (-1)^g$gr*sqrt(ifelse(g$x^2 + g$y^2 > 0.95, 0, (1-g$x^2-g$y^2)))
wireframe(z ~ x * y, g, groups = gr, asp = c(1,1), xlim = c(-1,1), ylim = c(-1,1), zlim = c(-1,1),
          scales = list(arrows = FALSE),     #gives tick marks on the axes
          drape = TRUE)                      #uses color 

#To get the complete manifold, we would also need to add
#two graphs of y as a function of x and z
#and two graphs of x as a function of y and z.
#Unfortunately, wireframe() requires an evenly spaced grid for the active variables.

#Here is the surface depicted in Hubbard, Figure 3.1.11
g <- expand.grid(x=seq(from = -2, to =  2, by = 0.05),y=seq(from = -2, to =  2, by = 0.05))
#Calculate the values in the third column
g$z <- g$x^2 - 0.2*g$x^4 - g$y^2
wireframe(z ~ x * y, g, asp = c(1,1), xlim = c(-2,2), ylim = c(-2,2), zlim = c(-2,2),
          scales = list(arrows = FALSE),     #gives tick marks on the axes
          drape = TRUE)                      #uses color 



#If the manifold is one dimensional there is only one active variable.
#We need 
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Helix", pch = 20)

#An alternative is to make a data frame with columns for x, y, and z 
#and use cloud() with the same syntax as for wireframe()

g <- data.frame(z = seq(-10, 10, 0.01)); head(g)
g$x <- cos(z)
g$y <- sin(z); head(g)
cloud(z ~ x * y, g, pch = ".")



#Topic 2 - Graphing a parametrized manifold

n <- 18
#Latitudes every 10 degrees from the south pole to the north pole
u <- seq (from = -pi/2, to = pi/2, length.out = n+1); u
#Longitudes every 10 degrees going east from the prime meridian
v <- seq (from = 0, to = 2*pi, length.out = 2*n+1); v
#Now we must make a data frame of pairs of parameter values as we did for contour
g <- expand.grid(u=u,v=v); head(g)
#Next compute x, y, and z as functions of the  parameter pairs
g$x = cos(g$u)*cos(g$v)
g$y = cos(g$u)*sin(g$v)
g$z = sin(g$u)
head(g)
#We need to convert the last three columns to matrices
x <- matrix(g$x, length(u))
y <- matrix(g$y, length(u))
z <- matrix(g$z, length(u))
#Now wireframe will plot the parametrized sphere
wireframe(z ~ x * y, scales = list(arrows = FALSE)) 


#Here is the surface (not a manifold) from Figure 3.1.15
n = 50
u <- seq (from = -5, to = 5, length.out = n+1)
v <- seq (from = -5, to = 5, length.out = n+1)
#Now we must make a data frame of pairs of parameter values as we did for contour
g <- expand.grid(u=u,v=v); head(g)
#Next compute x, y, and z as functions of the  parameter pairs
g$x = g$u^3*cos(g$v)
g$y = g$u^2+g$v^2
g$z = g$v^2*cos(g$u)
#We need to convert the last three columns to matrices
x <- matrix(g$x, length(u))
y <- matrix(g$y, length(u))
z <- matrix(g$z, length(u))
#Now wireframe will plot the parametrized surface
wireframe(z ~ x * y, scales = list(arrows = FALSE)) 
#The plot looks different because in the textbook, y is plotted vertically.

#The same approach works for a parametrized one-dimensional manifold.
#Parametrize a helix with a parameter t.
#The coils will get farther apart as t increases
t <- seq(from = 0, to = 8*pi, by = 0.01); head(t)
#Now make a data frame with columns for x, y, and z
g <- data.frame(t = t); head(g)
g$x <- cos(t)
g$y <- sin(t)
g$z <- t^2; head(g)
cloud(z ~ x * y, g, pch = ".", scales = list(arrows = FALSE))


#Topic 3 - Graphing a manifold that is specified as a locus

#Here is a brute-force way to create a data frame where each row is a point on the unit sphere
s <- seq(from = -1, to = 1, by = 0.02)
grid = expand.grid(x=s,y=s,z=s); head(grid)
g <- subset(grid, subset = abs(x^2+y^2+z^2-1)< 0.05)
head(g)
cloud(z ~ x * y, g, pch = ".", scales = list(arrows = FALSE))

#An alternative "Monte Carlo" approch is to generate random points
#and keep just the ones that are close to satisfying the constraint
N <- 10000
x <- numeric(N); y <- numeric(N); z <- numeric(N)
i = 1;
repeat {
  v <- runif(3, min = -1, max = 1)  #3 random coordinates
  if (abs (sum(v^2)-1) < 0.02) {
    x[i] <- v[1]; y[i] <- v[2]; z[i] <- v[3]
    i <- i + 1
    if (i > N) break   #we have found N points on the manifold
  }
}
g <- data.frame(x = x, y = y, z = z); head(g)
cloud(z ~ x * y, g, pch = ".", cex = 2, scales = list(arrows = FALSE))

#Here is the 2-dimensional manifold of Example 3.1.13
#It is the locus of F(x,y,z) = sin(x + yz) = 0
#D1F = cos(x + yz) = 1 or -1 for any point of the manifold
s <- seq(from = -2, to = 2, by = 0.02)
grid = expand.grid(x=s,y=s,z=s)
g <- subset(grid, subset = abs(sin(x + y*z))< 0.02)
cloud(z ~ x * y, g, pch = ".", scales = list(arrows = FALSE))




#This approach can also be used if there are two constraints.
#We can look for the intersection of the sphere with the plane x+2y+3z = 0.5
N <- 200   #have to try lots of points to find one good one
x <- numeric(N); y <- numeric(N); z <- numeric(N)
i = 1;
repeat {
  v <- runif(3, min = -1, max = 1)  #3 random coordinates
  if (abs (sum(v^2)-1) < 0.02 && abs(v[1] + 2*v[2] + 3*v[3] - 0.5) < 0.02) {
    x[i] <- v[1]; y[i] <- v[2]; z[i] <- v[3]
    i <- i + 1
    if (i > N) break   #we have found N points on the manifold
  }
}
g <- data.frame(x = x, y = y, z = z); head(g) #takes a few seconds
cloud(z ~ x * y, g, pch = 20, cex = 2, scales = list(arrows = FALSE))
#The manifold is a circle.

#A better approach (term project?)
#Once you find one point, use the tangent space to find a few nearby points.
#Perhaps even better, when you find a point close to the manifold,
#use Newton's method to find a point that is right on the manifold.
#Then make a small random change in the active variable(s)
#and use Newton's method to find the right change in the passive variables.
