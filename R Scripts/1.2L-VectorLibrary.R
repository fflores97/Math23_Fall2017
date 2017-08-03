#Math 23 Script 1.2L-VectorLibrary.R

#Last modified: September 3, 2014 by Paul Bamberg

#To use this library, include source("1.2L-VectorLibrary.R") at the top of your script.
#Be sure that you have set your working directory to your scripts folder.

#Topic 1 - Some useful angles and basis vectors
#Standard basis vectors in two dimensions
e1.2 <- c(1,0)   #unit length, points east
e2.2 <- c(0,1)   #unit length, points north

#Standard basis vectors in three dimensions
e1.3 <- c(1,0,0)   #unit length, points east
e2.3 <- c(0,1,0)   #unit length, points north
e3.3 <- c(0,0,1)   #unit length, points up

#Angles in Pythagorean triangles
A16 <- acos(24/25)*180/pi
A22.5 <- acos(12/13)*180/pi
A28 <- acos(15/17)*180/pi
A37 <- acos(4/5)*180/pi
A53 <- acos(3/5)*180/pi
A62 <- acos(8/17)*180/pi
A67.5 <- acos(5/13)*180/pi
A74 <- acos(7/25)*180/pi

#Angles with rational tangents and cotangents
A11.25 <- atan(1/5)*180/pi
A14 <- atan(1/4)*180/pi
A18.5 <- atan(1/3)*180/pi
A26.5 <- atan(1/2)*180/pi

#Topic 2 - Functions for working with angles in degrees

#Dot product operator
"%.%" <- function(x,y) sum(x*y)

#Cross product operator
"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2],v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])

#Length of a vector
Norm<- function(v) sqrt(v %.% v)   

#Angle in degrees between two vectors
Acos <- function(x) acos(x)*180/pi
angleBetween <- function(x,y) Acos((x%.%y)/(Norm(x)*Norm(y)))

#More inverse trig functions that give values in degrees.
Asin <- function(x) asin(x)*180/pi
Atan <- function(x) atan(x)*180/pi

#Make a vector from its length and its direction north of east
make2Dvec <- function(len, angle) {
  c(len*cos(angle*pi/180),len*sin(angle*pi/180))
}

#Trig functions for angles in degrees
Sin  <- function(x) sin(x*pi/180)
Cos  <- function(x) cos(x*pi/180)
Tan  <- function(x) tan(x*pi/180)
Cot  <- function(x) cot(x*pi/180)

#Make a random triangle in a square of side edge (don't worry about how this works)
makeTriangleABC <- function(edge) {
  repeat {   #keep trying until area exceeds 1/4 of plot region
    x <- runif(3,-edge,edge)  #random x coordinates of vertices
    y <- runif(3,-edge,edge)  #random y coordinates of vertices
    Area <- abs(det(matrix(c(x[2]-x[1],y[2]-y[1],x[3]-x[1],y[3]-y[1]),2))/2)
    if (Area > edge^2) break
  }
  return (list(c(x[1],y[1]),c(x[2],y[2]),c(x[3],y[3])))
}

#Determinant as a function of the columns of a matrix
det2 <- function(c1,c2) det(cbind(c1,c2))
det3 <- function(c1,c2,c3) det(cbind(c1,c2,c3))
