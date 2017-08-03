#Math 23 Script 3.5B-DerivativeVecField.R

#Last modified Decenber 1, 2014 by Paul Bamberg

library(numDeriv)

#Topic 1 - Plotting a vector field and its derivative in R^2

#For selected points in the domain, we represent the function value by an "anchored vector."
F1 <- function(x,y) x^2-y
F2 <- function(x,y) x*y - y
FVec <- function(v) c(v[1]^2 - v[2], v[1]*v[2] + v[2])
#Plot this in the square where x and y range from 0.5 to 2.6.
xSeq <- ySeq <- seq(0.5,2.6, 0.2)
par(mar = c(2,2,1,1))
plot(NULL, xlim = c(0.5,2.8), ylim = c(0.9,2.7)) #leave room for the arrows
scale <- 0.05 #controls the length of the arrows
for (x in xSeq) {
  for (y in ySeq) {
    arrows(x,y,x+scale*F1(x,y),y+scale*F2(x,y), length = 0.05)
  }
}
#Now focus on the derivative where x = 2, y = 2.
#This is just a numeric matrix.
DF <- jacobian(FVec, c(2,2)); DF
#We have a new linear vector field, that specifies the change in F
#as a function of the increment h.
hSeq <- seq (-0.1, 0.1, 0.02)
#Plot this the same way.
scale <- 0.05 #controls the length of the arrows
plot(NULL, xlim = c(-0.12,0.12), ylim = c(-0.12,0.12)) #leave room for the arrows
for (hx in hSeq) {
  for (hy in hSeq) {
    arrows(hx,hy,hx+scale*(DF%*%c(hx,hy))[1],hy+scale*(DF%*%c(hx,hy))[2], length = 0.05)
  }
}
#Inspection shows that it is "out" and "counterclockwise"
#and these properties result from the four numbers in the matrix.
#"Out" means that Df(h) is directed along h.
#For a single vector we can calculate a number to measure this property.
h <- c(1, -1)
sum(DF%*%h*h)    #the dot product is positive
#But we really should average over vectors in all directions
theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector of squared length 2

#First check that the average value of h[1]^2 and h[2]^2 is close to 1
#and that the average value of h[1]*h[2] is close to 0
N <- 10000; h1Sq <- h2Sq <- h1h2 <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector with h^2 = 2.
  h1Sq[i] <- h[1]^2
  h2Sq[i] <- h[2]^2
  h1h2[i] <- h[1]*h[2]
}
mean(h1Sq); mean(h2Sq); mean(h1h2) #should be close to 1, 1, 0

#Now compute outness for 10000 random directions
N <- 1000; outness <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector
  outness[i] <- sum(DF%*%h*h)   #positive means "out"
}
mean(outness)
DF; DF[1,1] + DF[2,2]
#The average outness is very nearly the sum of the diagonal entries (the trace).
#The trace of the derivative matrix is called the divergence of the field.

#Now recall that if the second column of a matrix is counterclockwise relative to the first,
#the determinant of the matrix is positive.
#For a single vector we can calculate a number to measure this property.
h <- c(1, -1)
det(cbind(h,DF%*%h))    #the determinant is positive
#Again, we should average over vectors in all directions.
#Do this 10000 times
N <- 10000; clock <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random unit vector
  clock[i] <- det(cbind(h,DF%*%h))    #positive means "counterclockwise"
}
mean(clock)
DF; DF[2,1] - DF[1,2]
#The average outness is very nearly the difference of the off-diagonal entries
#The difference of the off-diagonal entries of the derivative matrix is the curl of the field.

#Now repeat, using a famous example from physics.
#This is the electric field if you put a long thin line of + charge on the z axis.
F1 <- function(x,y) x/(x^2+y^2)
F2 <- function(x,y) y/(x^2+y^2)
FVec <- function(v) c(v[1]/(v[1]^2+v[2]^2), v[2]/(v[1]^2+v[2]^2))
#Plot this in the square where x and y range from 1 to 2.6.
xSeq <- ySeq <- seq(1,2.6, 0.2)
par(mar = c(2,2,1,1))
plot(NULL, xlim = c(1,3), ylim = c(1,3)) #leave room for the arrows
scale <- 0.2 #controls the length of the arrows
for (x in xSeq) {
  for (y in ySeq) {
    arrows(x,y,x+scale*F1(x,y),y+scale*F2(x,y), length = 0.05)
  }
}
#Focus on the derivative at the point x = 2, y = 2
#This is just a numeric matrix
DF <- jacobian(FVec, c(2,2)); round(DF, digits = 6) 
#This is a linear vector field. It specifies the change in F as a function of the increment h
hSeq <- seq (-0.1, 0.1, 0.02)
#Plot this the same way.
scale <- 1 #controls the length of the arrows
plot(NULL, xlim = c(-0.12,0.12), ylim = c(-0.12,0.12)) #leave room for the arrows
for (hx in hSeq) {
  for (hy in hSeq) {
    arrows(hx,hy,hx+scale*(DF%*%c(hx,hy))[1],hy+scale*(DF%*%c(hx,hy))[2], length = 0.05)
  }
}
#Inspection shows that it is neither "out" nor "counterclockwise"
#Again, these properties result from the four numbers in the matrix.
N <- 10000; outness <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector
  outness[i] <- sum(DF%*%h*h)   #positive means "out"
}
mean(outness)
round(DF, digits = 6)

N <- 10000; clock <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector
  clock[i] <- det(cbind(h,DF%*%h))    #positive means "counterclockwise"
}
mean(clock)
round(DF, digits = 6) 
#The curl is also zero.

#Topic 2 - the curl of a gradient is zero
#Start with any scalar field
f <- function(x,y) 2*x*exp(sqrt(x*y))
#Calculate its gradient
F1 <- function(x,y) 2*exp(sqrt(x*y))+ sqrt(x*y)*exp(sqrt(x*y))
F2 <- function(x,y) x^1.5*exp(sqrt(x*y))/sqrt(y)
FVec <- function(v) c(2*exp(sqrt(v[1]*v[2]))+ sqrt(v[1]*v[2])*exp(sqrt(v[1]*v[2])), v[1]^1.5*exp(sqrt(v[1]*v[2]))/sqrt(v[2]))
#Plot this in the square where x and y range from 1 to 2.6
xSeq <- ySeq <- seq(1,2.6, 0.2)
par(mar = c(2,2,1,1))
plot(NULL, xlim = c(1,3), ylim = c(1,3), asp = 1) #leave room for the arrows
z <- outer(xSeq,ySeq,f)
contour(xSeq,ySeq,z)
scale <- 0.002 #controls the length of the arrows
for (x in xSeq) {
  for (y in ySeq) {
    arrows(x,y,x+scale*F1(x,y),y+scale*F2(x,y), length = 0.05)
  }
}
#Focus on the derivative at the point x = 2, y = 2
#This is just a numeric matrix
DF <- jacobian(FVec, c(2,2)); round(DF, digits = 6) #the curl is zero
#We have a linear vector field. It specifies the change in F as a function of the increment h
hSeq <- seq (-0.1, 0.1, 0.02)
#Plot this the same way.
scale <- 0.01 #controls the length of the arrows
plot(NULL, xlim = c(-0.12,0.12), ylim = c(-0.12,0.12)) #leave room for the arrows
for (hx in hSeq) {
  for (hy in hSeq) {
    arrows(hx,hy,hx+scale*(DF%*%c(hx,hy))[1],hy+scale*(DF%*%c(hx,hy))[2], length = 0.05)
  }
}
#Inspection shows that it is not "counterclockwise"
#What about the divergence?
N <- 10000; outness <- numeric(N)
for (i in 1:N) {
  theta <- runif(1, min= 0, max = 2*pi); theta   #a random angle
  h <- sqrt(2)*c(cos(theta), sin(theta)); h #a random vector
  outness[i] <- sum(DF%*%h*h)   #positive means "out"
}
mean(outness)
round(DF, digits = 6); signif(DF[1,1] + DF[2,2])
#Again, the average "outness" is nearly equal to the divergence

#Topic 3 - Divergence in three dimensions
#Generate a random increment vector that has equal probability for every direction
#Again, we want the expectation of h_i^2 to be 1
#and the expectation of h_ih_j to be zero
z <- runif(1,min = -1, max = 1)   #all values of z equally likely
theta <- runif(1, min = 0, max = 2*pi)   #all longitudes equally likely
#Test that we are getting the correct expectations
N <- 10000; h1Sq <- h2Sq <- h3Sq<- h1h2 <- h1h3 <- h2h3 <- numeric(N)
for (i in 1:N) {
  z <- runif(1,min = -1, max = 1)
  theta <- runif(1, min = 0, max = 2*pi)
  h <- sqrt(3)*c(sqrt(1-z^2)*cos(theta), sqrt(1-z^2)*sin(theta),z);
  h1Sq[i] <- h[1]^2
  h2Sq[i] <- h[2]^2
  h3Sq[i] <- h[3]^2
  h1h2[i] <- h[1]*h[2]
  h1h3[i] <- h[1]*h[3]
  h2h3[i] <- h[2]*h[3]
}
mean(h1Sq); mean(h2Sq); mean(h3Sq) #should be close to 1, 1, 1
mean(h1h2); mean(h1h3); mean(h2h3); #should be close to 0, 0, 0

#We have to give up on plotting the field, but we can still do "average outness"
F1 <- function(x,y,z) xyz - 2x^2
F2 <- function(x,y,z) yz + xy^2
F3 <- function(x,y,z) x*sqrt(y*z)
FVec <- function(v) c(v[1]*v[2]*v[3] - 2*v[1]^2, v[2]*v[3] + v[1]*v[2]^2, v[1]*sqrt(v[2]+v[3]))
#Focus on the derivative at the point x = 2, y = 2, z= 2
DF <- jacobian(FVec, c(2,2,2)); round(DF, digits = 6)
N <- 100000; outness <- numeric(N)
for (i in 1:N) {
  z <- runif(1,min = -1, max = 1)
  theta <- runif(1, min = 0, max = 2*pi)
  h <- sqrt(3)*c(sqrt(1-z^2)*cos(theta), sqrt(1-z^2)*sin(theta),z);
  outness[i] <- sum(DF%*%h*h)   #positive means "out"
}
mean(outness); (DF[1,1] +DF[2,2] + DF[3,3])
#The divergence is nearly equal to the average "outness"

#In three dimensions the curl can be defined by the cross product.
#Here is our function to calculate the cross product.
"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2],v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])  


h <- c(1, -1,1); h #vector with h^2  = 3
h%x%(DF%*%h)    #the average of this quantity is the curl

#Now do this many times
N <- 100000; curls <- matrix(nrow = N, ncol = 3)
for (i in 1:N) {
  z <- runif(1,min = -1, max = 1)
  theta <- runif(1, min = 0, max = 2*pi)
  h <- sqrt(3)*c(sqrt(1-z^2)*cos(theta), sqrt(1-z^2)*sin(theta),z);
  curls[i,] <- h%x%(DF%*%h) 
}
curl <- apply(curls, 2, mean); curl #average the results
round(c(DF[3,2]-DF[2,3], DF[1,3]-DF[3,1], DF[2,1]-DF[1,2]),digits = 6) #formula for the curl

