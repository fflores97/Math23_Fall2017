#Math 23 Script 3.1C-DiffEquations.R

#Last modified October 25, 2014 by Paul Bamberg

#Topic 1 - Two real eigenvalues
#Since we know how to find the eigenvalues and eigenvectors for matrix A,
#we'll just construct the matrix from the eigenvalues and eigenvectors.
#You can change these to other values if you wish.
L1 <- 2; L2 <- -1   #the eigenvalues
v1 <- c(1,1); v2 <- c(-2,1)  #the eigenvectors
P <- cbind(v1, v2); D <- diag(c(L1,L2)); PInv <- solve(P)
A <- round(P%*%D%*%PInv, digits = 6); A

par(mar=c(4,1,1,1)) #leave room at bottom for the equation
plot(NULL, xlim = c(-4,4),ylim = c(-4,4), xlab = "", ylab = "", axes = FALSE, 
     asp = 1, pch = 20)
axis(1, pos = 0); axis(2, pos = 0)
mtext(paste("x.dot  =", A[1,1],"x + ", A[1,2], "y"),1,1)
mtext(paste("y.dot  =", A[2,1],"x + ", A[2,2], "y"),1,2)

#Calculate the exponential of At
Aexp <- function(t) P%*%diag(c(exp(L1*t),exp(L2*t)))%*%PInv
#Choose an initial value for the vector v
v0 <- c(2,0)
#Choose a sequence of times at which to plot the solution
times <- seq(from = 0, to =2, by = 0.1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20 )
v0 <- c(0,1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20, col = "green" )
#If v0 is an eigenvector the solution moves out or in along a line
for (t in times)
  points((Aexp(t)%*%v1)[1], (Aexp(t)%*%v1)[2], pch = 20, col = "red" )
for (t in times)
  points((Aexp(t)%*%v2)[1], (Aexp(t)%*%v2)[2], pch = 20, col = "blue" )

#The matrix A defines a vector field that specifies
#the velocity vector as a function of position.
plotvec = function(p) {
  velocity <- A%*%p
  arrows(p[1],p[2],p[1]+ 0.1*velocity[1], p[2]+ 0.1*velocity[2], length = 0.1)
}
plotvec(v0)

#We can draw a map of the vector field.
#Every vector is tangent to one of the solution curves.
for (x in -5:5)
  for (y in -3:3)
    plotvec(c(x,y))

#Topic 2 - A repeated real eigenvalue
L <- 2  #the only eigenvalue
v <- c(1.5,1) #the only eigenvector
#Make a nilpotent matrix whose image and kernel are both the eigenvector.
N <- cbind(v, -(v[1]/v[2])*v); N; N%*%N
A <- diag(c(L,L))+ N; A   #guaranteed to have a repeated eigenvalue of L.

plot(NULL, xlim = c(-4,4),ylim = c(-4,4), xlab = "", ylab = "", axes = FALSE, asp = 1, pch = 20)
axis(1, pos = 0); axis(2, pos = 0)
mtext(paste("x.dot  =", A[1,1],"x + ", A[1,2], "y"),1,1)
mtext(paste("y.dot  =", A[2,1],"x + ", A[2,2], "y"),1,2)

#Calculate the exponential of At
Aexp <- function(t) diag(c(exp(L*t),exp(L*t)))%*%(diag(c(1,1))+ t*N)
#Choose an initial value for the vector v
v0 <- c(2,0)
#Choose a sequence of times at which to plot the solution
times <- seq(from = 0, to =2, by = 0.1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20 )
v0 <- c(0,1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20, col = "green" )
#If v0 is the eigenvector the solution moves out or in along a line
for (t in times)
  points((Aexp(t)%*%v)[1], (Aexp(t)%*%v)[2], pch = 20, col = "red" )

#The matrix A defines a vector field that specifies
#the velocity vector as a function of position.
plotvec = function(p) {
  velocity <- A%*%p
  arrows(p[1],p[2],p[1]+ 0.1*velocity[1], p[2]+ 0.1*velocity[2], length = 0.1)
}

#We can draw a map of the vector field.
#Every vector is tangent to one of the solution curves.
for (x in -5:5)
  for (y in -3:3)
    plotvec(c(x,y))

#Topic 3 - Complex conjugate eigenvalues
a <-  -1  #the real part of the eigenvalue for spiraling in 
#a <-  0.2  #the real part of the eigenvalue for spiraling out 
#a <-  0  #the real part of the eigenvalue for circles or ellipses
b <-  2  #the imaginary part of the eigenvalue

#Make a conformal matrix from the eigenvalue
C <- matrix(c(a,b,-b,a),2); C
#First just use the conformal matrix
A <- C

plot(NULL, xlim = c(-4,4),ylim = c(-4,4), xlab = "", ylab = "", axes = FALSE, asp = 1, pch = 20)
axis(1, pos = 0); axis(2, pos = 0)
mtext(paste("x.dot  =", A[1,1],"x + ", A[1,2], "y"),1,1)
mtext(paste("y.dot  =", A[2,1],"x + ", A[2,2], "y"),1,2)

#Calculate the exponential of At
Aexp <- function(t) exp(a*t)*(cos(b*t)*diag(c(1,1)) + sin(b*t)*matrix(c(0,1,-1,0),2))
#Choose an initial value for the vector v
v0 <- c(4,0)
#Choose a sequence of times at which to plot the solution
times <- seq(from = 0, to =6, by = 0.1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20 )
v0 <- c(0,3)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20, col = "green" )

#We can draw a map of the vector field.
#Every vector is tangent to one of the solution curves.
for (x in -5:5)
  for (y in -3:3)
    plotvec(c(x,y))

#Now make a different matrix with the same complex eigenvalues
#The second column of P is what makes the difference
p22 <- 2
P <- matrix(c(1,0,1,p22), 2) ; PInv = solve(P)
A <- P%*%C%*%PInv

plot(NULL, xlim = c(-4,4),ylim = c(-4,4), xlab = "", ylab = "", axes = FALSE, asp = 1, pch = 20)
axis(1, pos = 0); axis(2, pos = 0)
mtext(paste("x.dot  =", A[1,1],"x + ", A[1,2], "y"),1,1)
mtext(paste("y.dot  =", A[2,1],"x + ", A[2,2], "y"),1,2)
#Show the direction of the second column of P
abline(0,p22, col = "red")

#Calculate the exponential of At
Aexp <- function(t) P%*%(exp(a*t)*(cos(b*t)*diag(c(1,1)) + sin(b*t)*matrix(c(0,1,-1,0),2)))%*%PInv
#Choose an initial value for the vector v
v0 <- c(2,0)
#Choose a sequence of times at which to plot the solution
times <- seq(from = 0, to =6, by = 0.1)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20 )
v0 <- c(0,3)
for (t in times)
  points((Aexp(t)%*%v0)[1], (Aexp(t)%*%v0)[2], pch = 20, col = "green" )

#We can draw a map of the vector field.
#Every vector is tangent to one of the solution curves.
for (x in -5:5)
  for (y in -3:3)
    plotvec(c(x,y))

