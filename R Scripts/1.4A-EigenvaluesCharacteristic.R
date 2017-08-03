#Math 23 Script 1.4A-EigenvaluesCharacteristic.R
#Module 1, Week 4, subtopics 1.1 and 1.2

#Last modified September 17, 2014 by Paul Bamberg

par(mar=c(2,2,1,1)+0.1)

plot(NULL,xlim=c(-4,4),ylim=c(-4,4), xlab="", ylab="",axes = FALSE, asp = 1)
axis(1,pos=0); axis(2,pos=0)

#Topic 1 - Eigenvectors for a 2x2 matrix
#Here is the matrix from the Executive Summary
A <-matrix(c(-1,-2,4,5),2); A
#Let's see what it does to various vectors
#The direction of a standard basis vector gets changed
v1 <- c(1,0); w1 <- A%*%v1
arrows(0,0,v1[1],v1[2], col="red", lty = 2);arrows(0,0,w1[1],w1[2], col="red")

#The director of almost any other vector also gets changed
v2 <- c(0.6,-0.4); w2 <- A%*%v2
arrows(0,0,v2[1],v2[2], col="blue", lty = 2);arrows(0,0,w2[1],w2[2], col="blue")

#But here is an exception
v3 <- c(1,1); w3 <- A%*%v3
arrows(0,0,v3[1],v3[2], col="green", lty = 2);arrows(0,0,w3[1],w3[2], col="green")
#The vector v3 got multiplied by 3
#It is an eigenvector with eigenvalue 3

#Here is a second exception
v4 <- c(2,1); w4 <- A%*%v4
arrows(0,0,v4[1],v4[2], col="magenta", lty = 2)
arrows(0,0,w4[1],w4[2], col="magenta")
#The vector v4 got multiplied by 1
#It is an eigenvector with eigenvalue 1

#These two eigenvectors for a basis for R^2.
#They let us figure out what happens to any other vector. Example:
v5 <- v3 - v4
3*v3 - v4   #use the eigenvalues
A%*%v5      #use the matrix -- same answer

#Suppose we subtract off 3 times the identity from A
A%*%v3; I <- diag(c(1,1));  3*I*v3    #same results
(A - 3*I)%*%v3
#Since A-3I maps a nonzero vector into the zero vector
#it cannot be invertible
det(A - 3*I)    #and its determinant is zero

#Alternatively, subtract off 1 times the identity from A
(A - I)%*%v4     #same results on v4
#Since A- I maps a nonzero vector into the zero vector
#it cannot be invertible
det(A - I)    #and its determinant is zero
#Since any vector in R^2 is a linear combination of v3 and v4
#the product (A-3I)(A-I) = 0.
(A - 3*I)%*% (A-I)    #yes, it's the zero matrix'

#This is the characteristic equation approach to finding eigenvalues
#The characteristic polynomial is the determinant of A-tI
A
chi <- function(t) (-1-t)*(5-t)+8
curve(chi(x),from = 0, to = 4); abline(h=0, col = "red")
#The roots 1 and 3 are the eigenvalues

#In the 2x2 case it is easy to find the eigenvectors once we know the eigenvalues.
#The matrix A-3I has a one-dimensional kernel, spanned by v3
#Any vector is a linear combination of v3 and v4.
v <- 0.7*v3 + 1.5*v4
(A-3*I)%*%v   #a multiple of v4
(A-I)%*%v   #a multiple of v3

#Topic 2 - Not every 2x2 matrix has real eigenvalues
C <- matrix(c(1,2,-2,1),2); C  #a conformal matrix

#The characteristic polynomial is
chi <- function(t) (1-t)*(1-t)+4
curve(chi(x), from = - 1, to = 3); abline(h=0, col = "red")
#It is never less than 4; it has no real roots.

#A standard basis vector gets rotated and stretched
plot(NULL,xlim=c(-4,4),ylim=c(-4,4), xlab="", ylab="",axes = FALSE, asp = 1)
axis(1,pos=0); axis(2,pos=0)
v1 <- c(1,0); w1 <- C%*%v1
arrows(0,0,v1[1],v1[2], col="red", lty = 2);arrows(0,0,w1[1],w1[2], col="red")

#So does any other vector
v2 <- c(-1,1); w2 <- C%*%v2
arrows(0,0,v2[1],v2[2], col="blue", lty = 2);arrows(0,0,w2[1],w2[2], col="blue")
#Every vector is rotated through the same angle


#In this case the complex number that C represents is an eigenvalue
z <- complex(real = 1, imaginary = 2); z
I <- diag(c(1,1)); B = C - z*I; B  #a matrix with complex entries!
#We have to calculate the determinant by hand
B[1,1]*B[2,2]-B[1,2]*B[2,1]   #yes, it is zero


#Here is another troubling case.
A <- matrix(c(3,-1,1,5),2); A

#The characteristic polynomial is
chi <- function(t) (3-t)*(5-t)+1
curve(chi(x), from = 2, to = 6); abline(h=0, col = "red")
#The only root of the characteristic polynomial (t-4)^2 is 4.
#In this case there is an eigenvector with eigenvalue 4.
plot(NULL,xlim=c(-4,4),ylim=c(-4,4), xlab="", ylab="",axes = FALSE, asp = 1)
axis(1,pos=0); axis(2,pos=0)
v1 <- c(1,1); w1 <- A%*%v1
arrows(0,0,v1[1],v1[2], col="red", lty = 2);arrows(0,0,w1[1],w1[2], col="red")

#The director of every vector gets changed
v2 <- c(0.6,-0.4); w2 <- A%*%v2
arrows(0,0,v2[1],v2[2], col="blue", lty = 2);arrows(0,0,w2[1],w2[2], col="blue")
#You cannot find a second eigenvalue or a second independent eigenvector!



