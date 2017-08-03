#Math 23 Script 1.4C-Diagonalization.R
#Module 1, Week 4, subtopic 1.6

#Last modified September 13, 2014 by Paul Bamberg
library("pracma") # for rref()

#Topic 1: Basis of real eigenvectors
A <- matrix(c(1,-2,1,4),2); A
#Quickly find the eigenvectors and eigenvalues
w <- c(1,0); I <- diag(c(1,1))
T <- cbind(w, A%*%w, A%*%A%*%w); T
rref(T)
#So (A^2- 5A +6I)w = (A-2I)(A-3I)w = 0
v2 <- (A - 3*I)%*%w; v2   #eigenvector with eigenvalue 2
A%*%v2     #check
v3 <- (A - 2*I)%*%w; v3   #eigenvector with eigenvalue 3
A%*%v3     #check

#Now create a matrix P whose columns are the eigenvectors
#Since eigenvectors are specified only up to a scalar multiple,
#we can choose both eigenvectors to have a first component of 1.
P <- cbind(c(1,1),c(1,2)); P
#Check that A does the right thing to the two columns.
A%*%P    #multiplies first by 2, second by 3
#Calculate the inverse matrix
PInv <- solve(P)   #converts each eigenvector to a standard basis vector
D <- PInv%*%A%*%P; D   #a diagonal matrix of eigenvalues

#Do this step by step on a standard basis vector and it becomes obvious
w<-c(1,0); P%*%w    #eigenvector for eigenvalue 2
A%*%P%*%w           #eigenvector times eigenvalue
PInv%*%A%*%P%*%w    #standard basis vector times eigenvalue

#It is more useful to express A in terms of P and D
P%*%D%*%PInv        #the matrix A

#This works with matrices of any size, as long as there is a basis of real eigenvectors
#From script 1.4B
A <- matrix(c(2,0,0, 1,2,1,-1,0,1), 3);A
#we found these eigenvectors
v1 <- c(1,0,1)     #eigenvalue 1
w <- c(1,0,0); v2 <- c(1,1,1)  #both with eigenvalue 2
P <- cbind(v1,w,v2); D<-diag(c(1,2,2))
PInv <- solve(P)

P%*%D%*%PInv; A       #the matrix A

#Topic 2 - Raising a matrix to a power
#Let's raise that matrix A to the fifth power
#Start with D, which is easy
D5 <- diag(c(1,2,2)^5); D5
#Now it is easy to calculate the fifth power of A
P%*%D5%*%PInv
A%*%A%*%A%*%A%*%A


#Alternatively, we can find a cube root
DCube <- diag(c(1,2,2)^(1/3)); DCube
#Now it is easy to calculate the cube root of A
ACube <- P%*%DCube%*%PInv; ACube
ACube%*%ACube%*%ACube


#Topic 3 - Wnat if the eigenvalues are complex?
#It seems a shame to do complex arithmetic
#when you start with a real matrix and will end up with a real matrix

A <- matrix(c(3,2,-4,-1),2); A
#The characteristic polynomial is (3-t)(-1-t)+8 = t^2 -2t +5
#Its roots (the eigenvalues) are 1 + 2i and 1 - 2i
#The first of these is represented by the conformal matrix
C <- matrix(c(1,2,-2,1),2); C
#We can make a new basis whose first column is the first standard basis vector and whose
#second is made from the real part (1) and imaginary part(2) of the eigenvalue.
I <- diag(c(1,1)); p1 <- c(1,0); p2 <- (1/2)*(A - 1*I)%*%p1
P <- cbind(p1,p2); P
PInv <- solve(P); P%*%C%*%PInv

#Now matrix C is almost as easy as a diagonal matrix to raise to a power
z <- complex(real = 1, imaginary = 2)
z^5
C5 <- matrix(c(41,-38,38,41),2)
A5 <- P%*%C5%*%PInv; A5
A%*%A%*%A%*%A%*%A

#Topic 4 - What if there is no eigenbasis?
#Here is our troubling case from script 4.1A
A <- matrix(c(3,-1,1,5),2); A
#When searching for eigenvalues, we found the polynomial p(t) = (t -4)^2
(A-4*I)%*%(A-4*I) 
#A - 4I is called a "nilpotent matrix"
N <- A- 4*I; N%*%N   #its square is zero
A <- 4*I + N; A
#In this form A is easy to raise to the fifth power
A5 <- 4^5 * I + 5*4^4*I%*%N; A5
A%*%A%*%A%*%A%*%A

