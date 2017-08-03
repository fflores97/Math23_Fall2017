#Math 23 Script 1.4X-EigenvectorApplications.R
#Module 1, Week 4, subtopic 1.8

#Last modified September 17, 2014 by Paul Bamberg
library("pracma")

#Topic 1 - The special case of a symmetric matrix
#Illustrating two properties, both easy to prove in the 2x2 case:
#All the eigenvalues are real. (proved in Hubbard chapter 3 using lots of analysis)
#Eigenvectors for distinct eigenvalues are orthogonal. (not hard to prove in general)
A<- matrix(c(1,2,3,4,2,-2,1,-3,3,1,-2,2,4,-3,2,1),4); A
identical(t(A),A)    #it is symmetric
w <- c(1,0,0,0)
T <- cbind(w, A%*%w, A%*%A%*%w, A%*%A%*%A%*%w, A%*%A%*%A%*%A%*%w); T
pCoef <- rref(T)[,5]  #the last column
p <- function(t) t^4 - pCoef[4]* t^3 - pCoef[3]*t^2 - pCoef[2]*t - pCoef[1]   #real-world problems come out like this!
curve(p(x), from = -8, to =8); abline(h=0, col="red")
#Now we have to find the four eigenvalues
lam1 <- uniroot(p, c(5,7))$root;lam1; p(lam1)
lam2 <- uniroot(p, c(-0,3))$root;lam2; p(lam2)
lam3 <- uniroot(p, c(-5,-1))$root;lam3; p(lam3)
lam4 <- uniroot(p, c(-8,-4))$root;lam4; p(lam4)
#We can compare with the built-in function
eigen(A)$values
lam1; lam2; lam3; lam4

#We can now get an eigenvector for each eigenvalue.
#It is conventional to make each be a unit vector.
I <- diag(c(1,1,1,1))
v <- (A-lam2*I )%*%(A-lam3*I )%*%(A-lam4*I )%*%w; v1<-v/sqrt(sum(v^2))
A%*%v1; lam1*v1    #it checks!
v  <- (A-lam1*I )%*%(A-lam3*I )%*%(A-lam4*I )%*%w; v2<-v/sqrt(sum(v^2))
v  <- (A-lam1*I )%*%(A-lam2*I )%*%(A-lam4*I )%*%w; v3<-v/sqrt(sum(v^2))
v  <- (A-lam1*I )%*%(A-lam2*I )%*%(A-lam3*I )%*%w; v4<-v/sqrt(sum(v^2))
P <- cbind(v1,v2,v3,v4); PInv <-solve(P); D <- diag(c(lam1,lam2,lam3, lam4))
round(P%*%D%*%PInv, digits = 4);A  #we diagonalized A!!
round(t(P) %*%P, digits = 4)   #the columns are orthonormal
eigen(A)$vectors; P            #and we agree with the built-in function

#Note: you need math skills to prove that this will always work
#plus computing skills to get the eigenvectors and eigenvalues.

#Topic 2 - Markov Process (from script 1.1D)
w <- c(1,0)  #this is legal; it means team 1 is serving for sure.

#The transition matrix
F <- matrix(c(0.8, 0.2, 0.3, 0.7),2); F

#Let's find the eigenvalues
T <- cbind(w, F%*%w, F%*%F%*%w)
pCoef <-rref(T)[,3]
p <- function(t) t^2 - pCoef[2]*t - pCoef[1]
curve(p(x), from = 0, to = 2); abline(h=0)
#Eigenvalues are 1 and 0.5
I <- diag(c(1,1))
v1 <- (F - 0.5*I)%*%w; v1
v1 <- 2*v1   #eigenvector for eigenvalue 1
v2 <- (F - 1*I)%*%w; v2  #eigenvector for eigenvalue 2

par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(-0.3,1), ylim = c(0,1), xlab = "", ylab= "",
     asp =1,axes = FALSE) #asp=1 gives a 1:1 aspect ratio
axis(1,pos = 0); axis(2,pos = 0) #axes blow and to the left
abline(1,-1, col = "red")
#Here are the eigenvectors.
arrows(0,0, v1[1], v1[2], col = "red")
arrows(0,0, v2[1], v2[2], col = "red")

#Here is the starting situation.
arrows(0,0, w[1], w[2], col = "blue")
#Now play the first point.
w1 <- F %*% w; w1
arrows(0,0, w1[1], w1[2], col = "blue")
#The difference w0-v1 was along the direction of v2 and got multiplied by 0.5
#Now play the second point.
w2 <- F %*% w1; w2
arrows(0,0, w2[1], w2[2], col = "blue")
#Each point, the component along eigenvector v2 is multiplied by eigenvalue 0.5

#Topic 3 - Eigenvectors for a reflection
#Here is a reflection matrix 
F <- matrix(c(1/9,4/9,8/9,4/9,7/9,-4/9,8/9,-4/9,1/9),3);F
round(t(F)%*%F); det(F)   #it's a reflection 

#Let's find the eigenvalues
w <- c(1,0,0); I  <- diag(c(1,1,1))
T <- cbind(w, F%*%w, F%*%F%*%w); T
rref(T) #A^2 - I = (A-I)(A+I) = 0
#Eigenvalues are -1 and 1
v1 <- (F - I)%*%w; v1  #eigenvector for eigenvalue -1
F%*%v1    #it is perpendicular to the plane
v2 <- (F + I)%*%w; v2   #eigenvector for eigenvalue +1
F%*%v2  #it is in the plane of reflection

#To get a third eigenvector we have to start over.
#The second standard basis vector produced nothing new.
w <- c(0,0,1); I  <- diag(c(1,1,1))
T <- cbind(w, F%*%w, F%*%F%*%w); T
rref(T)    #A^2 - I = (A-I)(A+I) = 0
#Eigenvalues are -1 and 1
v1x <- (F - I)%*%w; v1x; v1  #eigenvector for eigenvalue -1 is not new
v3 <- (F + I)%*%w; v3   #another eigenvector for eigenvalue +1
F%*%v3  #it is in the plane of reflection
P <- cbind(v1,v2,v3); P    #columns are a basis of eigenvectors
round(solve(P)%*%F%*%P)
#Relative to the new basis, the reflection 
#just reverses the first basis vector and leaves the others unchanged
sum(v2*v3)  #equal eigenvalues - need not be orthogonal
 
#Topic 4 - Sequences defined by linear recurrences
a <- numeric(30)  #an empty vector
a[1] <- 1; a[2] <- 2
a[3] <- a[1]+a[2] ; a
#We can express the rule by using matrices
A <-matrix(c(0,1,1,1),2);A
a[3:4] <- A%*%a[2:3]; a
a[5:6] <- A%*%A%*%a[3:4]; a  #two steps at once

#Let's raise A to a high power using eigenvectors
w <- c(1,0) ; I <- diag(c(1,1))
T <- cbind(w, A%*%w, A%*%A%*%w); T
#So p(t) = t^2 - t - 1
#Solve by the quadratic formula as in the textbook, or
p <- function(t) t^2 - t - 1
curve(p(x),from = -1, to = 2); abline(h=0, col = "red")
lam1 <- uniroot(p, c(1,2))$root; lam1; (1+sqrt(5))/2
lam2 <- uniroot(p, c(-1,0))$root; lam2; (1-sqrt(5))/2
#Now find the eigenvectors
v1 <- (A - lam2*I)%*%w; v2 <- (A - lam1*I)%*%w
#Construct the change-of-basis matrix
P <- cbind(v1,v2); PInv <- solve(P); D<- diag(c(lam1,lam2));P;D
round(P%*%D%*%PInv)   #strange way to write A
#It's easy to raise D to the twentieth power
D20 <- D^20; D20
A20 <- round(P%*%D20%*%PInv); A20

a[25:26] <- A20%*%a[5:6]; a  #twenty steps at once

