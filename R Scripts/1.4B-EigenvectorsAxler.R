#Math 23 Script 1.4B-EigenvectorsAxler.R
#Module 1, Week 4, subtopics 1.3, 1.4 and 1.5

#Last modified September 17, 2014 by Paul Bamberg
#This modern approach was introduced by Sheldon Axler in 1995: "Down with Determinants!"

library("pracma")   #for rref()

#Topic 1 - Finding eigenvectors by row reduction
A <-matrix(c(-1,-2,4,5),2); A   #the same old matrix
#Start with the first standard basis vector and apply A twice.
w <- c(1,0); w; A%*%w; A%*%A%*%w
#These three vectors in R^2 have to be linearly dependent.
#Confirm this by forming a matrix.
T <- cbind( w, A%*%w, A%*%A%*%w); T
#Row reduce to find the exact form of the dependence.
rref(T)
#So A^2w = -3Iw + 4Aw, or A^2w -4Aw + 3Iw = (A^2-4A+3I)w = 0.
#We have discovered the polynomial
p <- function(t) t^2 - 4*t + 3
#which in this case is the same as the characteristic polynomial.
#Since p(t)=(t-1)(t-3), p(A)=(A-I)(A-3I)
I <- diag(c(1,1))
(A - I)%*%(A-3*I)%*%w    #has to be zero
#Now we can find two eigenvectors
v1 <- (A-3*I)%*%w     #eigenvector for eigenvalue 1
A%*%v1; v1
#Reverse the order of the factors.
(A - 3*I)%*%(A-I)%*%w    #has to be zero
v2 <- (A-I)%*%w     #eigenvector for eigenvalue 3
A%*%v2; 3*v2

#If we use a different w, we learn nothing more
w <- c(0,1); w; A%*%w; A%*%A%*%w
T <- cbind( w, A%*%w, A%*%A%*%w); T #a different matrix
rref(T)
#Again p(t)=(t-1)(t-3), p(A)=(A-I)(A-3I)
v1x <- (A-3*I)%*%w     #eigenvector for eigenvalue 1
A%*%v1x; v1x
v2x <- (A-I)%*%w     #eigenvector for eigenvalue 3
A%*%v2x; 3*v2x
#We found eigenvectors that are proportional to the previous ones
v1; v1x
v2; v2x

#What could have gone wrong?
w <- c(1,1); w; A%*%w  #already dependent
T <- cbind( w, A%*%w); T #a matrix with rank 1 
rref(T)
#So Aw = 3w
#Now p(t)=(t-3), p(A)=(A-3I)
#w is an eigenvector for eigenvalue 3
w;  A%*%w

#To find a second independent eigenvector we must start again with a different w.

#What if there are fewer than two distinct eigenvalues?
#Case 1:
A <- diag(c(4,4)); A
w1 <- c(1,0); w1; A%*%w1  #already dependent
T <- cbind( w1, A%*%w1); T #a matrix with rank 1 
rref(T)
#So p(t) = t-4, p(A) = A - 4I
w1; A%*% w1   #an eigenvector with eigenvalue 4
#Look for a second independent eigenvector.
w2 <- c(0,1); w2; A%*%w2  #already dependent
T <- cbind( w2, A%*%w2); T #a matrix with rank 1 
rref(T)
#Again p(t) = t-4, p(A) = A - 4I
w2; A%*% w2   #a different eigenvector with eigenvalue 4
#p(t) was always simple, and we found a basis of eigenvectors.

#Case 2:
A <- matrix(c(3,-1,1,5),2); A      
w <- c(1,0); w; A%*%w; A%*%A%*%w
T <- cbind( w, A%*%w, A%*%A%*%w); T 
rref(T)
#So A^2w = -16Iw + 8Aw; (A^2 -8A + 16I)w = 0
#The polynomial is p(t) = t^2 - 8t - 16 = (t-4)^2
v<-(A-4*I)%*% w   #an eigenvector with eigenvalue 4
A%*% v; 4*v    #check that it's an eigenvector
#We look in vain for a second independent eigenvector.
w <- c(0,1); w; A%*%w; A%*%A%*%w
T <- cbind( w, A%*%w, A%*%A%*%w); T #a different matrix
rref(T)     #not quite echelon form, but we can use it.
#Again, the polynomial is p(t) = t^2 - 8t - 16 = (t-4)^2
v2<-(A-4*I)%*% w   #an eigenvector with eigenvalue 4
v; v2   #but it's the same eigenvector
#p(t) was not simple, and we could not find a basis of eigenvectors.

#Topic 2 - Eigenvectors for a 3 x 3 matrix
#This is example 2.7.5 from the Hubbard textbook.
A <- matrix(c(1,-1,0,-1,2,-1,0,-1,1), 3);A
#Any vector will do for w. An obvious choice is the first standard basis vector
w <- c(1,0,0)
T <- cbind(w, A%*%w, A%*%A%*%w, A%*%A%*%A%*%w); T
rref(T)
#So A^3 - 4A^2 + 3A = 0
p <- function(t) t^3 - 4*t^2 + 3*t
curve(p(x), from = -1, to = 4); abline(h=0, col = "red")
#Since P has three distinct real roots 0, 1, 3, we will get three eigenvectors.
I <- diag(c(1,1,1))
v0 <- (A - I)%*%(A - 3*I)%*%w; v0
A%*%v0; 0*v0      #eigenvector for eigenvalue 0
v1 <- A%*%(A - 3*I)%*%w; v1
A%*%v1; 1*v1      #eigenvector for eigenvalue 1
v3 <- A%*%(A - 1*I)%*%w; v3
A%*%v3; 3*v3      #eigenvector for eigenvalue 3

#Now rerun the script, choosing the sum of v1 and v3 as w. You will not find v0.
w <- v1+v3
T <- cbind(w, A%*%w, A%*%A%*%w); T
rref(T)
#So A^2 - 4A  + 3I = 0
p <- function(t) t^2 - 4*t + 3
curve(p(x), from = -1, to = 4); abline(h=0, col = "red")
#Since P has two distinct real roots 1 and 3, we will get only two eigenvectors.
I <- diag(c(1,1,1))
v1 <- (A - 3*I)%*%w; v1
A%*%v1; 1*v1      #eigenvector for eigenvalue 1
v3 <- (A - 1*I)%*%w; v3
A%*%v3; 3*v3      #eigenvector for eigenvalue 3
#Rerun, using any w not in the subspace spanned by v1 and v3,
#and you will find the eigenvector v0.

#Here is a more interesting example.
A <- matrix(c(2,0,0, 1,2,1,-1,0,1), 3);A
w1 <- c(1,0,0)
T <- cbind(w1, A%*%w1); T
rref(T)
#So A-2I = 0 and w1 is an eigenvector with eigenvalue 2.

w2 <- c(0,1,0)
T <- cbind(w2, A%*%w2, A%*%A%*%w2); T
rref(T)
#Now (A^2 - 3A +2I)w2 = (A-I)(A-2I)w2 = 0
v1 <- (A-2*I)%*%w2
A%*%v1; v1   #v1 is an eigenvector with eigenvalue 1.

v2 <- (A-I)%*%w2
A%*%v2; v2   #v2 is a new eigenvector with eigenvalue 2.

#We have found a basis of eigenvectors.
#Because there are only two distinct eigenvalues, no single choice of w
#will give us the whole basis. p(t) is simple and cannot be cubic.

#Here is an example where there is no eigenbasis.
J<-matrix(c(2,0,0,1,2,0,0,1,2),3);J
R<-matrix(c(0,1,1,1,0,1,1,1,0),3);R
RI<-solve(R); RI
A <- R %*% J %*% RI; A
A<-matrix(c(5,1,2,1,3,0,-1,1,4),3);A

w <- c(1,0,0)
T <- cbind(w, A%*%w, A%*%A%*%w, A%*%A%*%A%*%w); T
rref(T)
p <- function(t) t^3 - 12* t^2 + 48*t - 64
curve(p(x), from = 2, to = 6); abline(h=0, col = "red") #the only root is 4
#So p(t) = (t-4)^3 -- not simple
(A - 4*I)%*%(A - 4*I)%*%(A - 4*I)
#We can find only one eigenvector.
v <- (A - 4*I)%*%(A - 4*I)%*%w
A%*%v; 4*v   #an eigenvector with eigenvalue 4

#What we need to prove:
#Eigenvectors for distinct eigenvalues are linearly independent.
#There exists a basis of eigenvectors if and only if
#for each standard basis vector as w, the resulting p(t) has simple roots.

