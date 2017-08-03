#Math 23 Script 1.3C-OrthonormalBasis.R
#Module 1, Week 3, subsection 1.7

#Last modified: September 16, 2014 by Paul Bamberg
source("1.2L-VectorLibrary.R")  #for dot and cross products
library("pracma") #for rref()

#Topic 1: Using Gram-Schmidt to construct an orthonormal basis.
#Our subspace V will consist of vectors in R^4 for which a1 + 2 a2 = a3 - 2 a4.
#We need a basis for this three-dimensional subspace in order to get started.
w1 <- c(1,0,1,0)  #a1 = a3,  and a2 and a4 are both zero
w2 <- c(1,1,5,1)  #a1 + 2 a2 = a3 - 2 a4 = 3; not a multiple of w1
w3 <- c(2,-1,2,1) #a1 + 2 a2 = a3 - 2 a4 = 0;

#Check for independence by row reduction.
rref(cbind(w1,w2,w3,c(0,0,0,0)))  #rref likes a square matrix
#If we accidentally chose a linearly dependent set, the dependence would become apparent.
#Step 1: make the first unit vector by normalization
v1 <- w1/Norm(w1); v1; Norm(v1)

#Step 2: convert w2 to a vector that is orthogonal to v1.
x <- w2 - (w2%.%v1)*v1; x%.% v1
#Then convert x to a unit vector
v2 <- x/Norm(x); v2

#Step 3: convert w3 to a vector that is orthogonal to both v1 and v2.
x <- w3 - (w3%.%v1)*v1 - (w3%.%v2)*v2; x%.% v1; x%.% v2
#Then convert x to a unit vector
v3 <- x/Norm(x); v3

#The easy way to check that we have succeeded:
A <- cbind(v1,v2, v3) ;A  #basis vectors are the columns
round(t(A)%*%A, digits = 6)   #the identity matrix

#With our orthonormal basis, it is easy to find the components of a vector.

y <- c(4,0,6,1)   #a1 + 2 a2 = a3 - 2 a4 = 4; in the subspace
y1 <- y %.% v1; y1  #component along our first basis vector
y2 <- y %.% v2; y2  #component along our second basis vector
y3 <- y %.% v3; y3  #component along our third basis vector
#Check that we can reconstruct y from its components:
round(y1*v1+y2*v2+y3*v3, digits = 6) #we have reconstructed the vector

#Finding the components with respect to the non-orthonormal basis takes more work.
A <- cbind(w1,w2,w3,y)
AA <- rref(A); AA

#Check that we can reconstruct y from its components:
round(AA[1,4]*w1+AA[2,4]*w2+AA[3,4]*w3, digits = 6) #we have reconstructed the vector


#Topic 2 - Making a new orthonormal basis for R^3.
#The three new vectors might be built into some physical instrument
#that can be used in any orientation.

w1 <- c(1,1,1)  #any vector will do
w2 <- c(1,2,3)  #not a multiple of the first vector
w3 <- c(0,0,1)  #not a linear combination

#Check for independence by row reduction.
rref(cbind(w1,w2,w3))
#If we accidentally chose a linearly dependent set, the dependence would become apparent.
#Step 1: make the first unit vector by normalization
v1 <- w1/Norm(w1); v1; Norm(v1)

#Step 2: convert w2 to a vector that is orthogonal to v1
x <- w2 - (w2%.%v1)*v1; x%.% v1
#Then convert x to a unit vector
v2 <- x/Norm(x); v2

#Step 3: convert w3 to a vector that is orthogonal to both v1 and v2
x <- w3 - (w3%.%v1)*v1 - (w3%.%v2)*v2; x%.% v1; x%.% v2
#Then convert x to a unit vector
v3 <- x/Norm(x); v3

#The easy way to check that we have succeeded:
R <- cbind(v1,v2, v3) ;R  #basis vectors are the columns
round(t(R)%*%R, digits = 6)   #the identity matrix

#Topic 3 - testing the cross-product rule for isometries

#The matrix R is an isometry - rotation or not?
det(R)   #+1 means that it's a rotation

#Now we can check the cross product rule from last week.
#Use arbitrary vectors u and v.
u <- c(2,3,4)
v <- c(-1,2,1)   
uxv <- u %x% v ;uxv   #the cross product
R%*%uxv   #the rotated cross product
(R%*%u)%x%(R%*%v)   #the same, as we proved last week

#To make a non-rotation matrix, just change the sign of one of the columns

F <- cbind(v1,-v2, v3) ;F  #basis vectors are the columns
round(t(F)%*%F, digits = 6)   #the identity matrix -- it's an isometry

det(F)   #-1 means that it's not a rotation
identical(t(F),F)  #not a reflection, either!

#Again we can check the cross product rule from last week.
u <- c (2,3,4)
v <- c(-1,2,1)   
uxv <- u %x% v ;uxv   #the cross product
F%*%uxv               #the transformed cross product
(F%*%u)%x%(F%*%v)     #opposite sign, as we proved last week



