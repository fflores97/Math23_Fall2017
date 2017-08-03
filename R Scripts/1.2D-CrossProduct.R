#Math 23 Script 1.2D-CrossProduct.R
#Module 1, week 2, subsection 1.9

#Last modified: September 3, 2014 by Paul Bamberg

#Topic 1 - Algebraic properties of the cross product

source("1.2L-VectorLibrary.R")
#Define a function to calculate the cross product.
"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2],v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])  

#Define three vectors so we can illustrate all the key properties:
u <- c(1,2,3); v <- c(3,-2,2); w <- c(-1,1,1)
#Anticommutativity
u %x% v; v %x% u
#Special case of a vector with itself
u %x% u
#Linearity in the second factor(distributive law)
u %x% (v+w); u %x% v + u %x% w
#Standard basis vectors
c(1,0,0) %x% c(0,1,0)
#Swapping operators in a triple product
u %.% (v %x% w); (u %x% v) %.% w
#Connection with determinant
det(cbind(u,v,w))
#Triple cross product - the vector in the middle gets the plus sign
(u %x% v) %x% w; (u %.% w)*v - (v %.% w)*u
#This operation is not associative
u %x% (v %x% w); (u %.% w)*v - (u %.% v)*w
#Jacobi identity
u %x% (v %x% w) + v %x% (w %x% u) + w %x% (u %x% v)
#Square of the length
Norm(u %x% v)^2; Norm(u)^2*Norm(v)^2 - (u %.% v)^2
#Dot product of two cross products by first*last-outside*inside
x <- c(2,-1,2)
(u %x% v) %.% (w %x% x)
(u %.% w)*(v %.% x)-(u %.% x)*(v %.% w)

#Topic 2 - Geometric properties of the cross product
#The cross product of two vectors is perpendicular to the plane that they span
u %.% (u %x% v); v %.% (u %x% v)
#The length of the cross product is vw sin(alpha)
Norm(u %x% v)
Norm(u)*Norm(v)*Sin(angleBetween(u,v))
#|Norm(u %x% v)| is the area of the parallelogram spanned by u and v
#Proof: make a unit vector perpendicular to the parallelogram
n <- (u %x% v)/Norm(u %x% v)
#We now have a prism whose height is 1 and whose volume equals its base area
n %.% (u %x% v)

#Test for independence of three vectors in R^3
u %.% (v %x% w)     #nonzero means independent

#Topic 3 - Using cross products to invert a 3x3 matrix
M <- cbind(u,v,w); M #make a matrix from our three vectors
#The cross product of v and w is orthogonal to the last two columns
(v %x% w) %.% v; (v %x% w) %.% w
#Its dot product with the first column is the determinant.
(v %x% w) %.% u; det(M)
#Put this together as a product of matrices.
#R is happy to treat the cross product as a 1 x 3 matrix (row vector)
(v %x% w) %*% M
#Now stack three rows on the left-hand side
A <- rbind(v %x% w, w %x% u, u %x% v); A
A %*% M  #a diagonal matrix!
#Divide by the determinant and we have the inverse
MInv <- A / det(M); MInv
#Check the result
round(MInv %*% M); round(M %*% MInv)

