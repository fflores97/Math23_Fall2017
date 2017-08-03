#Math 23 Script 1.3B-RowReductionApplications.R
#Module 1, Week 3, subsections 1.3, 1.4, and 1.5

#Last modified: September 10, 2014 by Paul Bamberg
library(pracma)   #for the rref() row-reduction function

#Topic 1 - Testing for linear independence or dependence

#A set of vectors (v1, v2, ...,vn) is linearly dependent if
#there is a nonzero set of coefficients such that
#a1v1 + a2v2 + ... +anvn = the zero vector.

#Use the vectors as columns of a matrix
v1 <- c(2,4,1); v2<-c(3,-2,3); v3 <- c(1,-3,2); v4<- c(5,12,1)
A <- cbind(v1, v2, v3, v4); A
#The top row represents the equation
#2 a1+ 3 a2+ a3 + 5a4 = 0  #no need to include the column of zeroes on the right
#Row reduce the matrix:
rref(A)
#Now the top row represents the equation
#a1 + 2 a4 = 0
#The second and third rows represent
#a2 + a4 = 0 and a3 - 2a4 = 0 respectively.
#If we make the convenient choice a4 = -1  we have
#a1 = 2, a2 = 1, a3 = -2, a4 = -1
#and the solution vector is
a <- c(2,1,-2,-1)

#Check:
A %*% a
#We have established dependence, since 2v1+v2-2v3-v4=0.

#Suppose we start with linearly independent vectors like
v1 <- c(2,4,3,1); v2<-c(3,-2,3,-1); v3 <- c(-2,1,-3,2); v4<- c(5,1,2,1)
#It is useful to add a column of zeroes as the right-hand side of the equation.
A <- cbind(v1, v2, v3, v4, c(0,0,0,0)); A
#Now row reduction gives
rref(A)
#The only solution is a1 = a2 = a3 = a4 = 0
#which means that the vectors are independent.

#Here is an example that illustrates the general case.
v1 <- c(1,2,3); v2 <- c(2,4,6); v3 <- c(1,1,1); v4 <- c(3,4,5); v5 <- c(2,4,3); v6 <- c(-1,1,6)
A <- cbind(v1, v2, v3, v4, v5, v6); A

#For some reason rref() fails, but we can row reduce by hand
#Make the first column pivotal
A[2,] <- A[2,] - 2*A[1,]; A[3,] <- A[3,] - 3*A[1,]; A

#The second column is nonpivotal and tells us that v2 = 2v1
v2 - 2*v1

#Make the third column pivotal
A[2,] <- - A[2,]; A
A[1,] <- A[1,] - A[2,]; A[3,] <- A[3,] + 2*A[2,]; A

#The fourth column is nonpivotal and tells us that v4 = v1+2v3
v4 - v1 -2*v3

#Make the fifth column pivotal
A[3,] <- - A[3,]/3; A
A[1,] <- A[1,] - 2*A[3,]; A

#The sixth column is nonpivotal and tells us that v6 = 4v1-3v3-v5
v6 - 4*v1 + 3*v3 +v5

#Topic 2 - Inverting a matrix by row reduction
#Inverting matrices of real numbers leads to messy fractions.
#Let's work in Z_5
"%+5%" <- function(x,y) (x+y) %%5  #addition
"%-5%" <- function(x,y) (x-y) %%5  #subtraction
"%*5%" <- function(x,y) (x*y) %%5  #multiplication
"%/5%" <- function(x,y) (x*y*y*y) %%5  #division

#Now any 3x3 matrix whose determinant is not zero has a nice-looking inverse.
A <- matrix(c(2,4,1,0,2,3,4,1,2),3); A
det(A) %% 5   #nonzero - the matrix is invertible
#Append the standard basis vectors to the right
AExt <- cbind(A[,1],A[,2],A[,3],c(1,0,0), c(0,1,0),c(0,0,1)); AExt

#Just row reduce - make the first column pivotal
AExt[1,] <- AExt[1,]%/5% 2; AExt
AExt[2,] <- AExt[2,] %-5% (4 %*5% AExt[1,]);AExt[3,] <- AExt[3,] %-5% (AExt[1,]); AExt

#Make the second column pivotal
AExt[2,] <- AExt[2,]%/5% 2; AExt
AExt[3,] <- AExt[3,] %-5% (3 %*5% AExt[2,]); AExt

#Make the third column pivotal
AExt[3,] <- AExt[3,]%/5% 3; AExt
AExt[1,] <- AExt[1,] %-5% (2 %*5% AExt[3,]); AExt[2,] <- AExt[2,] %-5% (4 %*5% AExt[3,]); AExt

#The last three columns are the inverse matrix
AInv <- AExt[,4:6]; AInv
AInv %*% A %%5   #it really is the inverse

#Topic 3 - Showing that a given set of vectors fails to span R^n
#If a set of vectors spans R^n,
#any vector can be written as a linear combination of them.
#Here is a set of vectors that fails to span R^3.
v1 <- c(3,2,1); v2 <- c(6,4,2); v3 <- c(3,2,2); v4 <- c(3,2,0)
B <- cbind(v1,v2,v3,v4);B #make a matrix
#Append the identity matrix before we row reduce 
A <- cbind(v1,v2,v3,v4,c(1,0,0), c(0,1,0),c(0,0,1));A
#rref() fails on this sort of matrix, so we do it by hand
A[1,] <- A[1,]/3; A
A[2,] <- A[2,] - A[2,1]*A[1,];A
A[3,] <- A[3,] - A[3,1]*A[1,];A   #column 1 now pivotal

#We have to swap the last two rows.
temp <- A[2,]; A[2,] <- A[3,]; A[3,] <- temp; A

#Now it is easy to make the third column pivotal
A[1,] <- A[1,] - A[1,3]*A[2,];A   #column 3 now pivotal

#Interpretation:
#Columns 1 and 3 are pivotal; 
#v1 and v3 are independent
#They are a basis for the subspace spanned by v1, v2, v3, v4
#If we used elementary matrices, their product would be saved
#in the last three columns.
E <- A[,5:7]; E
E%*%B #it row reduces the original matrix
#The matrix E is guaranteed to be invertible.
EInv <- solve(E); EInv
#Now we can construct a vector that is not in the image of B
w <- EInv%*%c(0,0,1); w  #just the last column of EInv
#So the second standard basis vector is not in the subspace
#spanned by our four vectors.

#Topic 4 - Constructing a basis for the image and kernel
#We started with this matrix:
B
#We row reduced it to the following matrix:
E%*%B
#The first and third columns are pivotal.
#So the first and third columns of the orginal matrix are a basis for Img B.

#To find a basis for the kernel, use the two nonpivotal columns.
#Vectors of the form (x1 1 z1 0) and (x2 0 z2 1) must be independent,
#because no linear combination can have 0 as its 2nd and 4th component.
#The top row of the row reduced matrix applied to (x1 1 z1 0) says that x1+2 = 0.
#The second row of the row reduced matrix applied to (x1 1 z1 0) says that z1 = 0.
k1 <- c(-2,1,0,0)   #first basis vector for the kernel
B%*%k1     #yes, it is in the kernel

E%*%B     #the row reduced matrix
#The top row of the row reduced matrix applied to (x2 0 z2 1) says that x2+2 = 0.
#The second row of the row reduced matrix applied to (x2 0 z2 1) says that z2-1 = 0.
k2 <- c(-2,0,1,1)   #second basis vector for the kernel
B%*%k2     #yes, it is also in the kernel

#A simple rule:
#To make k1, negate the two entries in nonpivotal column 2
#and put them in the empty slots of vector (. 1 . 0)
k1 <- c(-2, 1, 0, 0)
#To make k2, negate the two entries in nonpivotal column 4
#and put them in the empty slots of vector (. 0 . 1)
k2 <- c(-2, 0, 1, 1)

