#Math 23 Script 1.3P-RowReductionProofs.R
#Module 1, Week 3, illustration of Proof 1.3.2

#Last modified: September 10, 2014 by Paul Bamberg
library(pracma) # for rref()

#Topic 1 - In R^n, n+1 vectors cannot be independent
#As an example, set up a random matrix with 5 rows, 6 columns.
A <- matrix(runif(30),5);A
#The columns are six vectors in R^5.
rref(A)
#The last column is a linear combination of the other 5.
#There is no way that we can get 6 pivotal columns in the row-reduced matrix.
#So the six columns must be linearly dependent.

#Topic 2 - In R^n, n-1 vectors cannot span.
#As an example, set up a random matrix with 6 rows, 5 columns
B <- matrix(runif(30),6);B
#The columns are five vectors in R^6.
#Append an identity matrix so that we can construct E.
A <- cbind(B,diag(6)); A   #use the diag() function
ARed <- rref(A); ARed      #of course we get a bottom row with five zeroes
#The last six columns are the matrix E.
E <- ARed[,6:11]; E
E%*%A
#As always, this matrix is invertible
EInv <- solve(E); EInv
w <- EInv[,6]      #the last column is not is the image of B
E%*%w              #because when we row reduce the matirx we get a 1 opposite the row of zeroes.
BB <- cbind(B,w)   #try to solve Bx = w
round(E%*%BB)      #but there is no solution

#It seems obvious that five vectors cannot span R^6,
#but we needed to prove that we can always construct a vector
#that is not a linear combination of the five vectors.


#Topic 3 - An invertible matrix must be square
#If A is invertible, the solution to Av = w
#must exist and be unique.

#Case 1: too many columns - use 6 columns
w<- 1:5; w    #Any other vector would also work equally well
A <- cbind(matrix(runif(6*5),5),w); A
rref(A)
#There is a nonpivotal column. We can assign any value to a6.
#The solution is not unique.

#Case 2: not enough columns - use 4 columns
A <- matrix(runif(4*5),5); A
rref(cbind(A,w, rep(0,5)))   #rref likes that column of zeroes
#The bottom row shows that the solution does not exist.

#In this case our arbitrary choice of w could have been a linear combination
#of the four columns, but we can guard against that.
B <- cbind(A,diag(5));B
E <- rref(B)[,5:9];E  #this matrix row-reduces A
round(E%*%A)
w <- solve(E,c(0,0,0,0,1));w   #this vector always works
rref(cbind(A,w, rep(0,5))) #because row reduction converts it to the last standard basis vector

#Conclusion: The solution to Av = w exists and is unique for all w
#so that A is invertible
#only if A has the same number of columns as rows (is square)

