#Math 23 Script 2C-ComplexConformal.R
#Module 1, week 2, subsections 1.6 through 1.8

#Last modified; September 3, 2014 by Paul Bamberg
source("1.2L-VectorLibrary.R")

#Topic 1 - Complex numbers in R
#Three ways to make a complex number in R
z1 <- complex(real = 4, imaginary = 3); z1
complex(1,4,3)   #the first "1" says to make a vector with one complex number
complex(modulus = 5, argument = A37*pi/180)

#Plotting a complex number is like plotting a vector
plot(NULL, xlim = c(0,5), ylim = c(0,5), xlab = "Re", ylab= "Im", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)
points(Re(z1), Im(z1))

#Adding complex numbers is just like adding vectors in R^2
z1 <- complex(real = 3, imaginary = 1)
z2 <- complex(real = 2, imaginary = 1)
v1 <- c(3,1); v2 <- c(2,1)
plot(NULL, xlim = c(0,5), ylim = c(0,5), xlab = "Re", ylab= "Im", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)
points(c(Re(z1), Re(z2)),c(Im(z1),Im(z2)))  #plot the complex numbers
arrows(0,0, v1[1], v1[2]);  arrows(0,0, v2[1], v2[2]) #plot the vectors
#Plot the sum of the complex numbers
zSum <- z1+z2; points(Re(zSum), Im(zSum))
#Plot the sum of the vectors
vSum <- v1+v2; arrows(0,0, vSum[1], vSum[2])
#The modulus of a complex number equals the length of the vector
Mod(z1); Norm(v1)
#The argument of a complex number is its angle above the x axis
Arg(z1); angleBetween(v1,e1.2)*pi/180   #in radians!

#Multiplying complex numbers does not correspond to any vector operation
zProd <- z1*z2; zProd
#The moduli get multiplied
Mod(zProd); Mod(z1)*Mod(z2)
#The arguments get added
Arg(zProd); Arg(z1)+Arg(z2)

#Topic 2 - Representing complex numbers by 2x2 matrices
#For z = x + iy, the first column is x, y; the second is -y, x
#Here is a function to create such a "conformal" matrix
confMat <- function(x,y) matrix(c(x,y,-y,x),2)
M43 <- confMat(4,3); M43
#This looks like a rotation matrix, but the columns are not unit vectors

plot(NULL, xlim = c(0,5), ylim = c(0,5), xlab = "Re", ylab= "Im", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)

#Look at the image of the first basis vector (the first column)
arrows(0,0,1,0, col = "red");  #first standard basis vector
arrows(0,0, M43[1,1], M43[2,1]);  #its image
angleBetween(e1.2, M43[,1])   #we rotated through 37 degrees
Norm(M43[,1])      #and we multiplied the length by 5

#Now we can make matrices for our complex numbers z1 and z2
Mz1 <- confMat(3,1); Mz2 <- confMat(2,1) ; Mz1; Mz2

#We can make the matrix for their sum
MzSum <- confMat(Re(zSum),Im(zSum))
#It is the sum of the matrices for z1 and z2
MzSum; Mz1+Mz2

#We can also make the matrix for their product
MzProd <- confMat(Re(zProd),Im(zProd))
#It is the product of the matrices for z1 and z2
MzProd; Mz1%*%Mz2; Mz2%*%Mz1 

#Notice that multiplication of conformal matrices is commutative.
#The complex numbers form a field, so every nonzero element has an inverse
z <- complex(1,4,3); z
zInv <- 1/z  ;zInv  #also a complex number
#The modulus of the inverse is the inverse of the modulus
Mod(z)*Mod(zInv)

#Try the same thing with the conformal matrix
Mz <- confMat(4,3); Mz
MzInv <- solve(Mz); MzInv   #its matrix inverse
#The first column is the inverse of the complex number
MzInv[,1]; zInv

#A curious mathematical fact:
#The conformal matrices are the only subset of matrices bigger than 1x1
#except for multiples of the identity matrix
#that satisfy all the axioms for a field.
