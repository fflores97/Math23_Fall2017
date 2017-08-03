#Math 23 Script 2B-RotateReflect.R
#Module 1, week 2, subsection 1.5

#Last modified; September 3, 2014 by Paul Bamberg
source("1.2L-VectorLibrary.R")  #capitalized functions use degrees, not radians
par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins

#Topic 1 - Rotation matrices
#Let's make a matrix that rotates counterclockwise through 37 degrees
#We defined Cos and Sin so that angles can be specified in degrees

#The first standard basis vector rotates to (cos 37, sin 37)
c1 <- c(Cos(A37), Sin(A37));c1  
#The second standard basis vector rotates to (-sin 37, cos 37)
c2 <- c(-Sin(A37), Cos(A37));c2
plot(NULL, xlim = c(-1,1), ylim = c(-1,1), xlab = "x", ylab= "y", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)
arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, c1[1], c1[2], col = "red") #after rotation
arrows(0,0, 0, 1, col = "green")   #second standard basis vector
arrows(0,0, c2[1], c2[2], col = "red") #after rotation

#The rotated basis vectors are the columns of the rotation matrix.
rot37 <- cbind(c1,c2); rot37
#Now rotate the red vectors another 37 degrees
rc1 <- rot37 %*% c1; rc2 <- rot37 %*% c2
arrows(0,0, rc1[1], rc1[2], col = "blue") #after rotation
arrows(0,0, rc2[1], rc2[2], col = "blue") #after rotation
angleBetween(rc1, e1.2) #we got a rotation through 74 degrees

#Let's write a function to create a rotation matrix.
rotMat <- function(a) matrix(c(Cos(a),Sin(a),-Sin(a),Cos(a)),2)
rotMat(A37)    #check that it works
#The product of rotation matrices rotates through the sum of the angles
R24 <- rotMat(24); R36 <- rotMat(36)
R24 %*% R36; rotMat(60)  #product is a 60 degree rotation

#The inverse of a rotation matrix rotates through minus the angle.
InvRot37 <- solve(rot37); InvRot37
arrows(0,0, InvRot37[1,1], InvRot37[2,1], col = "magenta") #first column
arrows(0,0, InvRot37[1,2], InvRot37[2,2], col = "magenta") #second column

#The inverse of a rotation matrix equals its transpose.
InvRot37; t(rot37)
#The determinant is +1
det(rot37)


#Topic 2 - Reflection matrices

plot(NULL, xlim = c(-1,1), ylim = c(-1,1), xlab = "x", ylab= "y", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)
#Reflect in a line at angle a above the x-axis
#Here is the function that creates the matrix
refMat <- function(a) matrix(c(Cos(2*a),Sin(2*a),Sin(2*a),-Cos(2*a)),2)

#Check that it works as advertised for a = 20 degrees
F20 <- refMat(20)

abline(0, Tan(20))   #specify the reflection line by intercept and slope
arrows(0,0, 1, 0, col = "green")   #first standard basis vector
arrows(0,0, F20[1,1], F20[2,1], col = "green") #after rotation
arrows(0,0, 0, 1, col = "red")   #second standard basis vector
arrows(0,0, F20[1,2], F20[2,2], col = "red") #after rotation
#Each standard basis vector got reflected in the reflection line

#Check the angle between the first standard basis vector and its image
angleBetween(e1.2, F20[,1])   #40 degrees
#Reflect twice and you end up where you started.
#So the square of a reflection matrix should be the identity.
F20 %*% F20
 
#The determinant of a reflection matrix is -1
det(F20)
#A reflection matrix is symmetric
identical(F20,t(F20))   #it is equal to its transpose

#The product of two reflections is a rotation
F35 <- refMat(35)
#Reflect first in the 20 degree line, then in the 35 degree line
Product <- F35 %*% F20   #the right factor happens first!
Product    #looks like a rotation matrix
angleBetween(e1.2, Product[,1])  #through 30 degrees = 2(35-20)

#Try reversing the order of the reflections
Product2 <- F20 %*% F35   #the right factor happens first!
Product2    #again, a rotation matrix
angleBetween(e1.2, Product2[,1])  #through -30 degrees = 2(20-35)

plot(NULL, xlim = c(-1,1), ylim = c(-1,1), xlab = "x", ylab= "y", asp =1,axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0)
abline(0, Tan(20), col = "red")   #one reflection line  
abline(0, Tan(35), col = "green")   #the other reflection line
#We look at the first column of the matrix
#to find out what happens to the first standard basis vector.
arrows(0,0, 1, 0, col = "blue")    #first standard basis vector
arrows(0,0, F20[1,1], F20[2,1], col = "blue") #after red reflection
arrows(0,0, Product[1,1], Product[2,1], col = "red") #red reflection first
arrows(0,0, F35[1,1], F35[2,1], col = "blue") #after green reflection
arrows(0,0, Product2[1,1], Product2[2,1], col = "green") #green reflection first


