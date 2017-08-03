#Math 23 Script 1.2A-LengthDotAngle.R
#Module 1, Week 2, subsections 1.1 and 1.2

#Last modified: September 3, 2014 by Paul Bamberg

#Topic 1 - Length, Dot Product, Angles

par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,5), ylim = c(-1,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

#Here is a vector in the plane.
v <- c(4,3)  #4 meters east, 3 meters north
arrows(0,0,4,3, col = "red")
#Get its length by the Pythagorean theorem.
sqrt(v[1]^2+v[2]^2)

#Here is another way, based on how R works with vectors,
v^2    #R squares each component of the vector
sqrt(sum(v^2))
#This second approach works with vectors of any length:
w <- c(1,2,3,4,1,1,2); sqrt(sum(w^2))

#We can define a function that does the job:
Norm <- function(v) sqrt(sum(v^2))
Norm(v); Norm(w)    #must capitalize N in "Norm"

#Dot product of two vectors
v1 <- c(3,1); v2 <-c(2,-1)
arrows(0,0,3,1); arrows(0,0,2,-1)
v1[1]*v2[1]+v1[2]*v2[2]   #do it "by hand"
#Here is another way, based on how R works with vectors
v1*v2   #R multiplies components of the two vectors
sum(v1*v2)     #again this works for vectors of any length

#We can define a function that does the job:
"%.%" <- function(x,y) sum(x*y) #define our own operator
"%.%"(v1,v2)   #quotes needed when we use the function
v1 %.% v2      #no quotes needed when we use the operator
#Now we have yet another way to get the length of v
sqrt(v%.%v)

#The angle between two vectors is defined in terms of the dot product.
acos((v1%.%v2)/(Norm(v1)*Norm(v2)))
#This comes out in radians. 
Acos <- function(x) acos(x)*180/pi  #this comes out in degrees
Acos((v1%.%v2)/(Norm(v1)*Norm(v2))) #45 degrees looks right
#Let's write a function to get the angle in degrees between two vectors.
angleBetween <- function(x,y) Acos((x%.%y)/(Norm(x)*Norm(y)))
angleBetween(v1,v2)   #included in the library script


#Topic 2 - Components of a vector
plot(NULL, xlim = c(0,15), ylim = c(0,5), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
#Define the standard basis vectors in R^2
e1.2 <- c(1,0)   #unit length, points east
e2.2 <- c(0,1)   #unit length, points north
arrows(0,0,1,0,col = "green"); arrows(0,0,0,1,col = "green") #display them
e1.2 %.% e2.2      #zero dot product means orthogonal

v <- c(4,3); arrows(0,0,v[1],v[2],col = "red")
angleBetween(v,e1.2)  #v points 37 degrees north of east
#The angle is correct because we set asp = 1.
v%.%e1.2; v%.%e2.2 #extract the two components

#Any vector is a linear combination of the standard basis vectors
v13 <- 12*e1.2 + 5*e2.2; v13
arrows(0,0,v13[1],v13[2],col = "blue")

#Let's make a vector of length 17 directed 28 degrees north of east
v17 <- c(17*cos(28*pi/180),17*sin(28*pi/180)); v17
arrows(0,0,v17[1],v17[2],col = "magenta")
#We can write a function to do this:
make2Dvec <- function(len, angle) {
  c(len*cos(angle*pi/180),len*sin(angle*pi/180))
}
make2Dvec(17,28)

#It is often useful to resolve a vector into components 
#relative to a unit vector.

plot(NULL, xlim = c(0,3), ylim = c(0,3), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
a <- c(2,1)   #any vector will do
aUnit <- a/Norm(a); a; arrows(0,0,aUnit[1], aUnit[2])  #a unit vector
v <- c(1.5, 2); arrows(0,0,1.5,2, col = "red")
v1 <- (v%.%aUnit)*aUnit #the component of v along aUnit
arrows(0,0,v1[1], v1[2], col = "green")
v2 <- v - v1   #the other component
arrows(v1[1],v1[2],v[1], v[2], col = "green") #perpendicular to aUnit

#Topic 3 - Angles in Pythagorean triangles
#It is convenient to have "28 degrees" mean the small angle in an 8-15-17 triangle
A28 <- acos(15/17)*180/pi; A28
make2Dvec(17,A28)    #now the components are integers
#In the library script are a complete set of these, e.g.
A53 <- acos(3/5)*180/pi; A53
make2Dvec(5,A53)


#Topic 4 - Vector calculation using components
#A ship travels 20 miles 67 degrees north of east
#It then travels 30 miles 18 degrees south of east
#Finally it travels 10 miles 25 degrees south of west
#How far has it moved from its starting point,and in what direction?

plot(NULL, xlim=c(0,40),  ylim =c(0,30), asp =1, axes = FALSE ) #make empty plot
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
#The aspect ratio must be 1 or the angles will look wrong!
O <- c(0,0)  #starting point
v1 <- make2Dvec(20,67); v1
P1 <- O + v1 #after first leg
arrows(O[1],O[2],P1[1],P1[2], col = "green")

v2 <- make2Dvec(30,-18); v2
P2 <- P1 + v2 #after second leg
arrows(P1[1],P1[2],P2[1],P2[2], col = "blue")

v3 <- make2Dvec(10,180+25); v3
P3 <- P2 + v3 #after third leg
arrows(P2[1],P2[2],P3[1],P3[2], col = "red")

arrows(O[1],O[2],P3[1],P3[2], col = "magenta" )
Norm(v1+v2+v3)    #distance from starting point
angleBetween(v1+v2+v3, e1.2)


 
