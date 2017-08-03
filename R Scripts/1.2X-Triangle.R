#Math 23 Script 2X-Triangle.R
#Module 1, week 2, examples related to subsections 1.1 and 1.2

#Last modified September 11, 2014 by Paul Bamberg

#Topic 1 - Generating and displaying a randomly generated triangle

source("1.2L-VectorLibrary.R")
edge <- 5    #size of the square in which the triangle will fit
vertices <- makeTriangleABC(edge)  #returns a list of three points
#Extracting a component of a list requires double square brackets.
A <- vertices[[1]]; B <- vertices[[2]]; C <- vertices[[3]];
#Plot the three vertices
par(mar = c(2,2,1,1)+0.1, pch = 20)
plot(c(A[1],B[1],C[1]), c(A[2],B[2],C[2]), xlim = c(-edge-1,edge+1), ylim = c(-edge-1,edge+1),asp = 1, xlab = "", ylab = "")  
 
#Make and display 2-component vectors for the sides
v.AB <- B-A; v.AB   #vector from A to B
arrows(A[1],A[2],B[1],B[2], col = "green")
v.BC <- C-B; v.BC    #vector from B to C
arrows(B[1],B[2],C[1],C[2], col = "green")
v.AC <- C-A; v.AC    #vector from A to C
arrows(A[1],A[2],C[1],C[2], , col = "green")

#Calculate and display the lengths of the sides
side.a <- Norm(v.BC); side.a
text((B[1]+C[1])/2, (B[2]+C[2])/2, paste("a = ", round(side.a, digits = 2)))
side.b <- Norm(v.AC); side.b
text((A[1]+C[1])/2, (A[2]+C[2])/2, paste("b = ", round(side.b, digits = 2)))
side.c <- Norm(v.AB); side.c
text((A[1]+B[1])/2, (A[2]+B[2])/2, paste("c = ", round(side.c, digits = 2)))

#Calculate and display the angles at the vertices
angle.A <- angleBetween(v.AB,v.AC); angle.A
text(A[1], A[2], paste("A = ", round(angle.A, digits = 1)))
angle.B <- angleBetween(-v.AB,v.BC); angle.B
text(B[1], B[2], paste("B = ", round(angle.B, digits = 1)))
angle.C <- angleBetween(-v.AC,-v.BC); angle.C
text(C[1], C[2], paste("C = ", round(angle.C, digits = 1)))

#Topic 2 - Checking some formulas of trigonometry

#Check that the angles sum to 180 degrees
angle.A + angle.B + angle.C
#Check the Law of Sines
Sin(angle.A)/side.a; Sin(angle.B)/side.b; Sin(angle.C)/side.c;
#Check the Law of Cosines
sqrt(side.a^2 + side.b^2 - 2* side.a*side.b * Cos(angle.C)); side.c

#Calculate the area in many different ways
#From the determinant of AB and AC
abs(det(cbind(v.AB, v.AC)))/2
#Using AB as the base and calculating the height from AC
side.c*side.b*Sin(angle.A)/2
#From Heron's formula
s <- (side.a + side.b + side.c)/2   #half the perimeter
sqrt(s*(s - side.a)*(s - side.b)*(s - side.c))
#From lengths and dot product of AB and AC
sqrt(Norm(v.AB)^2*Norm(v.AC)^2-(v.AB%.%v.AC)^2)/2

