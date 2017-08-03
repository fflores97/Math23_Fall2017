#Math 23 Script MakeTriangle.R
library(pracma)  #needed for the cross() and dot() functions
edge <- 5
repeat {   #keep trying until area exceeds 1/4 of plot region
  x <- runif(3,-edge,edge)  #random x coordinates of vertices
  y <- runif(3,-edge,edge)  #random y coordinates of vertices
  Area <- abs(cross(c(x[2]-x[1],y[2]-y[1],0),c(x[3]-x[1],y[3]-y[1],0))[3])/2
  if (Area > edge^2) break
}
paste("Made a triangle with area ", Area)
plot(NULL, NULL, xlim = c(-edge,edge), ylim = c(-edge,edge),asp = 1)  #make empty plot
points(x, y, pch = 20)  #plot the three vertices as solid dots
#Make 2-component vectors for the vertices
A <- c(x[1],y[1]); A  #vertex A
B <- c(x[2],y[2]); B   #vertex B
C <- c(x[3],y[3]); C   #vertex C

#Make and display 2-component vectors for the sides
v.AB <- B-A; v.AB   #vector from A to B
arrows(A[1],A[2],B[1],B[2], col = "green")
v.BC <- C-B; v.BC    #vector from B to C
arrows(B[1],B[2],C[1],C[2], col = "green")
v.AC <- C-A; v.AC    #vector from A to C
arrows(A[1],A[2],C[1],C[2], , col = "green")

#Calculate and display the lengths of the sides
side.a <- sqrt(dot(v.BC,v.BC)); side.a
text((B[1]+C[1])/2, (B[2]+C[2])/2, paste("a = ", round(side.a, digits = 2)))
side.b <- sqrt(dot(v.AC,v.AC)); side.b
text((A[1]+C[1])/2, (A[2]+C[2])/2, paste("b = ", round(side.b, digits = 2)))
side.c <- sqrt(dot(v.AB,v.AB)); side.c
text((A[1]+B[1])/2, (A[2]+B[2])/2, paste("c = ", round(side.c, digits = 2)))

#Calculate and display the measures of the angles at the vertices
angle.A <- acos(dot(v.AB,v.AC)/(side.b*side.c)); angle.A
text(A[1], A[2], paste("A = ", round(180*angle.A/pi, digits = 1)))
angle.B <- acos(dot(-v.AB,v.BC)/(side.a*side.c)); angle.B
text(B[1], B[2], paste("B = ", round(180*angle.B/pi, digits = 1)))
angle.C <- acos(dot(-v.AC,-v.BC)/(side.a*side.b)); angle.C
text(C[1], C[2], paste("C = ", round(180*angle.C/pi, digits = 1)))




#Check that the angles sum to pi radians
angle.A + angle.B + angle.C
#Check the Law of Sines
sin(angle.A)/side.a; sin(angle.B)/side.b; sin(angle.C)/side.c;
#Check the Law of Cosines
sqrt(side.a^2 + side.b^2 - 2* side.a*side.b * cos(angle.C)); side.c
#Calculate the area in many different ways
#From the cross product of AB and AC
abs(cross(c(v.AB,0),c(v.BC,0)))[3]/2
#From the determinant of AB and AC
abs(det(cbind(v.AB, v.AC)))/2
#Using AB as the base and calculating the height from AC
side.c*side.b*sin(angle.A)/2
#From Heron's formula
s <- (side.a + side.b + side.c)/2   #half the perimeter
sqrt(s*(s - side.a)*(s - side.b)*(s - side.c))
#From dot products of AB and AC
sqrt(dot(v.AB,v.AB)*dot(v.AC,v.AC)-dot(v.AB,v.AC)^2)/2

