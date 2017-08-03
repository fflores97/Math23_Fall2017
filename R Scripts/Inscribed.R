#Math 23 Script Inscribed.R

library("plotrix")    #needed for draw.circle()
plot(0,0,xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), asp = 1) #make empty plot
draw.circle(0,0,1)    #draw the unit circle
repeat {   #keep trying until points are not too close together
  angles <- sort(runif(3, min = 0, max = 2*pi))   #select 3 independent random angles
  if (max(angles[2]-angles[1], angles[3]-angles[2],2* pi + angles[1]-angles[3]) < 2.5) break
}
angles
O <- c(0,0); text(O[1]+0.1, O[2]-0.1, "O")
A <- c(cos(angles[1]), sin(angles[1])); points (A[1], A[2]); text(A[1]+0.1, A[2]-0.1, "A")
B <- c(cos(angles[2]), sin(angles[2])); points (B[1], B[2]); text(B[1]+0.1, B[2]-0.1, "B")
C <- c(cos(angles[3]), sin(angles[3])); points (C[1], C[2]); text(C[1]+0.1, C[2]-0.1, "C")
arrows(O[1], O[2], A[1], A[2], col = "green")
arrows(O[1], O[2], B[1], B[2], col = "green")
arrows(C[1], C[2], A[1], A[2], col = "blue")
arrows(C[1], C[2], B[1], B[2], col = "blue")
segments(A[1],A[2], B[1], B[2])
#The angle at O is twice the angle at C
angle.O <- acos(dot(A-O, B-O ))  #these are unit vectors
angle.C <- acos(dot(A-C, B-C)/sqrt(dot(A-C,A-C)*dot(B-C,B-C)))
angle.O; 2* angle.C
#The ratio in the Law of Sines is 0.5
sin(angle.C)/sqrt(dot(A-B,A-B))
#More generally for a circle of radius R, this ratio is 1/2R.


