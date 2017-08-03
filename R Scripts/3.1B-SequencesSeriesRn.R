#Math 23 Script 3.1B-SequencesSeriesRn.R

#Last modified October 24, 2014 by Paul Bamberg

library(plotrix) #for drawing circles
par(mar = c(1,1,1,1)) #maximize space for drawing
plot(NULL, xlim = c(0.5,2.5),ylim = c(0.5,2.5), xlab = "", ylab = "", axes = FALSE)

#Topic 1 - A convergent sequence of points in R^2

s <- function(n) {
  x <- 1 - 1/n^0.7 + (-1)^n/n
  y <- 2 + sin(n)/n
  c(x,y)
}

#Set up a target ball
epsilon <- 0.2
draw.circle(1,2,epsilon)

#Plot the first ten points in the sequence.
for (i in 1:10) {
  v <- s(i)
  points(v[1], v[2], col = "red", pch = 20)
  text(v[1],v[2]-0.03, paste(i),cex = 0.8)
}

#Plot the next ten points in the sequence.
for (i in 11:20) {
  v <- s(i)
  points(v[1], v[2], col = "blue", pch = 20)
  text(v[1],v[2]-0.03, paste(i),cex = 0.8)
}

#Beyond N = 17 all the points are inside the target ball
plot(NULL, xlim = c(0.5,2.5),ylim = c(0.5,2.5), xlab = "", ylab = "", axes = FALSE)
draw.circle(1,2,epsilon)
for (i in 18:40) {
  v <- s(i)
  points(v[1], v[2], col = "blue", pch = 20)
  text(v[1],v[2]-0.03, paste(i),cex = 0.8)
}

#Repeat with a smaller ball and a larger N
plot(NULL, xlim = c(0.5,2.5),ylim = c(0.5,2.5), xlab = "", ylab = "", axes = FALSE)
epsilon <- 0.1
draw.circle(1,2,epsilon)
for (i in 45:70) {
  v <- s(i)
  points(v[1], v[2], col = "red", pch = 20)
}

#This can be done for any epsilon > 0.
#So the sequence converges to (2,1).

#Topic 2 - A convergent infinite series of vectors.
v <- function(n) {
  x <- ifelse (n%%2 == 1,(-1)^((n-1)/2)/n,0)
  y <- ifelse (n%%2 == 0,(-1)^(n/2)/n,0)
  c(x,y)
}
plot(NULL, xlim = c(-2,2),ylim = c(-2,2), xlab = "", ylab = "", axes = FALSE)
arrows(0,0,v(1)[1], v(1)[2])
arrows(0,0,v(2)[1], v(2)[2])
arrows(0,0,v(3)[1], v(3)[2])
arrows(0,0,v(4)[1], v(4)[2])

#This sequence of vectors converges to zero, and it also defines a convergent series.
#Plot the first 10 partial sums.
sumX <- 0; sumY <- 0
plot(NULL, xlim = c(0,1.2),ylim = c(-1,0.2), xlab = "", ylab = "", axes = FALSE)
for (i in 1:10) {
  newsumX <- sumX + v(i)[1];newsumY <- sumY + v(i)[2]
  arrows(sumX,sumY,newsumX, newsumY)
  sumX <- newsumX; sumY <- newsumY
  arrows(0,0, sumX, sumY,col = "red")
}

#The limit of the partial sums is
#x = 1 -1/3 + 1/5 + ... = pi/4
#y = -1/2 + 1/4 - 1/6 + ... = -log(2)/2

limit <- c(pi/4, -log(2)/2)
#Set up a target ball
epsilon <- 0.1

sumX <- 0; sumY <- 0
plot(NULL, xlim = c(0,1.2),ylim = c(-1,0.2), xlab = "", ylab = "", axes = FALSE)
draw.circle(limit[1], limit[2], epsilon)

for (i in 1:10) {
  sumX <- sumX + v(i)[1];sumY <- sumY + v(i)[2]
  arrows(0,0, sumX, sumY,col = "red")
}

#Let's see if N = 10 is big enough
N <- 10
sumX <- 0; sumY <- 0
plot(NULL, xlim = c(0,1.2),ylim = c(-1,0.2), xlab = "", ylab = "", axes = FALSE)
draw.circle(limit[1], limit[2], epsilon)
for (i in 1:30) {
  sumX <- sumX + v(i)[1];sumY <- sumY + v(i)[2]
  if (i > N)
    arrows(0,0, sumX, sumY,col = "red")
}

#Set up a smaller target ball and a larger N
epsilon <- 0.05; N = 20
sumX <- 0; sumY <- 0
draw.circle(limit[1], limit[2], epsilon, border = "blue")
for (i in 1:40) {
  sumX <- sumX + v(i)[1];sumY <- sumY + v(i)[2]
  if (i > N)
    arrows(0,0, sumX, sumY,col = "blue")
}

#Easy to prove:
#If the series of vectors converges, so does each sequence of components.
#Slightly harder to prove:
#If each sequence of components converges, so does the series of vectors.

#Topic 3 - A convergent geometric series of matrices
#We need a matrix whose length is less than 1.
A <- matrix (c(0.3, -0.7, 0.4, 0.5), 2);A
sqrt(sum(A^2))  #just barely small enough to guarantee convergence
#Let's sum I + A + A^2 + ... + A^6.
sumA <- diag(c(1,1)) #the identity matrix
power <- A
for (i in (1:6)) {
  sumA <- sumA + power
  power <- power%*% A
  print(sumA)
}
#Let's sum 50 terms
sumA <- diag(c(1,1)) #the identity matrix
power <- A
for (i in (1:49)) {
  sumA <- sumA + power
  power <- power%*% A
  if (i > 47) print(sumA)
}
#The series appears to have converged
solve(diag(c(1,1))-A)  #and the sum is what we expected!

#Let's exponentiate the same matrix.
#We just have to divide each power by (n!).
#First try a case where we know the answer
B <- diag(c(2,1)); B
#Let's sum 50 terms
sumB <- diag(c(1,1)) #the identity matrix
power <- B
for (i in (1:49)) {
  sumB <- sumB + power
  power <- power%*% B/(i+1)
  if (i > 47) print(sumB)
}
diag(c(exp(2),exp(1))) #got it right!

#Now do the same with our matrix A
sumA <- diag(c(1,1)) #the identity matrix
power <- A
for (i in (1:49)) {
  sumA <- sumA + power
  power <- power%*% A/(i+1)
  if (i > 47) print(sumA)
}

