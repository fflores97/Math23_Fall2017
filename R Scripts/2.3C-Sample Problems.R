#Math 23 Script2.3C-SampleProblems.R

#Last modified October 16, 2014 by Paul Bamberg

par(mar=c(2,2,1,1), pch=20) #margins small as possible

#Problem 2
C <- function(x) 1 - x^2/2 + x^4/24
plot(NULL, xlim = c(-0.1,3.5), ylim = c(-1.1,1.1), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
curve(C(x), add = TRUE)
abline(v = c(1,2),col = "blue")


#Problem 3
f <- function(x) 1/x^2 + x^2
plot(NULL, xlim = c(0.1,3.5), ylim = c(0,6), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
curve(f(x), add = TRUE)
abline(v = c(1,2),col = "blue")

#Problem 4
f <- function(x) (1-cos(x))/x^2
plot(NULL, xlim = c(0.0001,3.5), ylim = c(0,2), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
curve(f(x), add = TRUE)
abline(v = c(0,pi),col = "blue")

#Problem 6
f <- function(x) (x^(1/3)-1)/(x-1)
plot(NULL, xlim = c(0,2), ylim = c(0,1), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
curve(f(x), add = TRUE)
abline(v = 1,col = "blue")
abline(h = 1/3,col = "red")
