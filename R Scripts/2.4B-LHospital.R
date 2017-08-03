#Math 23 Script2.4B-LHospital.R

#Last modified: October 21, 2014 by Paul Bamberg


#Topic 1 - Illustration of proof 6 from Week 8
f <- function(x) sqrt(x) - 1   #the numerator
g <- function(x) sin(x-1) #the denominator

curve(g(x), from = 0.5, to = 2)
abline(h = 0)
abline(v = 1, col = "blue") #we want the limit as x approaches 1 from above
curve(f(x), col = "red", add = TRUE)

#Set x = 2: g and its derivative are both positive on (1,2]
h <- function(x) f(x)*g(2)- f(2)*g(x)
curve(10*h(x), col = "green", add = TRUE)
z <- optimize(h, c(1,2), maximum = FALSE)$minimum; z #guaranteed by Rolle
r <- function(x) 1/(2*sqrt(x)*cos(x-1)) #ratio of derivatives
f(2)/g(2); r(z)

#The same thing works for any smaller value of x
xx <- 1.5
hh <- function(x) f(x)*g(xx)- f(xx)*g(x)
curve(100*hh(x), col = "green", lty = 2, add = TRUE)
z <- optimize(hh, c(1,xx), maximum = TRUE)$maximum; z #this time h has a maximum
f(xx)/g(xx); r(z)

curve(r(x), col = "blue", add = TRUE) #the limit is 1/2 as z approaches 1
curve(f(x)/g(x), col = "magenta", add = TRUE) #f/g has the same limit

