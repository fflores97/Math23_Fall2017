#Math 23 Script 2.4A-Taylor Series

#Last modified October 20, 2014 by Paul Bamberg

#Topic 1 - Convergence of the Taylor series for the cosine function

C1 <- function(x) 1 - x^2/2
C2 <- function(x) 1 - x^2/2 + x^4/24
C3 <- function(x) 1 - x^2/2 + x^4/24 - x^6/720
C4 <- function(x) 1 - x^2/2 + x^4/24 - x^6/720 + x^8/(8*7*720)
C5 <- function(x) 1 - x^2/2 + x^4/24 - x^6/720 + x^8/(8*7*720) - x^10/factorial(10)

curve(cos(x),  ylim = c(-2,2),from = -7, to = 7)
curve(C1(x),  col = "blue", add = TRUE)
curve(C2(x),  col = "red", add = TRUE)
curve(C3(x),  col = "green", add = TRUE)
curve(C4(x),  col = "magenta", add = TRUE)
curve(C5(x),  col = "cyan", add = TRUE)

#Each partial sum does well for a while then goes bad very quickly!

#Plot the remainders
curve(cos(x) - C1(x),  ylim = c(-2,2),from = -7, to = 7)
curve(cos(x) -C2(x),  col = "red", add = TRUE)
curve(cos(x) -C3(x),  col = "green", add = TRUE)
curve(cos(x) -C4(x),  col = "magenta", add = TRUE)
curve(cos(x) -C5(x),  col = "cyan", add = TRUE)

#Each remainder behaves like the first omitted term but falls a bit short.
curve(x^4/24, lty = 2, add = TRUE)
curve(-x^6/720, col = "red" ,lty = 2, add = TRUE)
curve(x^8/factorial(8),  col = "green",lty = 2, add = TRUE)
curve(-x^10/factorial(10),  col = "magenta", lty = 2,add = TRUE)
curve(x^12/factorial(12),  col = "cyan",lty = 2, add = TRUE)

#Topic 2 - A function that is not the sum of its Taylor series

f <- function(x) ifelse(x >0 , exp(-1/x), 0)
curve(f(x),  ylim = c(-0.5,1.5),from = -1, to = 7)

#The problem is that near x = 0, this function is less than any multiple of x^n
curve(x^6/50, col= "red",add = TRUE) #looks good

#But take a close-up look
curve(f(x),  ylim = c(-0.00000001,0.00000001),from = -0.001, to = 0.1)

#The problem is that near x = 0, this function is less than any multiple of x^n
curve(x^6/10, col= "red",add = TRUE)

#Topic 3 -- Illustrating Ross's proof of Taylor series with remainder.
#Evaluate the exponential of 1.5 using three terms plus remainder.

E <- function(x) 1 + x + x^2/2
curve(exp(x), ylim = c(-1,8), xlim = c(0,2), lty = 3)
curve(E(x), col = "red",  lty = 3 ,add = TRUE)
x0 <- 1.5
abline(v = x0)
abline(h = 0, lty = 3)
points(x0,0, pch = 20)
#Calculate the remainder at x = x0.
rem = exp(x0)-E(x0); rem
#Calculate the coefficient M 
M <- rem/(x0^3/6); M
Rolle0 <- function(x) exp(x) - E(x) - M*x^3/6
curve(10*Rolle0(x), , xlim = c(0,x0), col = "green", add = TRUE) #satisfies Rolle's theorem on (0,1.5)
x1 <- optimize(Rolle0, c(0,1.5), maximum = FALSE)$minimum; x1
points(x1,0, pch = 20)

Rolle1 <- function(x) exp(x) - (1+x) - M*x^2/2
curve(10*Rolle1(x), xlim = c(0,x1), col = "blue", add = TRUE) #satisfies Rolle's theorem on(0,x1)
x2 <- optimize(Rolle1, c(0,x1), maximum = FALSE)$minimum; x2
points(x2,0, pch = 20)

Rolle2 <- function(x) exp(x) - 1 - M*x
curve(10*Rolle2(x), xlim = c(0,x2),  col = "magenta", add = TRUE) #satisfies Rolle's theorem on(0,x2)
x3 <- optimize(Rolle2, c(0,x2), maximum = FALSE)$minimum; x3
points(x3,0, pch = 20)

Rolle3 <- function(x) exp(x)- M 
curve(Rolle3(x),xlim = c(0,x3), col = "cyan", add = TRUE) #equals 0 at x3

#So M equals the third derivative of the exponential function evaluated at x3
Taylor <- function(x) E(x) + exp(x3)*x^3/6
curve(Taylor(x), col = "red",  add = TRUE)

#Taylor series with remainder is exact at 1.5 and pretty good everywhere on (0,2)
