#Math 23 Script2.3A-Continuity.R
#Module 2, Week 3, Subtopics 1.1 and 1.3

#Last modified October 13, 2014 by Paul Bamberg

#Topic 1 - Two definitions of continuity
par(mar=c(1,1,1,1), pch=20) #margins small as possible
plot(NULL, xlim = c(0,4), ylim = c(0,6), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

f <- function(x) x^1.5    #define a function
#Our goal is to show that this function is continuous at x = 2.
curve(f(x), add = TRUE)
abline(v=2); abline(h=f(2))

#Ross's definition: the "no bad sequence" approach
#Define a sequence in the domain that converges to 2
xSeq <- 2 - 1/(1:40)^1.1  #just the first 40 terms.
points(xSeq, rep(0,40))
#Apply f to get a sequence of function values in the codomain.
ySeq <- f(xSeq)  #just the first 40 terms.
points(rep(0,40) ,ySeq, col = "red")

#This sequence is "good": xSeq converges to 2, ySeq converges to f(2)
#"Continuous" means that we cannot invent a bad sequence.

#The conventional definition, using epsilon and delta
epsilon <- 0.5   #choose an epsilon
#Use this epsilon to set up a target band
abline(h = c(f(2)-epsilon, f(2)+epsilon), col = "red")
#Find a delta that meets the target
delta <- 0.2  #found by inspecting the graph
abline(v = c(2 - delta, 2 + delta), col = "blue")
#If x is in the delta band, f(x) is in the epsilon band
epsilon <- 0.2   #choose a smaller epsilon
abline(h = c(f(2)-epsilon, f(2)+epsilon), col = "red", lty = 2)
#Find a delta that meets the target
delta <- 0.07  #found by inspecting the graph
abline(v = c(2 - delta, 2 + delta), col = "blue", lty = 2)

#For a formal proof of continuity, invent a function that
#calculates an appropriate delta from any epsilon.

#Now repeat with a discontinuous function
plot(NULL, xlim = c(0,4), ylim = c(0,6), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
f <- function(x) x^1.5 + (x > 2)    #define a function that jumps at 2
#Our goal is to show that this function is discontinuous at x = 2.
curve(f(x), from = 0, to = 2, add = TRUE)
curve(f(x), from = 2.01, to = 4, add = TRUE)
abline(v=2); abline(h=f(2))

#Now we can construct a bad sequence.
xSeq <- 2 + 1/(1:40)^1.1  #just the first 40 terms.
points(xSeq, rep(0,40))
#Apply f to get a sequence of function values in the codomain.
ySeq <- f(xSeq)  #just the first 40 terms.
points( rep(0,40),ySeq, col = "red")

#This sequence is "bad": xSeq converges to 2, ySeq does not converge to f(2).

#Here is an even worse sequence
plot(NULL, xlim = c(0,4), ylim = c(0,6), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
f <- function(x) x^1.5 + (x > 2)    #define a function that jumps at 2
#Our goal is to show that this function is discontinuous at x = 2.
curve(f(x), from = 0, to = 2, add = TRUE)
curve(f(x), from = 2.01, to = 4, add = TRUE)
abline(v=2); abline(h=f(2))
xSeq <- 2 + (-1)^(1:40)/(1:40)  #just the first 40 terms.
points(xSeq, rep(0,40))   #this sequence converges to 2

#Apply f to get a sequence of function values in the codomain.
ySeq <- f(xSeq)  #just the first 40 terms.
points( rep(0,40),ySeq, col = "red")  
#The sequence in the codomain does not converge to anything.

#The conventional definition, using epsilon and delta
epsilon <- 0.5   #choose an epsilon
#Use this epsilon to set up a target band
abline(h = c(f(2)-epsilon, f(2)+epsilon), col = "red")
#No delta can meet this target.
delta <- 0.2 
abline(v = c(2 - delta, 2 + delta), col = "blue")
#For all x > 2 in the delta band, f(x) is outside the epsilon band.
#Choosing a smaller delta will not help.

#Topic 2 -- Uniform continuity
plot(NULL, xlim = c(0,4), ylim = c(0,8), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

f <- function(x) x^1.5    #define the same function
#Our goal is to show that this function is uniformly continuous on [0,4].
curve(f(x), add = TRUE)
#The first thing we do is to agree on a value for epsilon.
epsilon <- 0.5
#The epsilon band can be placed anywhere in the codomain.
abline(h = c(1,1+epsilon), col = "red")
abline(h = c(2.8,2.8+epsilon), col = "red")
abline(h = c(5.1,5.1+epsilon), col = "red")
#We need a delta that will meet the challenge uniformly.
#Here is one that is not good enough.
delta <- 0.28
abline(v = c(1,1+delta), col = "blue")
#It works near 1: if x and y are in the delta band,
#f(x) and f(y) are in the epsilon band.

abline(v = c(2.9,2.9+delta), col = "blue")
points(c(2.92, 3.16), c(f(2.92), f(3.16)))
#It fails near 3: x and y are in the delta band, 
#but f(x) and f(y) are in the epsilon band.

#However, a smaller delta will do the job.
delta <- 0.2
abline(v = c(1.05,1.05+delta), col = "green")
#It works better near 1: if x and y are in the delta band,
#f(x) and f(y) are in the epsilon band.
#It also works fine near x = 3.
abline(v = c(2.93,2.93+delta), col = "green")
 
#The general rule: if f is continuous on a closed and bounded interval,
#it is uniformly continuous.

#What can go wrong if the interval is not closed.

plot(NULL, xlim = c(0,2), ylim = c(0,60), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
f <- function(x) 1/(2-x)    #define a function
#This function is continuous on the open interval [0,2)
curve(f(x), add = TRUE)
epsilon <- 1 #looks like an easy challenge
abline(h = c(45,45+epsilon), col = "red") #until you place it near the right edge

#What can go wrong if the interval is not bounded.

plot(NULL, xlim = c(0,100), ylim = c(0,10000), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
f <- function(x) x^3/100    #define a function
#This function is continuous on the closed interval [0, infinity]
curve(f(x), add = TRUE)
epsilon <- 30 #looks like an easy challenge
abline(h = c(9000,9000+epsilon), col = "red")#until you place it near the right edge

#In either case the problem is that the derivative is unbounded on the interval.





