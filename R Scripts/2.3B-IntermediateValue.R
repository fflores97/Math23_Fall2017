#Math 23 Script 2.3B-IntermediateValue.R
#Module 2, Week 3, Subsection 1.2

#Last modified October 13, 2014 by Paul Bamberg

#Topic 1 - Proving the intermediate value theorem

#Hypotheses: slight special case to match the proof in Ross.
#a < b, with a and b in some interval I on which f is continuous.
#f(a) < y < f(b)

#Conclusion: there exists at least one x in (a,b) for which f(x) = y.

par(mar=c(1,1,1,1), pch=20) #margins small as possible
plot(NULL, xlim = c(-1,4), ylim = c(-1,1), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom

f <- function(x) (x-1)*(x-2)*(x-3)   #define a function
#Function f is continuous on the interval I = [0,4]
curve(f(x), add = TRUE)
a <- 0.8; b<- 3.2    #a < b, and both are in I
abline(v = c(a,b), col = "blue")
points(c(a,b),c(f(a), f(b)))
y <- 0.2; abline(h=y,col = "green") #f(a) < y < f(b)

#The IVT states that there is at least one x in (a,b) s.t f(x) = y

#Step 1: Construct the subset S of (a,b) for which f(x) < y
uniroot(function(x) f(x)-y, c(1,1.5))$root  #this only works if the IVT is true!
uniroot(function(x) f(x)-y, c(1.5,2))$root
uniroot(function(x) f(x)-y, c(3,4))$root
segments(a,0, 1.121,0, col = "red")
segments(1.791,0, 3.088,0, col = "red")
#The set S is the red part of the horizontal axis
x0 <- 3.088 #the supremum of set S
abline(v=x0, col = "red")

#Step 2:  make a sequence that converges to x0 from below
xSeq <- x0 - 1/(1:50)^2
#Since no element of xSeq is an upper bound for S, construct
sSeq <- numeric(n)
for (n in  1:50) sSeq[n] <- max(1.791,xSeq[n]) #all elements in S
points(sSeq, rep(0,50),col = "red")
#Now apply f to this sequence
#Since f is continuous, the resulting sequence will converge to f(x0)
ySeq <- f(sSeq)
points( rep(0,50),ySeq,col = "red")
#Since every element of ySeq is < y, its limit f(x0) cannot exceed y.

#Step 3: make a sequence that converges to x0 from above
tSeq <- numeric(n)
for (n in  1:50) tSeq[n] <- min(x0+1/n^2 , b) #all elements outside of S
points(tSeq, rep(0,50), col = "cyan")

#Now apply f to this sequence
ytSeq <- f(tSeq)
points( rep(0,50),ytSeq, col = "cyan")
#Since every element of ytSeq is > y, its limit f(x0) cannot be less than y.
#Conclusion: f(x0) = y.

#Topic 2 - Corollaries of the IVT

#Existence of an inverse function
#f is a strictly increasing function on interval I
f <- function(x) 1 + 1.3*x^2 + cos(2*x) #increasing on (1,3)

plot(NULL, xlim = c(0,3), ylim = c(0,15), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
abline(v=c(1,3), col = "blue")
curve(f(x), add = TRUE)
abline(h=c(f(1),f(3)), col = "red")


abline(h = 6, col = "green")
#By the IVT, the graph intersects each such horizontal line once
#So there is a strictly increasing inverse function
#from the interval J between the red lines to I.

#Existence of a maximum
#Suppose f is a bounded continuous function on an closed interval [a,b].
#Being bounded, it has a supremum M
#This supremum is actually a maximum, not just achieved as a limit.
#Here is the construction that proves this result.

f <- function(x) 1+(x-0.6)*(1.3-x)*(1.9-x)*(2.6-x)

plot(NULL, xlim = c(0,3), ylim = c(0,2), axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left and bottom
curve(f(x), add = TRUE)
#R has a function to find the supremum.
M <- optimize(f, lower = 0, upper = 3, maximum = TRUE)$objective
abline(h=M, col = "red")
 
#Construct a sequence that converges to M from below
ySeq <- M - 1/(1:15)^2
abline(h = ySeq, col = "green")
#Now construct a sequence in the domain.
xSeq <- numeric(15)
xSeq[1] <- uniroot(function(x) f(x) - ySeq[1], c(0,0.6))$root; points(xSeq[1], ySeq[1])
xSeq[2] <- uniroot(function(x) f(x) - ySeq[2], c(2.5, 3))$root; points(xSeq[2], ySeq[2])
xSeq[3] <- uniroot(function(x) f(x) - ySeq[3], c(2.5, 3))$root; points(xSeq[3], ySeq[3])
xSeq[4] <- uniroot(function(x) f(x) - ySeq[4], c(0,0.8))$root; points(xSeq[4], ySeq[4])
xSeq[5] <- uniroot(function(x) f(x) - ySeq[5], c(1,1.5))$root; points(xSeq[5], ySeq[5])
xSeq[6] <- uniroot(function(x) f(x) - ySeq[6], c(1.8,2.3))$root; points(xSeq[6], ySeq[6])
xSeq[7] <- uniroot(function(x) f(x) - ySeq[7], c(0.3,0.8))$root; points(xSeq[7], ySeq[7])
xSeq[8] <- uniroot(function(x) f(x) - ySeq[8], c(0.9,1.5))$root; points(xSeq[8], ySeq[8])
xSeq[9] <- uniroot(function(x) f(x) - ySeq[9], c(1.8,2.3))$root; points(xSeq[9], ySeq[9])
xSeq[10] <- uniroot(function(x) f(x) - ySeq[10], c(2.3,2.7))$root; points(xSeq[10], ySeq[10])
xSeq[11] <- uniroot(function(x) f(x) - ySeq[11], c(0.3,0.85))$root; points(xSeq[11], ySeq[11])
xSeq[12] <- uniroot(function(x) f(x) - ySeq[12], c(0.9,1.5))$root; points(xSeq[11], ySeq[12])
xSeq[13] <- uniroot(function(x) f(x) - ySeq[13], c(1.8,2.3))$root; points(xSeq[14], ySeq[14])
xSeq[14] <- uniroot(function(x) f(x) - ySeq[14], c(0.3,0.85))$root; points(xSeq[14], ySeq[14])
xSeq[15] <- uniroot(function(x) f(x) - ySeq[15], c(0.9,1.5))$root; points(xSeq[15], ySeq[15])


ySeq  #converges to M
xSeq  #does not converge but has a subsequence that converges to x0
xSeq[which(xSeq < 1.5)]
x0 <- optimize(f, lower = 0, upper = 3, maximum = TRUE)$maximum; x0
abline(v = x0, col = "red")
#Since every sequence is good, f(x0) = M.



