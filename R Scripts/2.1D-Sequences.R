#Math 23 Script 2.1D-Sequences.R

#Last modified: September 24, 2014 by Paul Bamberg

#Topic 1 - Limit of an infinite sequence

#An infinite sequence is a function whose domain is the natural numbers.
sFun <- function(n) (n-1)/(2*n+3)
#We can put a finite number of elements of the sequence into a vector.

sVec <-sFun(10:100); sVec #terms at the beginning are irrelevant
#It's clear that this sequence is increasing, and it appears to have a limit of 0.5.

par(mar=c(3,2,5,1),pch=20)
plot(10:100,sVec, ylim = c(0.4, 0.6))
abline(h=0.5, col = "green")
#As a step in proving that the limit is 0.5, we specify an epsilon
s <- 0.5; epsilon <- 0.05
abline(h=s-epsilon, col = "red"); abline(h=s+epsilon, col = "red")
#Now find an N for which all subsequent values are in the red strip
N <- 25; abline(v = N)   #easy, since it's an increasing sequence.
#With a smaller epsilon
epsilon <- 0.03
abline(h=s-epsilon, col = "red", lty = 2); abline(h=s+epsilon, col = "red", lty = 2)
#We need a larger N
N <- 45; abline(v = N, lty = 2)
#No amount of graphing can prove that the limit is s.
#We have to find a formula that expresses a satisfactory N in terms of epsilon.
Nvalue <- function(ep) 5/(4*ep)
epsilon <- 0.02
abline(h=s-epsilon, col = "red", lty = 3); abline(h=s+epsilon, col = "red", lty = 3)
N <- Nvalue(epsilon); abline(v = N, lty = 3)
#The proof for the limit consists in showing that if n
#exceeds the number that Nvalue() calculates,
#then sFun(n) is within epsilon of the limit 0.5.

#Topic 2 - Limit of sum = sum of limits
#This is not a proof, just an illustration for a specific epsilon
tFun <- function(n) (3*n-5)/(4*n+1)
tVec <- tFun(10:100);tVec   #limit looks like 3/4
sumFun <- function(n) sFun(n)+tFun(n)
sumVec <- sumFun(10:100);sumVec   #limit looks like 5/4
plot(10:100,sVec, ylim = c(0.4, 1.4))
points(10:100,tVec)
points(10:100,sumVec); abline(h = 5/4, col = "green")
#We want to show that the sum sequence has a limit of 5/4
epsilon <- 0.06
abline(h = c(5/4 + epsilon,5/4 - epsilon), col = "red")
#Allocate half our epsilon to s, half to t
eps2 <- epsilon/2
abline(h = 1/2, col = "green")
abline(h = c(1/2 + eps2,1/2 - eps2), col = "red")
abline(v = Nvalue(eps2),col = "blue")    #42 does the job for s

abline(h = 3/4, col = "green")
abline(h = c(3/4 + eps2,3/4 - eps2), col = "red")
abline(v = 55,col = "blue", lty = 2)    #55 does the job for t

#So 55 also does the job for s + t
#The proof consists in showing that this approach works for any positive epsilon.



#Topic 3 - Convergence of sequence of inverses (proof 5.2)
#We now know how to prove that the limit of a sum is the sum of the limits.
#It is also easy to prove that the limit of a product is the product of the limits.
#The issue of quotients is trickier. Look just at inverses.
#We assume that s_n is never zero (cannot divide by zero)
#We also assume that, although an individual s_n may be positive or negative,
#the sequence has a positive limit s. (negative s would also work)
#Define a sequence with some randomness built into it.
myseq <- function(n) { 
   0.1+ 1/(n+2) + runif(1, min = -4/(n+2), max = 4/(n+2))
}
 
sVec <- numeric(100)
for (n in 1:100) sVec[n] <- myseq(n)
sVec   #some of the values are negative
plot(1:100, sVec, pch = 20)
abline(h = 0.1, col = "green")    #limit looks like 0.1

#Now investigate the sequence of inverses.
tVec <- numeric(100)
for (n in 1:100) tVec[n] <- 1/myseq(n)   #some large values may get missed
plot(1:100, ylim = c(-10, 20), tVec, pch = 20)
abline(h = 10, col = "green")     #limit looks like 10

#If we plot only every fifth term, the limits becaome more apparent
seq5 <- seq(5, 500, by = 5); seq5

for (n in seq5) sVec[n/5] <- myseq(n)
plot(seq5, sVec, pch = 20)
length(sVec)
abline(h = 0.1, col = "green")    #limit looks like 0.1
tVec <- numeric(100)
for (n in seq5) tVec[n/5] <- 1/myseq(n)    
plot(seq5, ylim = c(-10, 20), tVec, pch = 20)
abline(h = 10, col = "green")     #limit looks like 10

#Step 1 - choose N1 so large that s_n is guaranteed greater than 0.05
N1 <- 58   #calculated from the formula for the nth term

for (n in 1:100) sVec[n] <- myseq(n)
plot(1:100, sVec, pch = 20)
abline(h = 0.1, col = "green")    #limit looks like 0.1
abline(h = 0.05, col = "red")     #epsilon is half the limit
abline(v = N1, col = "blue")
#So for all n > N1, we are sure that s_n > s/2.

#We need to make |t - t_n| less than epsilon
#Algebra shows that making |s_n - s| less than 
#epsilon * s^2/2 will accomplish this goal.
epsilon <- 2  #very modest goal for the sequence t_n of inverses
eps.S <- epsilon*0.01/2; eps.S

for (n in seq5) sVec[n/5] <- myseq(n)
plot(seq5, sVec, ylim = c(-0.1, 0.3),pch = 20)
abline(h = 0.1, col = "green")    
abline(h = c(0.1 + eps.S,0.1 - eps.S), col = "red")
#It appears that N = 400 will do the job
#All that matters is that some such N exists!
abline(v = 400, col = "blue")

#Now go back to the sequence of inverses
for (n in seq5) tVec[n/5] <- 1/myseq(n)    
plot(seq5, ylim = c(-10, 20), tVec, pch = 20)
abline(h = 10, col = "green")     #limit is 10
abline(h = c(10+ epsilon,10 - epsilon), col = "red")
#N = 400 will do the job with plenty to spare
abline(v = 400, col = "blue")

#In this case the general proof is easier than the example,
#because we do not need to cite specific values for N.

