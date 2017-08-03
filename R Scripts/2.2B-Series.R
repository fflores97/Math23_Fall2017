#Math 23 Script 2.2B-Series.R
#Module 2, Week 2, subsections 1.6 through 1.9
#Last modified October 6, 2014 by Paul Bamberg

#Topic 1 -- Series and partial sums

#A slowly convergent series
a <- function(n) 1/n^1.5
aVec <- a(1:100) 

par(mar=c(2,2,1,1),pch=20)
plot(1:100,aVec, type = "h",  ylim = c(0,3))

#Calculate the partial sums
sVec <- numeric(100)
for (n in 1:100) sVec[n] <- sum(aVec[1:n]) 

#Add the partial sums to the plot
points(1:100+0.5,sVec, type = "s" , ylim = c(0,5), col = "red")

#We can estimate the sum of the series
sum(a(1:1000))
sum(a(1:10000))
sum(a(1:100000))
abline(h = 2.61, col = "blue")   #just a guess

#A series that diverges to plus infinity
b <- function(n) 1/n^0.9 
bVec <- b(1:100) 
plot(1:100,bVec, type = "h",  ylim = c(0,6))

#Calculate the partial sums
sbVec <- numeric(100)
for (n in 1:100) sbVec[n] <- sum(bVec[1:n]) 

#Add the partial sums to the plot
points(1:100+0.5,sbVec, type = "s",   ylim = c(0,5), col = "red")

#Change some signs and get a convergent alternating series
d <- function(n) ((-1)^(n+1))/n^0.9 
dVec <- d(1:100)
plot(1:100,dVec, type = "h")

#Calculate the partial sums.
sdVec <- numeric(100)
for (n in 1:100) sdVec[n] <- sum(dVec[1:n]) 

#Add the partial sums to the plot.
points(1:100+0.5,sdVec, type = "s", col = "red")


#Topic 2 - Passing and failing the root test

#Choose the lim sup for the nth root of the nth term.
alpha <- 0.8   #less than 1 is what matters
#Sooner or later the nth root will be less than 0.9, perhaps quite a bit less 
pass <- function(n) runif(1, min = 0.7, max = 0.9 )^n
passVec <- numeric(100)
for (i in 1:100) passVec[i] <- pass(i)
plot(1:100,passVec, ylim = c(0,7),type = "h")
#That nth power really makes the terms go to zero quickly!

#Calculate the partial sums
spassVec <- numeric(100)
for (n in 1:100) spassVec[n] <- sum(passVec[1:n]) 

#Add the partial sums to the plot
points(1:100+0.5,spassVec, type = "s", col = "red")

#Now for an example where the root test proves divergence.
alpha <- 1.02   #greater than 1 is what matters
#The nth root will be greater than 1.01 infinitely many times.
#Otherwise the lim sup would be 1.01 or less!
#But this is only the lim sup - many terms may be less.
fail <- function(n) runif(1, min = 0.7, max = 1.01 )^n
failVec <- numeric(100)
for (i in 1:100) failVec[i] <- fail(i)
plot(1:100,failVec, ylim = c(0,20),type = "h")
#That nth power leads to some occasional big terms.
#And there are infinitely many of them!

#Calculate the partial sums
sfailVec <- numeric(100)
for (n in 1:100) sfailVec[n] <- sum(failVec[1:n]) 

#Add the partial sums to the plot
points(1:100+0.5,sfailVec, type = "s", col = "red")
#As n gets large, the occasional jump keeps getting bigger.
#This series diverges to plus infinity.

#Topic 3 -- Why the harmonic series diverges

harm <- function(n) 1/n
#With no random numbers in the function, it is easy to make a vector of values.
harmVec <- harm(1:200)
plot(1:200,harmVec, ylim = c(0,1.2),type = "h")
#We can try the root test
rootVec <- harmVec^(1/1:200)
points(1:200,rootVec, type = "s")   #we already proved that the limit is 1
abline(h = 1, col = "red")
#The lim sup (and the limit) of the roots is 1, and the root test is useless.

#Calculate the partial sums
sumVec <- numeric(200)
for (n in 1:200) sumVec[n] <- sum(harmVec[1:n]) 
plot(1:200,harmVec, ylim = c(0,7),type = "h")
points(1:200,sumVec, type = "s", col = "blue")

#The problem is that the partial sums do not form a Cauchy sequence
#We can check this with an epsilon of 1/2
epsilon <- 1/2
#Try N = 30
N<- 30; abline(h=sumVec[30], col = "blue"); abline(v = 30,, col = "blue" )
abline(h=sumVec[30] + epsilon, col = "red")   #we overshoot the top edge of the band

#Try N = 90
N<- 90; abline(h=sumVec[90], col = "green"); abline(v = 30,, col = "green" )
abline(h=sumVec[90] + epsilon, col = "red")   #we again overshoot the top edge of the band

#The problem can be expressed directly in terms of the series itself.
#Starting with any N, we can find a finite sum of terms that exceeds epsilon
sum(harmVec[31:60])
sum(harmVec[51:100])
sum(harmVec[81:160])
#Perhaps you have guessed the limit of these values.
log(2)      
#This argument is easy to convert into a proof.

#A similar argument shows why the sum of inverse squares converges.
#Start with a sequence that converges to 2.
compVec <- 2 - 1/(1:100)
plot(1:100,compVec, ylim = c(0,2.2),type = "s")
abline(h = 2, col = "red")
#Now form the series for which this is the partial sum
diffVec <- numeric(100); diffVec[1] = 1
for (i in 2:100) diffVec[i] <- compVec[i] - compVec[i-1]
points(1:100,diffVec, type = "h", col = "blue")
#Check that we have got it right.
compVec[60]; sum(diffVec[1:60])


#Now form the series of inverse squares
invSqVec <- numeric(100)
for (i in 1:100) invSqVec[i] <- 1/i^2
points(1:100,invSqVec, type = "h", col = "red")  #never larger
diffVec - invSqVec   #always positive or zero
#So the series of inverse squares converges by the comparison test.

#The partial sums will be smaller for the series of inverse squares
#than for the comparison series.
newsumVec <- numeric(100)
for (n in 1:100) newsumVec[n] <- sum(invSqVec[1:n])
points(1:100,newsumVec, type = "s", col = "magenta")

#Back in the 18th century, Euler figured out the sum.
abline(h=pi^2/6, col = "red")

#Again, this argument is easy to convert to a proof.
