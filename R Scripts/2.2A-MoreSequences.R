#Math 23 Script 2.2A-MoreSequences.R
#Module 2, Week 2, subsections 1.1 through 1.5
#Last modified October 6, 2014 by Paul Bamberg

#Topic 1 -- Cauchy Sequences
#Here is a sequence that rises and also wiggles a bit
cauchy <- function(n) 1 - (1+0.3*sin(sqrt(200*n)))/(n+3)
 
#Make a vector that we can plot.
cauchyVec <-cauchy(1:100) 
par(mar=c(2,2,2,1),pch=20)
plot(1:100,cauchyVec, ylim = c(0.8, 1.1))
abline(h=1, col = "cyan")
#This sequence appears to have a limit of 1.

#How to show that the sequence is Cauchy:
#Choose a positive epsilon.
epsilon = 0.14
#Choose N = 7
abline(v=7, col = "blue")
#Construct a band of width epsilon
abline(h = c(0.88, 1.02), col = "red") 
#No two terms with n > 7 differ by more than epsilon.
#Notice that this first band sets an upper and lower bound on the sequence.
#We have proved that every Cauchy sequence is bounded!

#Choose a smaller epsilon
epsilon = 0.07
#Choose N = 17
abline(v=17, col = "blue", lty = 2)
#Construct a band of width epsilon
abline(h = c(0.94, 1.01), col = "red", lty = 2) 
#No two terms with n > 17 differ by more than epsilon.

#Choose a still smaller epsilon
epsilon = 0.03
#Choose N = 53
abline(v=53, col = "blue", lty = 3)
#Construct a band of width epsilon
abline(h = c(0.97, 1.0), col = "red", lty = 3) 
#No two terms with n > 53 differ by more than epsilon.

#The limit of the sequence is the least upper bound of the top edge of the bands.
#The limit is also the greatest lower bound of the bottom edge of the bands.
#Those quantities must exist if we have a sequence of REAL numbers.

#We do not need to know the limit to set up the bands.
#The bands are nested.
#Because there are terms arbitrarily close to 1, 
#the top of the band can never go below 1.
#By constrast, here is the band that would be needed
#if we used the limit as the center.
abline(v=41, col = "green", lty = 2)
abline(h = c(0.97, 1.03), col = "green", lty = 2) 


#Topic 2 -- Lim sup and lim inf of a sequence

#My terrier digs a hole every day.
#But she often feels lazy and does not use her full capacity.
#As she grows up, her excavation potential stabilizes.

terrier <- function(n) 2 + runif(1, min = -0.2 - 4/n , max = 4/n )
terrierVec <- numeric(300)
for (n in 1:300) terrierVec[n] <- terrier(n)
plot(1:200,terrierVec[1:200], ylim = c(1.5, 2.1))
#This sequence does not have a limit!

#Find the "dominant" elements that will never be surpassed.
maxEle <- logical(200)
for (i in 1:200)
  maxEle[i] <- (terrierVec[i] > max(terrierVec[(i+1):300])) 
maxes <- which(maxEle)
#Plot the dominant elements as red points
points(maxes,terrierVec[maxes], col = "red") 
#Every time we pass a dominant element,
#the supremum of the rest of the sequence becomes less.
#This supremum approaches a limit of 2.
abline(h=2, col = "red")
#All remaining elements are less than 2 + epsilon.
abline(h=terrierVec[maxes[12]], col = "red", lty = 2)
abline(v=maxes[12], col = "red", lty = 2)
#The lim sup of this sequence is 2.

#We can do the same thing with "subdominant" elements
minEle <- logical(200)
for (i in 1:200)
  minEle[i] <- (terrierVec[i] < min(terrierVec[(i+1):300])) 
mins <- which(minEle) 
#Plot them in green
points(mins,terrierVec[mins], col = "green") 
#Every time we pass a subdominant element, the infimum of the rest of the sequence becomes greater.
#This infimum approaches a limit of 1.8.
abline(h=1.8, col = "green")
#All remaining elements are greater than 1.8 - epsilon.
abline(h=terrierVec[mins[7]], col = "green", lty = 2)
abline(v=mins[7], col = "green", lty = 2)
#The lim inf of this sequence is 1.8.

#Now we have a proof of Bolzano-Weierstrass for a bounded sequence.
#If there are infinitely many dominant elements, they form a decreasing subsequence.
#Otherwise we go beyond the last one and select an increasing subsequence.
#Either way, this bounded monotone subsequence converges.
 

