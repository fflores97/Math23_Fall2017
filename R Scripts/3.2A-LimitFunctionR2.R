#Math 23 Script3.2A-LimitFunctionR^2

#Last modified November 2, 2014 by Paul Bamberg

library(plotrix)

#Topic 1 - Sequences that converge to the origin
N <- 50         #how many points in a sequence
iSeq <- 1:N    #sequence of integers
zSeq <- rep(0,N); zSeq
recip <- 1/iSeq; head(recip)
recip2 <- 2/iSeq; head(recip2)
invSq <- 1/iSeq^2; head(invSq)
altRecip <- (-1)^iSeq/iSeq; head(altRecip)

par(mar=c(1,1,1,1))
plot(NULL, xlim = c(-0.3,1), ylim = c(-0.3,1), axes = FALSE, asp = 1)
axis(1, pos = 0); axis(2, pos = 0)
#This function plots a sequence and shows some function values
plotseq <- function(x, y, f, color) {
  points(x,y, type = "b", pch = ".", cex = 2, col = color)
  for (i in c(2,5,10,21)) {
    text(x[i]+0.02, y[i], paste(round(f(x[i], y[i]), digits = 3)) , cex = 0.8, col = color)
  }
}
clear <- function() {
  plot(NULL, xlim = c(-0.3,1), ylim = c(-0.3,1), axes = FALSE, asp = 1)
  axis(1, pos = 0); axis(2, pos = 0)
}

#Topic 2 - Evaluating functions along these sequences
r <- function(x,y) sqrt(x^2+y^2)  #distance from the origin
#Test that the function is working
plotseq(recip, recip, r, "red")
plotseq(recip, invSq, r, "blue")
plotseq(recip, recip2, r, "green")
 
#Here is a function with no limit at the origin
g <- function(x,y)  (y^2 - x^2)/(x^2 + y^2)
clear()
plotseq(recip, recip, g, "red")      #always zero
plotseq(recip, recip2, g, "green")   #always 0.6
plotseq(recip, invSq, g, "blue")     #approaches a limit of 1

#Sometimes you can find a sequence where the function values have no limit
h <- function(x,y)  (x*y)/(x^2 + y^2)
clear()
plotseq(recip2, altRecip, h, "red")  #values are alternately positive and negative 

#Here is a function where approaching along any straight line gives a limit of zero
hh <- function(x,y)  (x^2*y)/(x^4 + y^2)
clear()
plotseq(zSeq, recip, hh, "red") #always zero
plotseq(recip, recip, hh, "blue") #limit is zero
plotseq(recip, recip2, hh, "green") #limit is zero
plotseq(recip, invSq, hh, "magenta") #always 1/2

#The problem is clear - in any disk around the origin are points where hh = 0 and where hh = 1
clear()
plotseq(zSeq, recip, hh, "red") #always zero
plotseq(recip, invSq, hh, "blue") #always 1/2
draw.circle(0,0, 0.25)
draw.circle(0,0, 0.1)      #no improvement




#Here is a function that looks like zero over zero but has a limit
ff <- function(x,y) (x*y^2)/(x^2+y^2)
clear()
plotseq(zSeq, recip, ff, "red") #always zero
plotseq(recip, recip, ff, "blue") #limit is zero
plotseq(recip, recip2, ff, "green") #limit is zero
plotseq(recip, invSq, ff, "magenta") #limit is zero


