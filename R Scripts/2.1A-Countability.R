#Math 23 Script 2.1A-Countability.R
#Module 2, Week 1
#Demonstrates the countability of the positive rational numbers.

#Last modified September 24, 2014 by Paul Bamberg

#Topic 1 - The set of ordered pairs of natural numbers is countable
par(mar=c(1 , 2, 5, 2))   
plot(1:6, 1:6, ylim = c(6.5,1), type="n", , ylab="", axes =FALSE,
     main="Pairs of positive integers are countable") 
axis(3); axis(2)
serial <- 1     #each pair is assigned a serial number, shown in red.
plotpair <- function(x,y) {
  text(x+0.1,y,(paste("(",x,",",y,")")))
  text(x+0.1,y+0.3,paste(serial),col="red")
  serial <<- serial+1   #changes global variable
}
plotpair(1,1)
plotpair(1,2); plotpair(2,1)
plotpair(1,3); plotpair(2,2); plotpair(3,1)
plotpair(1,4); plotpair(2,3); plotpair(3,2);plotpair(4,1)


#Topic 2 - The set of positive rational numbers is countable
#When we do the same thing with fractions
#we have to skip ones that are not in lowest terms.

plot(1:6, 1:6, ylim = c(6.5,1), type="n", xlab=" " , ylab="Denominator", axes =FALSE,  main="Positive rational numbers are countable") 
axis(3); axis(2)
mtext("Numerator", line = 1)
serial <- 1
plotfrac <- function(x,y) {
  text(x,y,(paste(x,"/",y)),col="blue")
  text(x,y+0.3,paste(serial),col="red")
  serial <<- serial+1   #changes global variable
}
plotfrac(1,1)
plotfrac(1,2); plotfrac(2,1)
plotfrac(1,3); text(2,2,"skip"); plotfrac(3,1)
plotfrac(1,4);  plotfrac(2,3); plotfrac(3,2); plotfrac(4,1)
plotfrac(1,5); text(2,4,"skip");text(3,3,"skip");text(4,2,"skip");  plotfrac(5,1)
plotfrac(1,6);  plotfrac(2,5); plotfrac(3,4); plotfrac(4,3); plotfrac(5,2); plotfrac(6,1)

#We have established a bijection between the natural numbers (in red)
#and the positive rational numbers (in blue)

