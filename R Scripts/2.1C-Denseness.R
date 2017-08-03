#Math 23 Script 2.1C-Denseness 
Module 2, Week 4, subsection 1.2

#Last modified September 24, 2014 by Paul Bamberg

#Topic 1 - Placing rational numbers between any two real numbers
#Choose two real numbers that are close together
a <- runif(1); a   #between 0 and 1
b <- a + runif(1, min = 0.0001, max = 0.0002); b

par(mar=c(4 , 4, 5, 2))   
plot(NULL,NULL, xlim = c(a-0.00005,b+.00005), ylim = c(-1,1), type="n", ylab="", 
     axes =FALSE, main="The rational numbers are dense") 
axis(3,pos = 0)
points(c(a,b), c(0,0), col = "red")  #Show points a and b
text(a, -0.07, "a", col = "red")
text(b, -0.07, "b", col = "red")
#Now compute a big denominator
N <- round(2/(b-a)); N    #2 in the numerator will let us show two rationals in (a,b)
#By the Archimedean property there is a multiple of 1/N that is bigger than a.
m <- trunc(N*a)+1; m
#Plot two rational numbers with denominator N. 
points(m/N, 0, pch = 20, col = "green" )
points((m+1)/N, 0, pch = 20, col = "green" )
text(m/N, -0.07, "q1", col = "green");
text((m+1)/N, -0.07, "q2", col = "green")

