#Math 23 Script 2.1B-Uncountability.R
#Module 2, Week 1
#Demonstrates the uncountability of the real numbers in [0,1].

#Last modified September 24, 2014 by Paul Bamberg

#Topic 1 - Cantor's proof of uncountability
 
par(mar=c(5-2, 4, 4+2, 2))   #decrease bottom margin, increase top margin
plot(1:15, 1:15, ylim = c(15,1), xlim = c(1,30),type="n", , ylab="",
     axes =FALSE, main="Real numbers in [0,1] are uncountable") 
digits <- sample(0:9, 100, replace = TRUE) 

 
plotdigit <- function(n){ 
  q <- (n-1) %/% 10+1; r <- (n-1) %% 10; print(q);print(r)
  color <- ifelse(q-1 == r,"red","blue")
  text(r+4,q,paste(digits[n]),col= color)
}

#Assume for a contradiction that you can enumerate the real numbers in [0,1] 
for(j in 1:10) {
  text(1,j,paste(j,":"),col= "green")
  text(2,j,paste(0),col= "blue")
  text(3,j,paste("."),col= "blue")
  text(14,j,paste("..."),col= "blue")
}
 
for (i in 1:100) plotdigit(i)

#Now construct a number that cannot be on the list
text(1,11,paste("x:"),col= "green")
text(2,11,paste(0),col= "blue")
text(3,11,paste("."),col= "blue")
text(4,11,paste(9-digits[1]),col= "red")
text(5,11,paste(9-digits[12]),col= "red")
text(6,11,paste(9-digits[23]),col= "red")
text(7,11,paste(9-digits[34]),col= "red")
text(8,11,paste(9-digits[45]),col= "red")
text(9,11,paste(9-digits[56]),col= "red")
text(10,11,paste(9-digits[67]),col= "red")
text(11,11,paste(9-digits[78]),col= "red")
text(12,11,paste(9-digits[89]),col= "red")
text(13,11,paste(9-digits[100]),col= "red")
text(14,11,paste("..."),col= "red")

#The number x cannot appear anywhere on the list
#because it differs from number n in its nth digit

#Fans of St. John the Divine can construct a different number
#that cannot be on the list
text(1,12,paste("y:"),col= "magenta")
text(2,12,paste(0),col= "blue")
text(3,12,paste("."),col= "blue")
text(4,12,paste(ifelse(digits[1]==6, 1, 6)),col= "magenta")
text(5,12,paste(ifelse(digits[12]==6, 1, 6)),col= "magenta")
text(6,12,paste(ifelse(digits[23]==6, 1, 6)),col= "magenta")
text(7,12,paste(ifelse(digits[34]==6, 1, 6)),col= "magenta")
text(8,12,paste(ifelse(digits[45]==6, 1, 6)),col= "magenta")
text(9,12,paste(ifelse(digits[56]==6, 1, 6)),col= "magenta")
text(10,12,paste(ifelse(digits[67]==6, 1, 6)),col= "magenta")
text(11,12,paste(ifelse(digits[78]==6, 1, 6)),col= "magenta")
text(12,12,paste(ifelse(digits[89]==6, 1, 6)),col= "magenta")
text(13,12,paste(ifelse(digits[100]==6, 1, 6)),col= "magenta")
text(14,12,paste("..."),col= "magenta")

#The number y cannot appear anywhere on the list
#because it differs from number n in its nth digit

#Topic 2 - A different-looking version of the same argument
#Given a set S, finite or infinite, the "power set" 2^S
#is the collection of all subsets of S

#Here is a clever way to generate a random subset of the digits 1:9
S <- 1:9
subset(S,  sample(0:1, 9, replace = TRUE) > 0)

#Now we can define a function from S to its power set.
f <- function(n) subset(S,  sample(0:1, 9, replace = TRUE) > 0)

plot(1:15, 1:15, ylim = c(15,1), xlim = c(1,20),type="n", , ylab="",
     axes =FALSE, main="No function from S to its power set can be surjective") 

for(j in S) {
  text(1,j,paste(j,": {"),col= "green")
  text(6,j,paste(toString(f(j))),col= "blue")
  text(10,j, "}",col= "green")
}
#Now make a vector containing all the elements n of S for which n is not in f(n).
X <- c()     #this has to be done by hand!

text(1,10,paste("X: {"),col= "green")
text(6,10,paste(toString(X)),col= "red")
text(10,10, "}",col= "green")

#There is no way that X can be equal to any value f(n), since X and f(n)
#always disagree about the membership of n.
#So X is not in the image of f, and f cannot be surjective
#This construction works even when S is infinite.
#There cannot be a bijection between the natural numbers N
#and the power set, the collection of all subsets of N.
#So the collection of subsets of N must be uncountably infinite!
#Rerun this script from the definition of f to see another example.




