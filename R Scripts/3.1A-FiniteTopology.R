#Math 23 Script 3.1A-FiniteTopology.R

#Last modified October 24, 2014 by Paul Bamberg

#install.packages("plotrix")   #comment this out after executing it once
library(plotrix)   #for drawing circles

#Topic 1 - The "standard" Web site graph, used in notes and examples
par(mar=c(2,2,2,2)) #leave small margins to maximize space
plot(NULL, xlim = c(0.5,3.5),ylim = c(0.5,2.5), xlab = "", ylab = "", axes = FALSE)

#Specify where to draw the circles that represent Web pages
centersX <- c(1,2,3,1,2,3); centersY <- c(1.2,0.8,1.2,1.8,2.2,1.8)
for (i in 1:6) {
  draw.circle(centersX[i],centersY[i],0.25); text(centersX[i],centersY[i]-0.1,paste(i))
}
links <- matrix(c(0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,0),
                6,byrow = FALSE)
for(i in 1:6){
  for (j in 1:6) {
    if (links[i,j] > 0)
      arrows(centersX[j], centersY[j], centersX[i], centersY[i],
             col = ifelse(links[j,i] > 0, "red", "blue"))
  } 
}

#Open sets are ones that cannot be reached by a link from outside the set.
#Examples: {2}, {45}, {123}, {456}
#Unions like {245} and {12345} are also open sets.
#So are the empty set and the set X = {123456}

#Closed sets are the complements of open sets.
#Examples: {13456}, {1236}, {456}, {123}
#Intersections like {136} and {6} are also closed sets.
#So are the empty set and the set X = {123456}
#If you choose page 4 and keep following outgoing links, 
#you find the closed set {456}

#Every set has an interior (largest open set inside it),
#a closure (smallest closed set that contains it),
#and a boundary (difference between closure and interior)

#Find the interior, closure, and boundary of this set
which(rbinom(6,1,0.5) > 0)

#Topic 2 - Drawing a random graph to create a different topology on the same set

plot(NULL, xlim = c(0.5,3.5),ylim = c(0.5,2.5), xlab = "", ylab = "", axes = FALSE)
for (i in 1:6) {
  draw.circle(centersX[i],centersY[i],0.25); text(centersX[i],centersY[i]-0.1,paste(i))
}
linkProb <- 0.08   #probability of a random link
links <- matrix(rbinom(36, 1, linkProb),6) #matrix with a few scattered 1s
twoWay <- sample(1:6)
#Generate two two-way links
for (i in c(1,3)) {
  links[twoWay[i],twoWay[i+1]]=1; links[twoWay[i+1],twoWay[i]]=1
}
#Generate a one-way link between the two remaining nodes.
links[twoWay[5],twoWay[6]]=1 #make sure everything has at least one link
for (i in 1:6) links[i,i] = 0  #nothing links to itself
#Here is the resulting matrix.
links  #each column shows links out of a page

for(i in 1:6){
  for (j in 1:6) {
    if (links[i,j] > 0)
      arrows(centersX[j], centersY[j], centersX[i], centersY[i],
             col = ifelse(links[j,i] > 0, "red", "blue"))
  } 
}

#You can analyze the topology that was just generated.

#Find closed sets by choosing a node and following links from it.
#Identify open sets by the property that there are no incoming links.
#The empty set and the set {1,2,3,4,5,6} are both open and closed.
#Intersections and unions of open sets are also open.
#Intersections and unions of closed sets are also closed.

#Every set has an interior (largest open set inside it),
#a closure (smallest closed set that contains it),
#and a boundary (difference between closure and interior)

#Find the interior, closure, and boundary of this set
which(rbinom(6,1,0.5) > 0)
  