#Math 23 Script 2Y-Angles3D.R
#Module 1, week 2, examples related to subsection 1.1 and 1.2

#Last modified September 3, 2014 by Paul Bamberg

source("1.2L-VectorLibrary.R")  #load the vector library

#Topic 1 - Angles between vectors in R^3
#The Goodyear blimp is located 1 mile east, 0.2 miles south, 1 mile up
#The MetLife blimp is located 0.5 miles west, 0.8 miles north, 0.3 miles up
#You point at one blimp with each arm. What is the angle between your arms?

vGood <- c(1,-0.2,1)
vMet <- c(-0.5, 0.8, 0.3)
angleBetween(vGood, vMet)

#Topic 2 - Angles and distances in a cube
#Define the eight vertices of the unit cube
#First the bottom face
B1 <- c(0,0,0); B2 <- B1+e1.3; B3 <- B2 + e2.3; B4 <- B1 + e2.3
#Now add e3.3 to get the vertices in the top face
T1 = B1 + e3.3; T2 = B2 + e3.3;  T3 = B3 + e3.3;  T4 = B4 + e3.3;  

#A body diagonal runs from B1 to T3
vBody <- T3 - B1; vBody

#What is its angle with a side of the bottom face?
angleBetween(vBody,e1.3)

#A face diagonal runs from B1 to B3
vFace <- B3 - B1; vFace

#What is the angle between a body diagonal and a face diagonal?
angleBetween(vBody,vFace)

#The center of the bottom face
pCenter <- (B3 + B1)/2

#The midpoint of one of the top edges
pTopMid = (T2+T3)/2

#What is the distance between these points?
Norm(pCenter-pTopMid)

#Topic 3 - Calculating airline mileage
#By modifying this script you can work out the length of any airline journey.
 
#You can look up latitudes and longitudes on latlong.com
#Boston is at latitude 42.35, longitude -71.05
latB <- 42.35; longB <- -71.05
#Oslo is at latitude 59.91, longitude 10.75
latO <- 59.91; longO <- 10.75
#Now calculate the unit vector for each city
#The capitalized trig functions take angles in degrees
B <- c(Cos(latB)*Cos(longB),Cos(latB)*Sin(longB),Sin(latB)); B #unit vector for Boston
O <- c(Cos(latO)*Cos(longO),Cos(latO)*Sin(longO),Sin(latO)); O #unit vector for Boston

#How long is the trip?
#Work out the distance in degrees from Boston to Oslo
degrees <- angleBetween(B,O); degrees
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers <- 10000*degrees/90
kilometers   #the trip is 5600 kilometers
miles <- kilometers*5/8; miles   #or 3508 miles

