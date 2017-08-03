#Math 23 Script 1.3A-RowReduction.R
#Module 1, Week 3, subsections 1.1 and 1.2

#Last modified: September 10, 2014 by Paul Bamberg

#Topic 1 - Row reduction to solve two equations, two unknowns
#we start with a system of linear equations, e.g.
#2x + 2y = 8
#x + 4y = 7

#Make the coefficients and the value on the right into a vector for each equation.
r1 <- c(2,2,8); r2 <- c(1,4,7)
#Bind the rows into a matrix
rbind(r1,r2)
#Now use standard techniques from elementary algebra
r1 <- r1/2; rbind(r1,r2)  #divide top row by 2 -- x+y = 4
r2 <- r2 - r1;rbind(r1,r2) #subtract first row from second -- 3y = 3
r2 <- r2/3; rbind(r1,r2)    #make the first nonzero entry in r2 be 1
r1 <- r1 - r2;rbind(r1,r2)  #subtract the second row from the first
#At this point we have converted the equations to
#1x + 0y = 3
#0x + 1y = 1
#The solution is obvious by inspection: x = 3, y = 1
#In this case there is a quicker way: 
solve(matrix(c(2,1,2,4),2),c(8,7))

#However, there are cases where solve() fails.
#2x + 4y = 14
#3x + 6y = 21
r1 <- c(2,4,14); r2 <- c(3,6,21); rbind(r1,r2)
#First try the solve() approach.
A <- matrix(c(2,3,4,6),2); w <- c(14,21)
solve(A,w)
#All we get is an error message.
#Use row reduction instead:
r1 <- r1/2;rbind(r1,r2)   #divide top row by 2
r2 <- r2 - 3*r1;rbind(r1,r2) #subtract 3 times first row from second
#We have converted the equations to the single equation x+2y = 7.
#Now we can choose any value for y, and then x = 7-2y.
#We have found many solutions to the matrix equation Av = w, e.g.
y <- 2; x <- 7-2*y; v<- c(x,y); A%*% v
y <- -1; x <- 7-2*y; v<- c(x,y); A%*% v

#There is yet a third possibility: no solution at all
#2x + 4y = 14
#3x + 6y = 24
r1 <- c(2,4,14); r2 <- c(3,6,24); rbind(r1,r2)

#Use row reduction as before
r1 <- r1/2;rbind(r1,r2)   #divide top row by 2
r2 <- r2 - 3*r1;rbind(r1,r2) #subtract 3 times first row from second

#Now the bottom equation is 0x +0y = 3, which has no solution!

#Topic 2 - Row reduction to solve three equations, three unknowns
#Again, start with a system of linear equations, e.g.
#2x + 3y -  z = 4
#3x -  y + 2z = 11
# x + 2y - 3z = -5
#The process is exactly the same.
r1 <- c(2,3,-1,4); r2<-c(3,-1,2,11); r3<-c(1,2,-3,-5)
rbind(r1,r2,r3)    #display as a matrix
#Our first goal is to make column 1 into a "pivotal column"
r1 <- r1/2; rbind(r1,r2,r3); #get a "pivotal 1"
r2 <- r2 - 3*r1; r3 <- r3 - r1; rbind(r1,r2,r3) #clear the column

#Our next goal is to make column 2 into a "pivotal column"
r2 <- r2/(-5.5); rbind(r1,r2,r3); #get a "pivotal 1"
r1 <- r1 - 1.5*r2; r3 <- r3 - 0.5*r2; rbind(r1,r2,r3) #clear the column

#Our final goal is to make column 3 into a "pivotal column"
r3 <- r3/(r3[3]); rbind(r1,r2,r3); #get a "pivotal 1"
r1 <- r1 - r1[3]*r3; r2 <- r2 - r2[3]*r3; rbind(r1,r2,r3) #clear the column

#The solution is x=2, y=1, z=3.

#Topic 3 - Row reduction by elementary matrices
#We start with the same 3 x 3 matrix
r1 <- c(2,3,-1,4); r2<-c(3,-1,2,11); r3<-c(1,2,-3,-5)
A <- rbind(r1,r2,r3); A    #display as a matrix
E1 <- matrix(c(1/2,0,0,0,1,0,0,0,1),3); E1  #divides top row by 2
E1%*%A
E2 <- matrix(c(1,-3,0,0,1,0,0,0,1),3); E2  #subtracts 3 x top row from second row
E2%*%E1%*%A
E3 <- matrix(c(1,-0,-1,0,1,0,0,0,1),3); E3  #subtracts  top row from third row
E3%*%E2%*%E1%*%A
E4 <- matrix(c(1,0,0,0,-1/5.5,0,0,0,1),3); E4  #divides second row by -5.5
E4%*%E3%*%E2%*%E1%*%A
E5 <- matrix(c(1,0,0,-1.5,1,0,0,0,1),3); E5  #subtracts 1.5 x second row from top row
E5%*%E4%*%E3%*%E2%*%E1%*%A
E6 <- matrix(c(1,-0,0,0,1,-0.5,0,0,1),3); E6  #subtracts 0.5 x second row from third row
AA<-round(E6%*%E5%*%E4%*%E3%*%E2%*%E1%*%A, digits = 6); AA
E7 <- matrix(c(1,0,0,0,1,0,0,0,-11/24),3); E7  #divides third row by -24/11
E7%*%AA
E8 <- matrix(c(1,0,0,0,1,0,-5/11,0,1),3); E2  #subtracts 5/11 x third row from top row
E8%*%E7%*%AA
E9 <- matrix(c(1,-0,0,0,1,0,0,7/11,1),3); E2  #adds 7/11 x third row to second row
AAA<-round(E9%*%E8%*%E7%*%AA,digits = 6); AAA
E <- E9%*%E8%*%E7%*%E6%*%E5%*%E4%*%E3%*%E2%*%E1; E #this matrix does the row reduction
solve(E)  #E is always invertible; in this case the inverse is A
#Even if A has no inverse, the matrix E is guaranteed to be invertible.
#E is the product of a lot of invertible elementary matrices!


#Topic 4 - Automating row reduction in R
#We need the Practical Math library
library("pracma")
r1 <- c(2,3,-1,4); r2<-c(3,-1,2,11); r3<-c(1,2,-3,-5)
A <- rbind(r1,r2,r3); A    #display as a matrix
rref(A)      #This function row-reduces the matrix!
#Beware -- if the columns of A are linearly dependent rref() may fail!
#Sometimes, appending one or more columns of zeroes will fix this error. 

#Topic 5 - Row reduction to solve equations in a finite field
#Define operators for doing arithmetic in Z_5
"%+5%" <- function(x,y) (x+y) %%5  #addition
"%-5%" <- function(x,y) (x-y) %%5  #subtraction
"%*5%" <- function(x,y) (x*y) %%5  #multiplication
"%/5%" <- function(x,y) (x*y*y*y) %%5  #division
#The last operation works because y^4 = 1 in Z_5

#Start with three equations in four unknowns.
#2x +  y + 3z +2w = 4
#3x + 4y +  z +3w = 1
# x + 2y + 4z +2w = 2

#The process is exactly the same.
r1 <- c(2,1,3,2,4); r2<-c(3,4,1,3,1); r3<-c(1,2,4,2,2)
rbind(r1,r2,r3)    #display as a matrix
#Our first goal is to make column 1 into a "pivotal column"
r1 <- r1%/5%2; rbind(r1,r2,r3); #get a "pivotal 1"
r2 <- r2 %-5% (3%*5%r1); r3 <- r3 %-5% r1; rbind(r1,r2,r3) #clear the column

#To get the next pivotal 1 in row 2, we must do a swap
temp <- r2; r2<- r3; r3<- temp; rbind(r1,r2,r3)

#Now we can make column 2 into a "pivotal column"
r2 <- r2%/5%4; rbind(r1,r2,r3); #get a "pivotal 1"
r1 <- r1 %-5% (3%*5%r2); rbind(r1,r2,r3) #clear the column

#Finally we can make column 3 into a "pivotal column"
r3 <- r3%/5%4; rbind(r1,r2,r3); #get a "pivotal 1"
r1 <- r1 %-5% (4%*5%r3); rbind(r1,r2,r3) #clear the column

#We can choose any value that we like for w, e.g.
w <- 3
#The top equation is x + 4w = 2, so
x <- 2 %-5% (4%*5%w)
y <- 0 %-5% (4%*5%w); z <- 0
#Here is our solution vector
v <- c(x,y,z,w); v
#Here is the matrix of coefficients from our original equations:
r1 <- c(2,1,3,2); r2<-c(3,4,1,3); r3<-c(1,2,4,2)
A <- rbind(r1,r2,r3); A   #display as a matrix
A %*% v %%5    #Our answer checks!

#A different value of w leads to a different answer.
w <- 1
#The top equation is x + 4w = 2, so
x = 2 %-5% (4%*5%w)
y <- 0 %-5% (4%*5%w); z <- 0
#Here is our new solution vector.
v2 <- c(x,y,z,w); v2
A %*% v2 %%5    #Our new answer also checks!

#Since we are working in Z_5, there are five different solutions, one for each w.
#If we worked with the real numbers, we would get infinitely many solutions.


