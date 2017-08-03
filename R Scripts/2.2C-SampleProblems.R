Math 2.2C-SampleProblems.R

#Problem 1
s <- numeric(20); s0 <- 1; s[1] <- (2/3)*(s0+1)
for (n in 1:19) s[n+1]<- (2/3)*(s[n]+1)
s0;s   #appears to increase to a limit of 2
par(mar=c(2,2,1,1),pch=20)
plot(1:20,s  , type = "b" ,  ylim = c(0,2))

#Problem 2:
a <- -(-1)^(1:1000)/(1:1000); head(a)
sum(a); log(2)
#Keep just the positive terms
iOdd <- seq.int(1,999,2); head(iOdd)
head(a[iOdd])
sum(a[iOdd])
#Keep just the negative terms
iEven <- seq.int(2,1000,2); head(iEven)
head(a[iEven])
sum(a[iEven])
#For any fixed partial sum it is OK to sum the positive and negative terms separately.
sum(a[iOdd])+sum(a[iEven]); log(2)

#m is the first unused odd term; n is the first unused even term
m <- 1; n <-2; balance <- 0
balance  <- balance + 1/m; m <- m+2; balance  #collect some revenue
balance  <- balance - 1/n; n <- n+2; balance  #pay a bill
 

#Problem 4:
s <- function(n) ((n+2)/(n+1))*sin(n*pi/4)
sVec <- s(1:100) 


plot(1:100,sVec, type = "b" ,  ylim = c(-3,3))
#Convergent subsequence 1
i1 <- seq.int(4,100,4)
points(i1,sVec[i1], col = "red")
#Convergent subsequence 2
i2 <- seq.int(2,98,4)
points(i2,sVec[i2], col = "green")
#Convergent subsequence 3
i3 <- seq.int(1,97,8)
points(i3,sVec[i3], col = "magenta")

