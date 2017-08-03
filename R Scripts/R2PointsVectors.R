#Math 23 Script R2PointsVectors.R

#An element of R^2 is just a list of two numbers
A <- c(1,2)
#We can think of this as a point and plot it
plot(A[1], A[2], xlim = c(0,5),ylim = c(-1,5))  
B <- c(4,4)  #another point
points(B[1], B[2])     #add it to the plot
v <- B - A; v  #difference of two points is a vector
arrows(A[1],A[2], B[1], B[2])
#We can also show the same vector with its "tail" at the origin
arrows(0,0,v[1],v[2])
w <- c(1,-1)   #a second vector
arrows(0,0,w[1],w[2])   #plot it from the origin
u <- v + w; u  #add the two vectors
arrows(0,0,u[1], u[2])    #plot the sum from the origin
#the sum is a diagonal of a parallelogram
arrows(w[1], w[2], u[1], u[2])
arrows(v[1], v[2], u[1],u[2])
C = B + w; C   #add a point to a vector to get another point
points(C[1],C[2])    #plot the new point
arrows(B[1],B[2], C[1], C[2])   #a vector connects two points
#Notice that the vector sum connects point A to point C.
arrows(A[1], A[2], A[1] + u[1], A[2] + u[2])

