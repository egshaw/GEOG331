#my new script

#Question 2 - practicing vector operations in R
#some unit vectors, i,j and k and a vector, l
i <- (c(1L, 0L, 0L, 0L))
j <- (c(0L, 1L, 0L, 0L))
k <- (c(0L, 0L, 1L, 0L))
l <- (c(3, 5, 7, sqrt(2)))
# I don't know how I feel about the element-wise operations
l %*% i
l %o% k
j + k
i * j * k
