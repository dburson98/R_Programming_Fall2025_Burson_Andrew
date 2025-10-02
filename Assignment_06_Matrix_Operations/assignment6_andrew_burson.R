# Assignment #6: Matrix Operations and Construction
# Andrew Burson

# Task 1: Matrix Addition & Subtraction
A <- matrix(c(2, 0, 1, 3), ncol = 2)
B <- matrix(c(5, 2, 4, -1), ncol = 2)
A + B
A - B

# Task 2: Diagonal Matrix
D <- diag(c(4, 1, 2, 3))
D

# Task 3: Custom 5x5 Matrix
M <- diag(3, 5)
M[1, 2:5] <- 1
M[2:5, 1] <- 2
M
