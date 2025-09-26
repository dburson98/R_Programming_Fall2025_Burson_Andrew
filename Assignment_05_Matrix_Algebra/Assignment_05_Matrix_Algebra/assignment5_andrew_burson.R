 # Assignment #5: Matrix Algebra in R
# Andrew Burson

# Create the matrices
A <- matrix(1:100,  nrow = 10)
B <- matrix(1:1000, nrow = 10)

# Inspect dimensions
dim(A)  # 10 x 10
dim(B)  # 10 x 100 -- not square

# Compute inverse and determinant
# For A
invA <- tryCatch(solve(A), error = function(e) e)   # will error if singular
detA <- det(A)

# For B, capture errors (non-square)
invB <- tryCatch(solve(B), error = function(e) e)
detB <- tryCatch(det(B),   error = function(e) e)

# Print results (for your blog screenshots)
dim(A); dim(B)
detA
invA
detB
invB

