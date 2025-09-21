# Assignment 2 - Importing Data and Function Evaluation in R

# Vector provided in assignment
assignment2 <- c(16, 18, 14, 22, 27, 17, 19, 17, 17, 22, 20, 22)

# Broken Version (given in instructions) 
myMean <- function(assignment2) {
  return(sum(assignment) / length(someData))
}

# Run broken version to capture error
# Expected: Error in myMean(assignment2) : object 'assignment' not found
try(myMean(assignment2))


# Corrected Version 
myMean_fixed <- function(x) {
  return(sum(x) / length(x))
}

# Run corrected version to get actual mean
myMean_fixed(assignment2)

# New Output:
# [1] 19.25

# myMean_fixed is now fully functional!
