## This function creates a special "matrix" object that can store its inverse in an attempt to avoid repeated calculations.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # Start with no cached inverse
  
  ## Function to update the matrix (and reset the inverse)
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL  # Reset inverse when matrix changes
  }
  
  ## Function to get the stored matrix
  get <- function() x
  
  ## Function to store the inverse
  setInverse <- function(newInverse) inverse <<- newInverse
  
  ## Function to get the stored inverse
  getInverse <- function() inverse
  
  ## Return a list of these functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, 
##it retrieves it from the cache instead of recalculating.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()  # Get the cached inverse
  
  ## If the inverse is already stored, return it
  if (!is.null(inverse)) {
    message("Getting cached inverse...")
    return(inverse)
  }
  
  ## Otherwise, calculate the inverse
  matrix <- x$get()  # Get the stored matrix
  inverse <- solve(matrix, ...)  # Compute its inverse
  x$setInverse(inverse)  # Save the inverse
  
  inverse  # Return the inverse
}

# As an exmaple of how it should work. Create a simple 2x2 matrix
my_matrix <- matrix(c(4, 7, 2, 6), 2, 2)

# Create a special matrix object
cachedMatrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse (first time)
cacheSolve(cachedMatrix)

# Retrieve the cached inverse (second time)
cacheSolve(cachedMatrix)
