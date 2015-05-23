## File info
# Author: Sandeep Gore
# Date of creation: 23-May-2015

## About the code
# The code returns an inverse of a square matrix from a cached memory if the inverse is 
# already estimated for a given matrix.
# Else, it estimates afresh the inverse of a given matrix. 
# The code assumes that user always enters an invertible matrix
# Example of a function call: 
#    mat <- matrix(rnorm(16), 4, 4)
#    x <- makeCacheMatrix(mat)
#    cacheSolve(x)

# The function makeCacheMatrix returns a list of functions defined to set and get the input matrix
# and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function cacheSolve estimates the inverse of an input matrix if the 
# the inverse does not exist in cache memory. 
# If the inverse exists, it simply pulls the inverse from the memory and displays it
# thus saving computational time. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
