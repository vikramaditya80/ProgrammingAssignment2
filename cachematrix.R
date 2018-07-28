## This file contains the definition of two functions named makeCacheMatrix() and cacheSolve().
## These functions facilitate caching of the inverse of a matrix.

## This function accepts a matrix as formal argument and returns a named list of four functions.
## Along with the sub functions, it creates an environment for caching of the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the cache variable 'i'
  i <- NULL
  
  ##1) Assign a new value to the matrix,
  ##2)Clear the cache variable
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##Return the matrix
  get <- function() x
  
  ##Assign a new value to the inverse
  setinv <- function(inv) i <<- inv
  
  ##Return the cached value of inverse
  getinv <- function() i
  
  ##Return the functions as a list of named elements
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function accepts an object of type makeCacheMatrix() as a formal argument
## and returns the inverse of the matrix. It makes use of the cache provided by
## makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ##Check for cached value of inverse and return if the value exists
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##1) Compute inverse of the new matrix
  ##2) Store the inverse in cache
  ##3) Return the inverse to calling environment
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}