## This assignments consists of 2 function which are interlinked, so they need 
## to be both run at the same time.
## The main objective is to cache the inverse of a matrix

## makeCacheMatrix is a function that stores a list of functions, 
## which we will need for caching the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) s <<- solve
  get_inv <- function() s
 ## Saving all functions in a list
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function now calculates and caches the inverse of the matrix.

cacheSolve <- function(x, ...) {
  s <- x$get_inv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$set_inv(s)
## Returning the inverse of the matrix x
  s
}
## Use the functions 'makeCacheMatrix' and 'cacheSolve' to calculate and cache an inverse matrix
