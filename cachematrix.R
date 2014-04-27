## These 2 functions can be used to compute the inverse of a matrix
##  - makeCacheMatrix creates a list including the matrix
## and cache of inverse if it has been computed
## - cacheSolve computes the inverse of a matrix
## if already computed, it gets the cached data instead of computing.

## Function takes arguement 'x' (square matrix)
## Returns a list of 4 items:
## 1. set; 2. get; 3. setInverse; 4. getInverse
## - get() returns the matrix
## - getInverse() returns the inverse of the matrix (if calculated)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of a square matrix 'x'
## that has been created by makeCacheMatrix.
## If the inverse matrix has not been calculated,
## cacheSolve will calculate, cache, and return.
## If it has been calculated,
## it will return the cached matrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
