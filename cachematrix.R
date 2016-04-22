## These functions allow the user to compute, 
## cache, and retrieve the inverse of a matrix.

## The function makeCacheMatrix creates a matrix 
## object that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invX <<- solve
  getInverse <- function() invX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve computes the inverse of
## the matrix object returned by makeCacheMatrix. If
## the inverse has already been calculated, cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invX <- x$getInverse()
  if(!is.null(invX)) {
    message("getting cached inverse matrix")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setInverse(invX)
  invX
}
