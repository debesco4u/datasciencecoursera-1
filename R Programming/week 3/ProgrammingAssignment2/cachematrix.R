# R Programming - week 3
# Programming Assignment 2: Lexical Scoping

# Introduction:
## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## The aim of the assignment is to write a pair of functions that cache the inverse of a matrix.

##  1)makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inverse <<- solve
  getSolve <- function() inverse
  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##  2)cacheSolve: This function computes the inverse (using the solve() function) of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
##  changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Try to get the matrix from cache
  inverse <- x$getSolve()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## if it skip the if clause it means that the cache is empty: then it gets the matrix data,
  ## solve the inverse, set the cache and return the inverse.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setSolve(inverse)
  inverse
}
