## R programming Coursera
## LMB
## In this assignment I will write a pair of functions to cache the
## inverse of a matrix

## First I write a function for a matrix object which is able to cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
            }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## Second, I write a function which computes the inverse of the matrix
## of macheCacheMatrix. If the inverse has already been calculated, 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}