## makeCacheMatrix() is used to enter the value of a matrix, and to pull/store
## the inverse of the matrix from/in cache. cacheSolve() calculates the inverse
## of a matrix, unless it has already been stored in cache, in which case it
## simply prints the inverse of a matrix.

##makeCacheMatrix() uses a matrix as input and defines four functions that can
##be called as a subset of makeCacheMatrix:
##1. set (sets the value of a matrix) 
##2. get (prints the value of a previously assigned matrix)
##3. setinv (sets the inverse of previously assigned matrix)
##4. getinv (prints the inverse of previously assigned matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() calculates the inverse of a matrix that may or may not have
## already been called with makeCacheMatrix. cacheSolve first checks to see if
## the inverse has already been calculated--if so, it prints the inverse after
## pulling it from cache and skips the calculation. Otherwise, it calculates and
## prints the inverse of the matrix and stores it in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}