## Matrix inversion is usually a costly computation and there may be benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. This program consists of a pair of functions that cache the inverse of a matrix.
## start condition is that the matrix x is invertable

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() 
    x
  setinverse <- function(inverse) 
    i <<- inverse
  getinverse <- function() 
    i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## if the inverse of x is cached, then Return cached inverse of 'x'

  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

## if the inverse of x has not been cached yet, then calculate the inverse of 'x', 
## cache and return the calculated inverse of 'x'     

  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
