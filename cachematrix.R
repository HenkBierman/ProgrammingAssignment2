## Matrix inversion is usually a costly computation and there may be benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. This program consists of a pair of functions that cache the inverse of a matrix.
## start condition is that the matrix x is invertable

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialise inverse matrix
  i <- NULL
  ## determine the methods of "matrix" x
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
  ## return a list with the methods for "matrix" x to resp. set and get the originating matrix
  ## and to set resp. get its inverse.
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
## start condition is that the matrix x is invertable, and that a "matrix" object was created with the 
## makeCacheMatrix function.

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
