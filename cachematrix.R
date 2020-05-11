
# Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly. This API is to create a special matrix object that
## caches the inverse of itself.
##
## makeCacheMatrix: This function creates a special "matrix" object that can
##	cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix"
##	returned by makeCacheMatrix above. If the inverse has already been
##	calculated (and the matrix has not changed), then cacheSolve should
##	retrieve the inverse from the cache.

##

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## then cacheSolve returns the inverse from the cache.
##

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
## Notes:
##   1. Assumes that the matrix is invertible
##   2. Once inverse is cached, its not invalidated on further changes
##      to matrix
##
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  m <- solve(data) %*% data

  x$setInverse(m)
  

  m
}
