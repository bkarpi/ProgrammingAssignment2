## Two functions that cache the inverse of a matrix
##
##
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
  i <- NULL
  
  ## Cache matrix 
  ## Initialize and cache inverse matrix
  set <- function( y ) {
    m <<- y
    i <<- NULL
  }
  
  ## Function get original matrix
  get <- function() m
  
  ## Function set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  
  ## Function get the inverse of the matrix
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get inverse matrix
  i <- x$getInverse()
  
  ## If inverse matrix not null, return cached inverse
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## get original matrix and calculate inverse
  data <- x$get()
  i <- solve(data)
  
  ## Set the inverse matrix
  x$setInverse(i)
  
  ## Return the inverse matrix
  i
}
