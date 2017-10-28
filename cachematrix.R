## The following functions are used to cache the inverse of a square, invertible
## matrix so that the inverse calculation only has to be done once. 

## This function creates a special "matrix" object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated (and the 
## matrix has not changed), then the cachsolve will retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
