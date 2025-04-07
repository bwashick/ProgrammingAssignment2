## This set of two functions calculates and caches the 
## inverse of a matrix.


## This function creates a special "matrix" object that
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" object returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix
## is unchanged), then cacheSolve retrieves the inverse 
## from the cache via the 'getinverse' function. Otherwise,
## the inverse is calculated via the 'setinverse' function. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
