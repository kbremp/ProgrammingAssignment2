## This pair of functions works together to cache the inverse of
## of a matrix so that the inverse can be computed one time and
## not repeatedly computed, saving time and resources.

## makeCacheMatrix creates a "special" matrix which can cache its
## inverse. This function is a list of four functions which: set
## the value of the matrix, get the value of the matrix, set the 
## value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with
## makeCacheMatrix after it checks to see if the inverse has already
## been calculated and cached, in which case it retrieves the inverse
## without repeating the calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
