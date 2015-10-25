makeCacheMatrix <- function(x = matrix()) { ## function to create a matrix ##
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse ## to set inverse of a value ##
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {    ## function to return the cache value stored ##
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
