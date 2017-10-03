## Assignment 2
## Caching the Inverse of a Matrix

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) inv <<- inversion
  getinversion <- function() inv
  list(set = set, get = get,
       setinversion = setinversion
       getinversion = getinversion)
}


## Computes the inverse of the special matrix returned by the function above.
## If the inverse has been calculated (& matrix not changed) retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversion()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversion(inv)
  inv
}
