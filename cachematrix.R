
## This function creates a cache matrix which is able to hold the inverse of a square matrix. Since calculating this is potentially a costly operation in terms of computing resources, it is beneficial to cache the computation.
## It serves as a cache for that calculation.

makeCacheMatrix <- function(x = matrix()) { ##creating x as a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function checks if the inverse of the matrix, which is stored in the cache matrix, has already been computed.
## If so, the cached inversed matrix is retrieved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}