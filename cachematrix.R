## Makes a special type of matrix that can have its inverse cached

## Makes a data structure that acts like a matrix whose inverse is cached
## after the first time it is caculated

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) {
    matrixInverse <<- inv
  }
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Takes a cacheMatrix and either calculates and saves the
## inverse or returns the saved inverse

cacheSolve <- function(x, ...) {
     matrixInverse <- x$getInverse()
     if(!is.null(matrixInverse)) {
       message("getting cached data")
       return(matrixInverse)
     }
     data <- x$get()
     matrixInverse <- solve(data, ...)
     x$setInverse(matrixInverse)
     matrixInverse
}
