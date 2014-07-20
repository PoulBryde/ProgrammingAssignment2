##
## Matrix inversion is usually a costly computation and therefore a caching 
## mechanism is provided for holding a matrix and its inverse. 
##

##
## Create a cached matrix that supports caching of the inverse of a matrix
## together with the matrix itself.
##
## Arguments:
##   x - the matrix that should be cached
##
## Returns:
##   a list object that provided accesors to the matrix and its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##
## Given a cached matrix the function returns the inverse of the matrix. If the 
## inverse already has been calculated it is returned, otherwise the inverse is 
## calculated, cached in the cached matrix and then returned.
##
## Arguments:
##   x - a cached matrix. The matrix must be invertable.
##
## Returns:
##   the inverse of the cached matrix
##
cacheSolve <- function(x, ...) {
  inverse <- cachedMatrix$getInverse()
  if (is.null(inverse)) {
    matrix <- cachedMatrix$get()
    inverse <- solve(matrix, ...)
    cachedMatrix$setInverse(inverse)
  }
  inverse
}

