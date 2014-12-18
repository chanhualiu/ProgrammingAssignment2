## The inversion of a matrix is usually a time consuming operation.
## In order to reduce the computing overhead, if the inverse of a 
## matrix is needed in many places, cacheSolve is used to calculate 
## the inverse of a matrix in an efficient way. If the inversion of
## a matrix has been calcualted, cacheSolve will just retrieve its
## cached value instead of re-calculation every time. cacheSolve needs
## to work with a special cached matrix created by makeCacheMatrix.

## makeCacheMatrix creates a special matrix with cached inverse matrix.
##   The returned "matrix" is a list which consists of 4 functions.
##   1. set --> set the value of the matrix
##   2. get --> get the value of the matrix
##   3. set.inv --> set the value of the inverse matrix
##   4. get.inv --> get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## cacheSolve returns the inverse of a matrix. The matrix needs to 
## be a special matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get.inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inv(inv)
  inv
}
