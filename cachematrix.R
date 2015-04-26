# This file contains two functions:
# The 'makeCacheMatrix' function creates a a matrix
# and can cache its inverse if it already has been calculated.
# 
# The 'cacheSolve' function takes the special 'matrix' created 
# by makeCacheMatrix and returns the inverse from the cache if
# it has been calculated before otherwise it calculates the
# inverse and stores it in the cache.
#
# Example:
# m <- makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(m)

# The following function creates a special "matrix", which is
# really a list containing a function to
#
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of matrix
# 4.  get the value of the inverse o matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) inverseMatrix <<- solve
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

# The following function calculates the inverse of a matrix created with
# the makeCacheMatrix function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse matrix in the cache via the 
# `setInverseMatrix`function.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  #if inverseMatrix is not null then return cached result
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}