## Functions that cache the inverse of a matrix
##
## How to Use:
## Source this R file
## m <- makeCacheMatrix(matrix(c(2, 3, 0, 1), c(2, -5)))
## cacheSolve(m)
## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inv) i <<- inv
   getinverse <- function() i
   list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
   )
}

## Calculate the inverse of the special "matrix" created with makeCacheMatrix,
## Purpose: Reusing cached result if it is available

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   invrs <- x$getinverse()
   if(!is.null(invrs)) {
      message("getting cached data")
      return(invrs)
   }
   m <- x$get()
   invrs <- solve(m, ...)
   x$setinverse(invrs)
   invrs
}
