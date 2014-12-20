## makeCacheMatrix and cacheSolve are a pair of functions that can
## be used to compute the inverse of a given matrix, and cache the
## the result to avoid repeating the calculation.

## makeCacheMatrix creates a list object containing functions to: 
## 1. get the matrix
## 2. set the inverse of the matrix
## 3. get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve takes the function, x, and simply returns the cached 
## inverse, i, if i is not NULL. Otherwise cacheSolve will compute 
## the inverse, cache the value using "setinv(i)", and return it.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached inverse")
            return(i)
      }
      else {
            data <- x$get()
            i <- solve(data, ...)
            x$setinv(i)
            return(i)
      }
}
