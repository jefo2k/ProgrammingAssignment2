## makeCacheMatrix creates a special "matrix" object that can cache its inverse, which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the solve(inverse matrix)
## get the value of the solve(inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() {
          x
     }
     setsolve <- function(solve) {
          m <<- solve
     }
     getsolve <- function() {
          m
     }
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## calculates the mean of the special "matrix" created with makeCacheMatrix. 
## First checks to see if the inverse matrix has already been calculated.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
