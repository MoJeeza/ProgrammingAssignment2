## Put comments here that give an overall description of what your
## functions do
##
## These functions demonstrate the implementation of lexical scoping 
# with in the R programming language - as opposed to dynamic scoping
# the are basicially two functions in this file: makeCacheMatrix and 
# cacheSolve


##
## Author: Mohsin Jessa
## Coursera Course: R-Programming (Prog-004)

# Write a short comment describing this function
# 
# this function makeCacheMatrix is being defined at the parent environment
# which is the global environment.

## 

makeCacheMatrix <- function(x = matrix()) {      ## by default a matrix is expected as the value for the formal argument x

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(solvex) {
        inv <<- solvex
    }
    getinv <- function() {
        inv
    }
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
