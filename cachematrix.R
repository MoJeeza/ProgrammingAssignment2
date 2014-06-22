## Put comments here that give an overall description of what your
## functions do
##
## These functions demonstrate the implementation of lexical scoping 
# with in the R programming language - as opposed to dynamic scoping
# there are basicially two functions in this file: makeCacheMatrix and 
# cacheSolve
# makeCacheMatrix is a function that is actually a list of 4 functions
# namely :get, set, setinv and getinv. 
# There is actually no matrix conversions done in any of these functions.
# These functions are used only for setting, caching and retrieving the matrix
# or the inverse of the matrix.
# cacheSolve on the other hand calls the four inner functions of the
# makeCacheMatrix function set or retrieve the matrix or inverse of the
# matrix. The inverse of the matrix is computed in this function.

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
##
## checks if the inverse of the matrix exists by calling the $getinv function
## if so, return that - ie cached value
## if not, compute the inverse and store it using $setinv function
##
cacheSolve <- function(x, ...) {        ## here x is the list of 4 functions
        ## Return a matrix that is the inverse of 'x'
    get_matrix_inv <- x$getinv()        ## check if the inverse of the matrix
                                        ## has been computed before. if so retrieve it
    if(!is.null(get_matrix_inv)) {     ## if get_matrix_inv is not null then an
                                        ## inv of the matrix exists, 
                                        ## so lets get that cached value
        
        message("getting cached data")  ## tell that we found the cached value
        return(get_matrix_inv)          ## return the cached value
    }                                   ## inverse of the matrix was not found so
    my_matrix <- x$get()                ## get the current matrix
    my_matrix_inv <- solve(my_matrix, ...)     ## compute the inverse of the matrix 
    x$setinv(my_matrix_inv)                     ## cache the inverse of the matrix
    my_matrix_inv                           ## return the inverse of the matrix
}
