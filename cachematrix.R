## Put comments here that give an overall description of what your
## functions do
##
## These functions demonstrate the implementation of lexical scoping 
# with in the R programming language - as opposed to dynamic scoping
# there are basicially two functions in this file: makeCacheMatrix and 
# cacheSolve

# makeCacheMatrix is a function that is actually a list of 4 functions
# namely :get_my_matrix, set_my_matrix, set_the_inv and get_the_inv.
# There is actually no matrix conversions done in any of these 4 functions.
# These functions are used only for setting, caching and retrieving the matrix
# or the inverse of the matrix.

# cacheSolve on the other hand calls the four inner functions of the
# makeCacheMatrix function to set or retrieve the matrix or inverse of the
# matrix. The inverse of the matrix is computed in this function.

##
## Author: Mohsin Jessa
## Coursera Course: R-Programming (Prog-004)

# Write a short comment describing this function
# 
# this function makeCacheMatrix is being defined at the parent environment
# which is the global environment. This function will store/cache the matrix as well as it's inverse. The storing of the matrix is done via the function call to set

## 

makeCacheMatrix <- function(my_matrix = matrix()) {      ## by default a matrix is expected for the formal argument my_matrix

    store_inv_of_matrix <- NULL         ## initially we don't have inverse of the matrix
    set_my_matrix <- function(get_new_matrix) {
        my_matrix <<- get_new_matrix
        store_inv_of_matrix <<- NULL
    }
    get_my_matrix <- function() {
        my_matrix
    }
    set_the_inv <- function(get_inv_of_matrix) {
        store_inv_of_matrix <<- get_inv_of_matrix
    }
    get_the_inv <- function() {
        store_inv_of_matrix
    }
    list(set_my_matrix = set_my_matrix, 
         get_my_matrix = get_my_matrix,
         set_the_inv = set_the_inv,
         get_the_inv = get_the_inv)
}


## Write a short comment describing this function
##
## checks if the inverse of the matrix exists by calling the $get_the_inv function
## if so, then return the inverse  - ie cached value
## if not, compute the inverse and store it using $set_the_inv function
##
cacheSolve <- function(x, ...) {        ## here x is the list of 4 functions
        ## Return a matrix that is the inverse of 'x'
    get_matrix_inv <- x$get_the_inv()  ## check if the inverse of the matrix
                                       ## has been computed before. if so retrieve it
    if(!is.null(get_matrix_inv)) {     ## if get_matrix_inv is not null then an
                                       ## inv of the matrix exists, 
                                       ## so lets get that cached value
        
        message("getting cached data")  ## tell that we found the cached value
        return(get_matrix_inv)          ## return the cached value
    }                                   ## inverse of the matrix was not found so
    my_matrix <- x$get_my_matrix()          ## get the current matrix
    my_matrix_inv <- solve(my_matrix, ...)  ## compute the inverse of the matrix 
    x$set_the_inv(my_matrix_inv)            ## cache the inverse of the matrix
    my_matrix_inv                           ## return the inverse of the matrix
}
