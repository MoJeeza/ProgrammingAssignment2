## Put comments here that give an overall description of what your
## functions do
##
## These functions demonstrate the implementation of lexical scoping 
# with in the R programming language - as opposed to dynamic scoping
# there are basicially two functions in this file: makeCacheMatrix and 
# cacheSolve

# makeCacheMatrix is a function that is actually a list of 4 functions
# namely :get_my_matrix, set_my_matrix, set_the_inv and get_the_inv.
# There is no matrix conversions done in any of these 4 functions.
# These functions are used only for setting, caching and retrieving the matrix
# or the inverse of the matrix. The use of <<- assignment is exploited here
# to define the variables in the functions at the parent level (the environment
# in which these functions are defined)

# cacheSolve on the other hand calls the four inner functions of the
# makeCacheMatrix function to set or retrieve the matrix or inverse of the
# matrix. The inverse of the matrix is computed in this function.

##
## Author: Mohsin Jessa
## Coursera Course: R-Programming (Prog-004)

# Write a short comment describing this function
# 
# this function makeCacheMatrix is being defined in the global environment
# It will store/cache the matrix as well as it's inverse. 
# The storing of the matrix is done via the function call to
# set_my_matrix and the inverse is stored/cached using the set_the_inv function.


# by default a matrix is expected for the formal argument my_matrix, 
# if none is passed then an empty matrix is generated

makeCacheMatrix <- function(my_matrix = matrix()) {      
    

    store_inv_of_matrix <- NULL     # initially we don't have inverse of the matrix
    
    f_set_my_matrix <- function(get_new_matrix) {
        my_matrix <<- get_new_matrix    # make these variables available to the
                                        # parent environment by using <<- assignment
        store_inv_of_matrix <<- NULL
    }
    
    f_get_my_matrix <- function() {
        my_matrix                   # my_matrix technically is a free variable, 
                                    # it's value is defined in the makeCacheMatrix
                                    # function, which is the parent environment in
                                    # which this function was defined
                                    # 
    }
    
    f_set_the_inv <- function(get_inv_of_matrix) {
        store_inv_of_matrix <<- get_inv_of_matrix   # store the inv in the parent
                                                    # environment
    }
    
    f_get_the_inv <- function() {
        store_inv_of_matrix         # store_inv_of_matrix technically is a free
                                    # variable, 
                                    # it's value is defined in the makeCacheMatrix
                                    # function, which is the parent environment in
                                    # which this function was defined
                                    # 
    }
    
    list(set_my_matrix = f_set_my_matrix, 
         get_my_matrix = f_get_my_matrix,
         set_the_inv = f_set_the_inv,
         get_the_inv = f_get_the_inv)
}


## Write a short comment describing this function
##
## checks if the inverse of the matrix exists by calling the $get_the_inv function
## if so, then return the inverse  - ie cached value
## if not, compute the inverse and store it using $set_the_inv function
##
cacheSolve <- function(x, ...) {        # here x is the list of 4 functions

    get_matrix_inv <- x$get_the_inv()   # get an inverse of the matrix

    if(!is.null(get_matrix_inv)) {     # check if the inverse of the matrix
                                       # has been computed before. if so retrieve it        
                                       # if get_matrix_inv is not null then an
                                       # inv of the matrix exists, 
                                       # so lets get that cached value
        
        message("getting cached data")  # tell that we found the cached value
        return(get_matrix_inv)          # return the cached value
    }                                   
    
    # inverse of the matrix was not found so
    my_matrix <- x$get_my_matrix()          # get the current matrix
    my_matrix_inv <- solve(my_matrix, ...)  # compute the inverse of the matrix 
    x$set_the_inv(my_matrix_inv)            # cache the inverse of the matrix
    my_matrix_inv                           # return the inverse of the matrix
}
