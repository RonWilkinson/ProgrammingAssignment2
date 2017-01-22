#########################################################################################################################
##
##  cachematrix.R code written by Ronald Wilkinson for Coursera's R Programming course, Programming Assignment 2
##
##      Originally written when I took the course standalone in 2015.  Redoing course as part of certificate program
##      in 2017.  The only change in 2017 is a slight revision of the comments.
##
#########################################################################################################################
##
## This code defines functions used to store a matrix and cache its inverse in a custom object class one could call
## a "cacheMatrix".
##
## makeCacheMatrix creates an object that holds:
##
##      an internal storage location for a matrix
##      an internal storage location for the inverse of the matrix
##      get and set functions for the matrix storage location
##      getinverse and setinverse functions for the inverse matrix storage location
##
##      It returns a list containing the get and set functions for both matrix and inverse
##      so their values can be accessed from outside the object.
##
##
## cacheSolve returns the inverse of a matrix that is stored in a "cacheMatrix" object.
##
##      If the inverse is already cached, cacheSolve returns the cached copy.
##
##      Otherwise, cacheSolve:
##
##            1. Calculates the inverse using the solve() function, passing it any optional arguments.
##            2. Caches the calculated inverse in the "CacheMatrix" object for later retrieval.
##            3. Returns the calculated inverse for immediate use.
##
##      Note that if just returning the cached inverse, the optional arguments are ignored, so if you want to recalculate
##      the inverse of a cacheMatrix x using different optional arguments, it is necessary to first clear the cache using
##      x$setinverse(NULL) before calling cacheSolve(x).
##
##########################################################################################################################


makeCacheMatrix <- function(x = matrix()) {

        ## This function constructs an object
        ## that contains storage for a matrix and its inverse
        ## and defines object functions to get and set the storage values.


        ## The function argument already initializes storage for the candidate matrix x,
        ## so just need to explicitly initialize storage for the inverse of x.

        xinverse <- NULL


        ## Create an object function to store the value of the x matrix.
        ## Since x is newly set, its inverse has not yet been calculated,
        ## so clear the inverse cache of any previous value.

        set <- function(y) {
                ## Use <<- to store variables at object level, not subfunction level
                x <<- y
                xinverse <<- NULL
        }


        ## Create an object function to retrieve the value of the x matrix.

        get <- function() x


        ## Create an object function to store the value of the x inverse matrix.

        setinverse <- function(xi) xinverse <<- xi  ## Use <<- to store variable at object level, not subfunction level


        ## Create an object function to retrieve the value of the x inverse matrix.

        getinverse <- function() xinverse


        ## Return a list of the storage and retrieval object functions for external access.

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(x, ...) {

        ## This function returns the inverse of a cacheMatrix's matrix,
        ## calculating the inverse only if it is not already cached.

        ## Return cached inverse if it exists

        xinverse <- x$getinverse()
        if(!is.null(xinverse)) {
                message("getting cached data")
                return(xinverse)
        }

        ## Otherwise:

        xmatrix <- x$get()              ## Retrieve the x matrix
        xinverse <- solve(xmatrix, ...) ## Calculate its inverse, passing optional parameters
        x$setinverse(xinverse)          ## Cache the calculated inverse
        xinverse                        ## Return the calculated inverse for immediate use.

}
