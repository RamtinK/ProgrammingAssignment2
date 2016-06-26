## Problem: matrix inversion is computationally expensive and recomputation 
## the inverse for same matrix is inefficient
## Solution : to compute matrix inversion for a matrix and cache it for further
## usage

## "makeCacheMatrix" gets a square matrix as an input and returns a list of 
## set and get functions to cache the computed inverse for the given matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve gets the provide list of functions by makeCacheMatrix and return 
## the inverse of given matrix if it's already been computed without 
## recomputation or compute the inverse and then return it

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("get cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
