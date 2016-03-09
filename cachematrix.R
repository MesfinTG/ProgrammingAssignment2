## R Programming Assignment 2: Cahching the Inverse of a Matrix
## The makeCaheMatrix function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function cacheSolve below computes the inverse of the special "matrix" object
##  returned by the makeCacheMatrix function above. If the matrix under consideration 
##  has not changed and if the inverse has already been computed, then this CacheSolve
## function retrieves the inverse from the cache and skips the computation step. Otherwise 
## this function calculates the inverse of the special matrix and sets the value in the
## cache via the setInverse function.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting chached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        }
