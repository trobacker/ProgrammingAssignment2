## Author: trobacker (coursera account name)

## The following function, makeCacheMatrix,
## creates a special "matrix"object that can 
## cache its inverse.I modified the code 
## given as an example for calculating a mean.


makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    get <- function() x
    setInv <- function(inv) mInverse <<- inv
    getInv <- function() mInverse
    list(set = set, get = get, setInv = setInv,getInv = getInv)
}

## The following function, cacheSolve, computes the inverse of the special
## "matrix" returned by the makeCacheMatrix function.
## I modified the code given as an example for 
## calculating a mean.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mInverse <- x$getInv()
    if(!is.null(mInverse)) {
        message("Getting cached data..")
        return(mInverse)
    }
    data <- x$get()
    mInverse <- solve(data, ...)
    x$setInv(mInverse)
    mInverse
}