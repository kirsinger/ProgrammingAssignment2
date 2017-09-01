# Cache Matrix
#
# Author: Kai Hirsinger
# Since: 1st September 2017
#
# Creates a variation of the base matrix object
# where the inverse can be cached, set and accessed
# via a set of methods.


# Makes a new cache matrix from an existing matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


# Calculates the inverse of a cache matrix.
# If the inverse has already been calculated for
# an instance of a cache matrix, a cached value will
# be returned.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if  (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
