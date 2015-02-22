## These functions are used together to cache the inverse of a matrix

## The first function can be given an invertible matrix, and returns a list containing four functions. "setinverse" stores
## the calculated inverse of the matrix in a cache, "getinverse" retrieves the cached value, the "get" function
## returns the matrix itself, and "set" allows the matrix to be changed while resetting the cached inverse. 

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is used on the output of makeCacheMatrix and calculates the inverse of the matrix inside.

cachesolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
