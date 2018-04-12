## functions to cache and retrieve the inverse of a matrix
## to avoid computing inverses repeatedly.

## makeCacheMatrix creates a wrapper object
## that stores both the matrix and the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(val) {
        x <<- val
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(val) i <<- val
    getInverse <- function() i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve first checks if cached inverse exists
## if not it computes, caches and return the inverse.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached inverse of matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

