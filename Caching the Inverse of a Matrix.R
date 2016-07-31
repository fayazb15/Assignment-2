## Week3-Assignment 2 
## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## function :makeCacheMatrix
## Argument : x 
## This function creates a special matrix object that can cache its inverse.
 
makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 
}

## function :CacheSolve
## This function computes the inverse of the special matrix returned by ## makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix ## has not changed), then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
 
