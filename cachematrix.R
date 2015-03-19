## The functions 'makeCacheMatrix' and 'cacheSolve' 
## provide a useful interface for matrix calculations
## when computational time can be saved by caching 
## computed inverses.

## The function makeCacheMatrix(x) creates a composite 
## object containing matrix 'x' and a place to cache  
## its inverse. Variables set and get are functions  
## for storing and retrieving the matrix. Variables 
## setinverse and getinverse are similarly used to
## store or retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
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


## The function cacheSolve(x, ...) is used to compute
## the inverse of a matrix contained in a composite
## cacheMatrix object 'x' as returned by the function
## makeCacheMatrix. If cached inverse exists, this is 
## returned, otherwise inverse is computed with the
## function 'solve' with additional parameters '...' 
## and the computed inverse is subsequently cached.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    #If a cached inverse exists, return that
    if(!is.null(inv)) {
        # Comment next line if you do not want a 
        # message when returning a cached inverse.
        message("getting cached inverse")
        return(inv)
    }
    
    #If not, compute inverse, then cache and return it
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
