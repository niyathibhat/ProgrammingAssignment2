## The below functions make the process of inverting a matrix more efficient
## by taking advantage of lexical scoping rules and the ability to cache
## values in the parent environments.

## makeCacheMatrix converts a regular matrix into a special matrix object,
## which can cache its own and inverse values in the enclosing environment,
## so that retrieving them is quicker and more efficient. It is assumed that
## the input matrix can be inverted.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## cacheSolve finds the inverse of special matrix objects which are
## constructed by the makeCacheMatrix function. If the inverse is
## already in the cache, it is retrieved. If the inverse hasn't been
## calculated or matrix value has been changed, the inverse is calculated
## and returned, as well as updated in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Retrieving inverse value from the cache")
                return(inv)
        }
        
        matrix_data <- x$get()
        inv <- solve(matrix_data)
        x$setinverse(inv)
        inv
}
