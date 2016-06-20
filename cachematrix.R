## These functions cache the inverse of a matrix.

## This function sets the value of a matrix, gets the value of the matrix
## sets the value of the inverse and gets the value of the inverse.
## It then stores these in a list.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}