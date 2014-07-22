## Script responsible of creating a special matrix-like object which caches
## the result of its inverse

## Creates a matrix-like object
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## Calculates the inverse of a matrix-like object 'x'.
## Uses a cached-value if the inverse was already calculated.
cacheSolve <- function(x, ...) {    
    inv <- x$getinverse()
    
    if (is.null(inv)) {
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)    
    }
    
    return(inv)
}
