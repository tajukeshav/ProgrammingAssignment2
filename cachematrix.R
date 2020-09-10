## Overall, these functions cache the calculation of the inverse of a specified matrix, allow for quick access to the value for repeat computation.

## makeCacheMatrix represents a list of functions that allow for the matrix and its inverse to be cached 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the matrix from makeCacheMatrix and stores it in cache for easy access later. If solution exists, it pulls from cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
