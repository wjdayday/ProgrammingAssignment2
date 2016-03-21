
## this function creates a list consisting of four functions: to set the matrix, to get the matrix, 
## to set the inverse of the matrix and to get the inverse of the matrix.


makeMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function checks whether the inverse of the matrix has already been 
calculated, if yes, then the inverse from the cache will be returned. Otherwise
the inverse of the matrix will be calculated.


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}