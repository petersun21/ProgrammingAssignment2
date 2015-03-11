## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    xsolve = NULL
    set <- function(y) {
        x    <<- y
        xsolve <<- NULL
    }
    get <- function() x
    setsolve <- function(inv) {
        xsolve <<- inv
    }
    getsolve <- function() xsolve
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xsolve <- x$getsolve()
    if(!is.null(xsolve)) {
        message("getting cached data")
        return(xsolve)
    }
    data <- x$get()
    xsolve <- solve(data, ...)
    x$setsolve(xsolve)
    xsolve
}
