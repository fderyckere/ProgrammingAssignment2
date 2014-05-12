## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a number of functions for a matrix
# The functions:
# - initiate the matrix
# - get the matrix
# - set its inverse in cache
# - and obtain the inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(invertedMatrix) m <<- invertedMatrix
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## Write a short comment describing this function
# The first time this function is called, it calculates the inverse of the matrix and puts the result into cache
# The following times this function is called, it returns the inverted matrix from cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

## example
## first calculate without cache
# b <- matrix(as.integer(20*rnorm(1000000)), ncol=1000)
# solve(b)
## Check through b %*% solve(b)
#
## Now make use of the cache:
# b1 <- makeCacheMatrix(b)
# system.time(cacheSolve(b1))   # will initiate the cache
# system.time(cacheSolve(b1))   # will read the cache
