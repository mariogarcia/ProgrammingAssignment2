## Caching matrix value
## ====================
##
## non_cached_matrix  <- matrix(c(1, 1, 0.25, 1, 1, 0.33, 0.25, 1, 1), ncol = 3)
## cached_matrix      <- makeCacheMatrix()
## cached_matrix$set(non_cached_matrix)
## same_cached_matrix <- cached_matrix$get()
##
## identical(cached_matrix, same_cached_matrix)
##
## Caching inverse of a square matrix
## ==================================
##
## cached_matrix$setsolve(solve(cached_matrix$get()))
## inverse <- cached_matrix$getsolve()
##
##

## This function caches a the matrix passed as an argument
## and the inverse of that matrix when calculated
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL

    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    get        <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function aches the inverse of the matrix 'x'
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()

    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    data <- x$get()
    s    <- solve(data, ...)

    x$setinverse(s)
    s
}
