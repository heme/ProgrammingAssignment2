## These functions work together to solve and cache the inverse of a matrix

## Creates matrix object that can cache its inverse
 # args:
 #   x: matrix object you would like to decorate.
 # returns:
 #   matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Inverts a matrix object. Memoizes the work by retrieving from, and saving to, a cache 
 # args:
 #   x: existing matrix object created by makeCacheMatrix
 # returns:
 #   the inverse of the matrix object
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
}