## Programm Assignment 2 for "R Programming" (rprog-006)
##
## The functions cache tine inverse of a square, invertable matric
## so that it does not have to be re-calculated for every use (unless
## the matrix, and therefore it's inverse, have changed).
##
## Jeff Shilling

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object
    ## that can cache its inverse.

    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## If the inverse has already been calculated 
        ## (and the matrix has not changed), then
        ## `cacheSolve` retrieves the inverse from the cache.
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

