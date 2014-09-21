## Functions to speed us the releated use of a matrix and it's inverse by
## caching the matrix and it's inverse.

## Function crates a data structure to store a matrix object and it's inverse;
## and the method functions to access the data.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL          # cache of matrix inverse
    set <- function(y) {
        x <<- y        # Store the matrix
        i <<- NULL     # No inverse yet
    }
    get <- function() x    # return the matrix
    setinverse <- function(inverse) i <<- inverse  # Store the inverse
    getinverse <- function() i  # return the cached inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)  # Return a list of accessor functions
}


## Returns the inverse of a given CacheMatrix object.
## Uses & stores cached inverse data to speed up the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()  # Get cached inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)        # return the cached inverse and stop
    }
    data <- x$get()       # Get the matrix data
    i <- solve(data, ...) # Calculate the inverse of the matrix
    x$setinverse(i)       # store the inverse in the CacheMatrix
    i
}
