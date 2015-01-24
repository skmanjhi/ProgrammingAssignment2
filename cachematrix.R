## Coursera: R-Programming: Assignment 2 : Sachin Kumar Manjhi
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL
    
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Set the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getinv <- function() inv
    
    # Return the matrix with these functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
        
    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
            message("Returning cached Inverse")
            return(inv)
    }
        
    # The inverse is not yet calculated, so calculate it using Solve()    
    inv <- solve(x$get())
        
    # Cache the inverse
    x$setinv(inv)
        
    # Return the inverse
    inv
}
