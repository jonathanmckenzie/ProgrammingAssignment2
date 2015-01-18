## The following functions 

## makeCacheMatrix generates a "cacheable" matrix.  This is used as input to the
## function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    # Function to set the value of the matrix.
    set <- function(mat_set) {
        x <<- mat_set
        # On creation of matrix, inverse is not known
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv_set) inv <<- inv_set
    getInverse <- function() inv
    
    # Return the cachematrix as a list of getters and setters
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve determines the inverse of a "cacheable" matrix.  If the inverse is
## stored, then this is returned without calculation required.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached_inv <- x$getInverse()
    if (!is.null(cached_inv)) {
        message("Inverse is cached, returning cached value")
        return(cached_inv)
    }
    # We execute the following when inverse needs to be calculated
    mat <- x$get()
    inv_mat <- solve(mat)
    x$setInverse(inv_mat)

    # We have now cached the calculated inverse, return this to user
    inv_mat
}
