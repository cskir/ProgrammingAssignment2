## These two functions support the matrix inverse computation 
##   by providing a cached solution for this time-comsuming operation.
## Constraint: it works only for invertible matrix

## makeCacheMatrix 
## It is a wrapper around a matrix extended by a cached inverse value, 
## what is calculating for the first request, and then cached 
##   arguments: an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialization
    invValue <- NULL 
    
    ## setter for the matrix 
    set <- function(y) {
        ## superassigns
        ## re-init the inverse value
        x <<- y
        invValue <<- NULL
    }
    ## getter for the matrix
    get <- function() x
    
    ## setter for the inverse 
    setsolve <- function(solve) invValue <<- solve
    
    ## getter for the inverse
    getsolve <- function() invValue
    
    ## return the augmented matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve
## Calculates and returns the inverse of x
## based on the cahcing of the data structure of makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## try get the inverse
    invValue <- x$getsolve()
    
    ## if already calculated, take and return
    if(!is.null(invValue)) {
        message("getting cached data")
        return(invValue)
    }
    
    ## else get the matrix of the cacheMatrix 
    data <- x$get()
    ## calculate the inverse
    invValue <- solve(data, ...)
    ## store the inverse in the cacheMatrix
    x$setsolve(invValue)
    ## and return with the inverse
    invValue
}
