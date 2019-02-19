## Implement caching of an inverted matrix to avoid 
## recalculating it every time it is needed

## Create an object that would contain a matrix x
## and its inverse m, once it has been cached

makeCacheMatrix <- function(x = matrix()) {
    # Clean the variable to hold the inverse of a matris
    m <<- NULL
    
    # Set the matrix and clean the variable to hold its inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Return the stored matrix
    get <- function() x
    
    # Calculate the inverse of the matrix and store it
    setinverse <- function(solve) m <<- solve(x)
    
    #Return the stored inverse of the matrix 
    getinverse <- function() m
    
    # Make all functions available as a list 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the cached inverse of the matrix or, when null,
## calculate and store the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # The inverse is not available, thus calculate it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}