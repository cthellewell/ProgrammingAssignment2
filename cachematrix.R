## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the inverse
    i <- NULL
    
    ## define a function to set the value of the "matrix"
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## define a function to get the matrix
    get <- function() x
    
    ## define a function to store the inverse matrix in the cache
    setinverse <- function(inv) i <<- inv
    
    ## define a function to get the value of the inverse stored
    ## in the cache
    getinverse <- function() i
    
    ## return the list of functions defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## get the inverse of our "special" matrix
    i <- x$getinverse()
    
    ## if the inverse has already been caculated and
    ## is in the cache, return the cached value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## the inverse hasn't been calculated yet
    ## grab matrix from our special "matrix"
    data <- x$get()

    ## calculate the inverse of the matrix
    ## Assume that the matrix supplied is always invertible.
    i <- solve(data, ...)
    
    ## save the inverse in the cache
    x$setinverse(i)
    
    ## return the inverse
    i
    
}
