## The 2 functions are used to produce a matrix and then the inverse of 
## the matrix stored in cache memory for quick retrieval of results

## the first function helps create a matrix using the matrix() function

makeCacheMatrix <- function(x = matrix()) {
    ## set any previously set m inverse object to 'NULL'
    m <- NULL
    ## defining set function to assign the new matrix in the place of existing object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## defining get() function which returns x
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ## brings the list of functions to be used in different scenarios of cached object x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function below inverts the matrix produced by the previous function and then retrieves the cached inverse 
## of the matrix

cacheSolve <- function(x, ...) {
    ## Retrieves cached matrix of 'x'
    m <- x$getinverse()
    ## if m (cached inverse) is not 'Null' then retrieve it after showing a message that 
    ## notifies of the cache data retrieval
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if m does not exist then retrieve x
    data <- x$get()
    ## set m as inverse of matrix x and cache it
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
        
