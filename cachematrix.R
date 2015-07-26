## These functions can be used to store the inverse of a matrix in a cache 
## which can be accessed at any time without having to recompute it.

## This function returns a list object with 4 members each pointing to a  
## different function. The initial function call with a matrix stores a matrix 
## object using the set function. This can be retrieved using the get function
## getinv can be used to obtain the inverse of the matrix object stored from the
## cache if it has already been computed. setinv can be used to store the inverse
## in the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function first checks to see if the inverse for the special matrix object
## has been calculated, if so, it retrieves it from the cache. Otherwise it 
## calculates the inverse and stores it in the cache using the setinv function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
