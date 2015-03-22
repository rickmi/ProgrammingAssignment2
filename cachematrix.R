## These functions are used to calculate the inverse of a matrix.
## If the matrix inverse has been calculated before, the inverse will be returned from cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                            ## Set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## Get the value of the matrix
        setinverse <- function(inverse) m <<- inverse   ## Set the inverse of the matrix
        getinverse <- function() m                      ## Get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#