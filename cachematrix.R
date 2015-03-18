## Code to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## It contains a list of funcions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        ## the <<- operator is used to assign a value to an object
        ## in an environment different from the current environment
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## If inverse is already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        ## it retrieves the inverse from the cache
        return(m)
    }
    ## Otherwise it calculates the value of the inverse via the solve function
    data <- x$get()
    m <- solve(data, ...)
    ## Set the value of the inverse in the cache via the setinverse function 
    x$setinverse(m)
    m
}