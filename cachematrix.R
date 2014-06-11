## There are two functions here. makeCacheMatrix is to create a matrix 
## that can be cached and cacheSolve computes the inverse of a matrix
## if the solution has yet to be cached. Else, return cached matrix

## makeCacheMatrix Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve: Computes the inverse of a cacheMatrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}