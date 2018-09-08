## Overall aim of the following fuctions is to cache the inverse of a matrix


##This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }
    get <- function() x
    setinve <- function(inverse) inve <<- inverse
    getinve <- function() inve
    list(set = set, get = get,
         setinve = setinve,
         getinve = getinve)
}


## This function computes the inverse of the special "matrix" returned by the
#function above

cacheSolve <- function(x, ...) {
    inve <- x$getinve()
    if(!is.null(inve)) {
        message("getting cached data")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data, ...)
    x$setinve(inve)
    inve
}

