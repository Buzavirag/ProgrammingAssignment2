## Aim of the two functions is to compute the inverse of a matrix while caching the value of the
## inverse, so that it does not need to be calculated again, instead it can be looked up in the 
## cache later as well.

## The first function serves to create a special vector that stores the inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
            x <<- y
            matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix <<- solve
        getinverse <- function() matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function below serves the purpose to return a matrix that is the inverse of "x". 
## If the inverse is already stored in the cache, it will use the cached data, otherwise it will
## calculate the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getinverse()
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setinverse(matrix)
    matrix
}
