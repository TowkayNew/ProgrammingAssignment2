## This function creates a special "matrix" object 
## that can cache its inverse.

## ----------------------------
## Sample Test Code & Results
## ----------------------------
## > test1 <- c(1,2,3,4)
## > dim(test1) <- c(2,2)
## > test2 <- makeCacheMatrix(test1)
## > test2$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test2$getInverse()
## NULL
## > cacheSolve(test2)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(test2)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > test2$getInverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}