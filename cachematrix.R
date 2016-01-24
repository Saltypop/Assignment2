## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation.
## It will be benefit to caching the inverse of a matrix
## rather than compute it repearedly.
## Here is a pair of function in which we can catch the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversion <- function(inverse) inv <<- inverse
    getinversion <- function() inv
    list(set = set, get = get, setinversion = setinversion,
         getinversion = getinversion)
}


## The second function is to compute the inverse of the matrix returned by makeCacheMtrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getinversion()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinversion(inverse)
    inv
        ## Return a matrix that is the inverse of 'x'
}

