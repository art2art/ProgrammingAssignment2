## These R functions give the ability to cache the inverse of a matrix.
## Instead of recomputing the inverse of a matrix it can be quick got
## out of the cache.

## @makeCacheMatrix - creates an `object' with setters and getters of an original
## matrix and an inverse matrix, which is in fact a list containing functions to
## set the value of the original matrix, set the value of the inverse matrix,
## get the value of the original matrix, get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(mat) {
        if (!identical(x, mat)) {
            x <<- mat
            inverse <<- NULL
        }
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(inv) inverse <<- inv
    list(get = get, set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}

## @cacheSolve - the function calculates the inverse of special matrix
## created by makeCacheMatrix. Foremost it checks the cached value of
## the inverse matrix. The function skips the computations if the value 
## of the inverse matrix is already calculated. Otherwise, it calculates
## the inverse of the matrix and sets the invesed matrix in the via
## the setInverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        inv
    } else {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
        inv
    }
}
