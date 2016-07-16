## As what the programming assignment requires, these functions inverses a
## matrix, and if the inverse was already calculated; the cached value will be
## outputted.

## This function in a sense would be getters and setters.
## It would set ang get the matrix and inverse, and its output would be used
## for the cacheSolve function.

makeCacheMatrix <- function(matrix_data = matrix) {

    matrix_inverse <- NULL

    set <- function(a) {
        matrix_data <<- a
        matrix_inverse <<- NULL
    }

    get <- function() matrix_data
    setInverse <- function(inverse) matrix_inverse <<- inverse
    getInverse <- function() matrix_inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function evaluates if the inverse was already in the cache, if its in
## the cache, the value of the cache will be returned.

cacheSolve <- function(matrix_data, ...) {

    ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- matrix_data$getInverse()

    if(!is.null(matrix_inverse)) {
        message("Getting cached data:")
        return(matrix_inverse)
    }

    data_get <- matrix_data$get()
    matrix_inverse <- solve(data_get)
    matrix_data$setInverse(matrix_inverse)
    matrix_inverse
}