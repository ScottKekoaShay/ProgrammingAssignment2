## These functions will cache the inverse of a matrix in order to save
## compuation time for large matrices.
## Usage: call makeCacheMatrix() with a matrix to be inverted as an argument
##        call cacheSolve() with the returned matrix from makeCacheMatrix()
##        The initial call will not use the cache but will return the inverse
##        Subsequent calls will return the cached inverse

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
      x <<- y
      mat <-- NULL
    }
    get <- function() x
    setmatrix <- function(solve) mat <<- solve
    getmatrix <- function() mat
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat <- x$getmatrix()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setmatrix(mat)
    mat
}