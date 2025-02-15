#' Create a special "matrix" object that can cache its inverse
#'
#' This function creates a list containing functions to:
#' - Set the value of the matrix
#' - Get the value of the matrix
#' - Set the value of the inverse
#' - Get the value of the inverse
#'
#' @param x A matrix (default is an empty matrix)
#' @return A list of functions to interact with the matrix and its inverse
#' @examples
#' m <- makeCacheMatrix(matrix(1:4, 2, 2))
#' m$get()
#' cacheSolve(m)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset cached inverse if the matrix changes
    }
    get <- function() x  # Retrieve the matrix
    setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
    getInverse <- function() inv  # Retrieve the cached inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#' Compute and cache the inverse of the "matrix" object
#'
#' This function computes the inverse of the special "matrix" object returned by
#' makeCacheMatrix. If the inverse has already been calculated and cached, it
#' retrieves the cached inverse; otherwise, it computes the inverse, caches it,
#' and then returns the inverse.
#'
#' @param x A special "matrix" object created by makeCacheMatrix
#' @param ... Additional arguments to be passed to the solve function
#' @return The inverse of the matrix
#' @examples
#' m <- makeCacheMatrix(matrix(1:4, 2, 2))
#' cacheSolve(m)
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)  # Return cached inverse if it exists
    }
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse
}
