## Luis Acevedo-Arreguin, Monterey, CA, Jul 23, 2015
## Put comments here that give an overall description of what your
## functions do
## This script provides the user with two functions to treat a matrix as 
## an object by creating set and get methods and to compute its inverse.

## Write a short comment describing this function
## This function creates an object from a matrix by defining methods
## that set and get the matrix, as well as methods to get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mat) {
		x <<- mat
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of a matriz provided that the
## computation has not been already performed by the user previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("... getting cached data ...")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
