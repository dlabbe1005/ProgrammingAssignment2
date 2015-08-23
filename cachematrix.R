## This file contains the functions to create our special matrix and solve its inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## creates the object that is going to be cached
	m <- NULL

	## creates the set function
	set_m <- function(y) {
		x <<- y
		m <<- NULL
	}

	## creates the get function
	get_m <- function() {
		x
	}

	## creates the set cached matrix function
	setcachematrix <- function(mat) {
		m <<- mat
	}
	
	## creates the get cached matrix function
	getcachedmatrix <- function() {
		m
	}

	## creates a list with all the objects (functions) related to the creation of the matrix
	list(set_m = set_m, get_m = get_m, setcachematrix = setcachematrix, getcachedmatrix = getcachedmatrix)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	## assign the cached matrix to a local scope variable using the function created
	m <- x$getcachedmatrix()
	
	## if there is a cached matrix already and it didn't changed, return the invert of the cached matrix
	if(!is.null(m)){
		m2 <- solve(m)
		if( all.equal(m2, x$get_m())) {
			return (m2)
		}
	}
		
	## if there isn't anythig cached, get, invert and set to the cache
	data <- x$get_m()
	m <- solve(data, ...)
	x$setcachematrix(m)
	m
}
