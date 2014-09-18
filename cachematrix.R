## The inversion of a matrix is a costly computation,
## so these functions are designed to cache the inversion of a matrix
## and return the cache if the matrix has not changed and the inversion ahs already been done
## 
## Created by Warren McGee for Coursera Programming Assignment 2 due 2014-09-21

## This function creates a special matrix that can store the cache of its inversion
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL # Initialize the inversion
	set <- function(y) { # Replace old with new matrix; then remove the cache of the old one
		x <<- y 
		inv <<- NULL
	}
	get <- function() x # Return the matrix
	setInv <- function(inversion) inv <<- inversion # Store the inversion cache in "inv"
	getInv <- function() inv # Return the cached inversion or NULL if it doesn't exist
	object <- list(set = set, get = get, setInv = setInv, getInv = getInv) # List of functions
	class(object) <- "cacheMatrix" # set the class of the list of functions to "cacheMatrix"
	object # Return a cacheMatrix object with matrix and ability to store a cached inversion
}


## This function calculates the inversion of a special matrix or returns the cache if it exists
cacheSolve <- function(x, ...) {
	# If x is not a cacheMatrix object, then the rest of the function won't work
	if(!class(x)=="cacheMatrix") { 
		stop("cacheSolve requires x to be a cacheMatrix object.") 
	}
	
	inv <- x$getInv() # Get cache
	# If cache exists, then return that instead of calculating it
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	# If cache does not exist, then calculate it instead
	matrix <- x$get() # get the Matrix
	inv <- solve(matrix) # calculate the inversion using the solve() function
	x$setInv(inv) # set the cache so that it can be used for the future
	inv ## Return the matrix that is the inverse of 'x'
}
