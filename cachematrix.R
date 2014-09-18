## The inversion of a matrix is a costly computation,
# so these 2 functions are designed to cache the inversion 
# of a matrix and return the cache if the matrix has not 
# changed and the inversion has already been done.
# 
# Created by Warren McGee for Coursera Programming Assignment 2 due 2014-09-21
##

## makeCacheMatrix:
# This function creates a special matrix that can store the cache of its inversion
# NOTE: this function assumes that the matrix supplied is invertible!
# Args: x = invertible matrix
# Returns: cacheMatrix object with functions to store matrix and cache and change them
##
makeCacheMatrix <- function(x = matrix()) {
	
	inv <- NULL  # Initialize the inversion
	
	# Replace old with new matrix; then remove the cache of the old one
	set <- function(y) { 
		x <<- y 
		inv <<- NULL
	}
	
	# Function to return the matrix
	get <- function() x  
	# Function to store a cache of the inverse of 'x'
	setInv <- function(inversion) inv <<- inversion  
	# Function to return the cached inverse of 'x' (or NULL if it doesn't exist)
	getInv <- function() inv
	
	# Put the above functions into a list to define an object to return
	object <- list(set = set, get = get, setInv = setInv, getInv = getInv)
	# set the class of above list to "cacheMatrix"
	class(object) <- "cacheMatrix"
	# Return the cacheMatrix object with matrix and ability to store a cached inversion
	object
}


## cacheSolve:
# This function calculates the inversion of cacheMatrix or returns the cache if it exists
# Args: x = cacheMatrix; ... = other arguments
# Returns: the inverse of x
##
cacheSolve <- function(x, ...) {
	
	# If x is not a cacheMatrix object, then the rest of the function won't work
	if(!class(x)=="cacheMatrix") { 
		stop("cacheSolve requires x to be a cacheMatrix object.") 
	}
	
	inv <- x$getInv()  # Get cache
	
	# If cache exists, then return that instead of calculating it
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# If cache does not exist, then calculate it instead
	matrix <- x$get()  # get the Matrix
	# calculate the inversion using the solve() function
	inv <- solve(matrix) 
	# set the cache so that it can be used for the future
	x$setInv(inv)
	# Return the matrix that is the inverse of 'x'
	inv 
}
