## These functions are supposed to be used in serial, one use to create a matrix contained in a caching mechanism,
## while the other uses this function to return either the value of the cached inverse, or to calculate it anew

## This function will construct a cache for the input matrix and use that to store the value of the matrix's inverse value
## It will return a list with four named functions:
##   get: which will expose the matrix the contraption contains
##   set(x): which will enable us to override the value of this matrix (and reset the cache)
##   getInverse(): which will return to us the cached value of the matrix's inverse (or NULL if no inverse has been calculated for this matrix)
##   setInverse(i): which will let us override the cached value of the inverse for the given matrix
makeCacheMatrix <- function(x = matrix()) {
	## we first set the inverse to NULL to signify it has not been calculated yet
	inverse <- NULL
	## this will reset the value of the matrix, as well as reset the cache
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	## this will return the current value of the matrix
	get <- function () x
	## this will override the value of the inverse in the cache
	setInverse <- function (i) inverse <<- i
	## this will return the cached inverse for the matrix
	getInverse <- function () inverse
	## finally, we return all the created functions that will let us work with the cache
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## This function will solve the value of the matrix's inverse from the given cache container. The input to this
## function must be the output of the `makeCacheMatrix` function.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	## we first check the cache
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		## if the value in the cache is not null, we return it
		return(inverse)
	}
	## otherwise, we have to perform the calculations right here
	## we first need to get the data
	data <- x$get()
	## then calculate the inverse
	inverse <- solve(data, ...)
	## and put it back in the cache for later use
	x$setInverse(inverse)
	## and finally return it to the user
	inverse
}


## Why this isn't a good solution?
## Because it exposes the internals of how we need to work with this mechanism to the outside. People need to know about our
## caching mechanism for it to work. They need to learn to work with two functions just to be able to calculate the inverse.
## They also can override the value of the inverse whenever they want, making this value unsafe to use.

## There are better solutions:

## Solution 1: Eager calculation

## In this approach we calculate the value of the inverse as we go, always ensuring that it is available
eagerInverse <- function (x = matrix()) {
	inverse <- solve(x)
	set <- function (y) {
		x <<- y
		inverse <<- solve(y)
	}
	get <- function () x
	getInverse <- function () inverse
	list(
		get = get,
		set = set,
		getInverse = getInverse
	)
}

## Solution 2: Lazy calculation

## In this approach, we will calculate the inverse only when it is needed, and cache it after the first call.
## We invalidate the cache as the value of the matrix is changed using the `set` function
lazyCalculation <- function (x = matrix()) {
	inverse <- NULL
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function () x
	getInverse <- function () {
		if (is.null(inverse)) {
			inverse <<- solve(x)
		}
		inverse
	}
	list(
		get = get,
		set = set,
		getInverse = getInverse
	)
}

## Solution 3: Lazy function

## In this approach we return a function that will calculate the value of the inverse or return it from the cache
lazyInverse <- function (x = matrix()) {
	inverse <- NULL
	lazy <- function () {
		if (is.null(inverse)) {
			inverse <- solve(x)
		}
		inverse
	}
	lazy
}
