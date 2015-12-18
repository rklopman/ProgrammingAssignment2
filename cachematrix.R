##
## cachematrix.R
##

## The functions makeCacheMatrix and cacheSolve allow to use the cached
## inverse of a square matrix rather than having to calculate this all the time

## First apply makeCacheMatrix to create an object for storing a matrix and its
## inverse
## Then apply cacheSolve to get the inverse. If the inverse is calculated
## before for the matrix, the cached result will be used.
## Otherwise, the inverse is calculated and cached.
## Caution: this only works for a square matrix!
## For changing a matrix, execute <object>$set(<newmatrix>)

## Example
## >mat <- matrix (c(1.1, 2, 3, 4.2, 5, 6, 7.4, 8, 9), 3, 3)
## >matobject <- makeCacheMatrix(mat) #create object
## >cacheSolve(matobject) #get inverse
## >cacheSolve(matobject) #get inverse again; cache used
## >newmat <- matrix (c(1.4, 2, 3, 4.2, 5, 6, 7.1, 8, 9), 3, 3)
## >matobject$set(newmat) #change matrix; cache cleared
## >cacheSolve(matobject) #get inverse
## >cacheSolve(matobject) #get inverse again; cache used


##
## Function makeCacheMatrix
##

## This function creates an object consisting of a list of functions for
## storing a matrix and its inverse

## Result: a list of 4 functions:
## - get: get current matrix
## - set: change matrix; cache of inverse will be cleared, also if the same
##   matrix is entered again
## - getinverse: get current inverse; not supposed to be directly called
##   by users
## - setinverse: cache calculated inverse; does not do the actual calculation
##   of the inverse but only stores a calculated result; not supposed to be
##   directly called by users

## Parameters:
## - x: matrix initially to used; may be empty

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverse <<- inverse
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


##
## Function cacheSolve
##

## This function returns the inverse of the matrix stored by an object made
## with the function makeCacheMatrix. If the inverse was not already calculated
## before for this matrix, a the inverse will be calculated. It will also be
## cached for future reuse. If the inverse was already calculated
## before for thematrix, the cache will be used.

## Result: inverse of the matrix stored by an object made with the function
## makeCacheMatrix

## Parameters:
## - x: object with a matrix, made with the function makeCacheMatrix
## - any additional parameters for calculating the inverse
## - caution: the function does not check if the same additional parameters
##   are used for subsequent calls. So the inverse will be taken from the cache
##   also if other additional parameters are used

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("using cached inverse")
	} else {
		message("calculating inverse")
		data <- x$get()
		inverse <- solve(data, ...)
		x$setinverse(inverse)
	}
        inverse
}
