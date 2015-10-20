## Put comments here that give an overall description of what your
## functions do

## Cachematrix caches the inverse of a matrix so that if the 
## contents of the matrix is not changed, the inverse can be
## looked up in the cache rather thanr recomputed

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse via the following 4 functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, 
		setinv = setinv,
		getinv = getinv)
}


## Write a short comment describing this function

## The following function computes the inverse of the special 
## "matrix" returned the above function. 
## If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve will retrieve
## the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value of 
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
