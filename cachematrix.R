## Put comments here that give an overall description of what your
## functions do

## The functions here are used to invert an invertible matrix.  
## If the given matrix's inverse has been calculated before, the   
## cached result is retrieved and returned else the inverse is 
## calculated and stored for future reuse. 


## Write a short comment describing this function

## The function creates a list of getter and setter functions
## for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inverse <<- inverse
	getInverse <- function() inverse
  
	list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function checks if the inverse has already been calculated
## and cached before; if yes it returns the cached value else 
## calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		if(!is.null(inverse)){
			message("getting cached matrix inverse data")
			return(inverse)
		}
		m <- x$get()
		inverse <- solve(m, ...)
		x$setInverse(inverse)
		inverse
}
