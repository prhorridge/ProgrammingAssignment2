## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(thematrix = numeric()) {
	inverse <- NULL
	set <- function(matrix_in) {
		thematrix <<- matrix_in   # set new matrix to invert
		inverse <<- NULL          # reset the inverse since we need to recalculate
	}
	get <- function() thematrix   # return the matrix
	setinverse <- function(inv_in) inverse <<- inv_in # set the inverse
	getinverse <- function() inverse                  # return the inverse
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(cachedMatrix,...) {
	inv <- cachedMatrix$getinverse()
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	thematrix <- cachedMatrix$get()
	inv <- solve(thematrix,...)
	cachedMatrix$setinverse(inv)
	inv
}
