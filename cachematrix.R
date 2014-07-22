# These functions demonstrate lexical scoping rules in R by creating
# an environment which stores a matrix, computes its inverse when
# requested and stores the computed inverse to avoid recalculation

# makeCacheMatrix creates an environment which stores a matrix and
# its inverse inside it (using lexical scoping) and returns functions:
#   set(x):        Set the matrix to x and reset the inverse to NULL
#   get():         Get the stored matrix
#   setinverse(x): Set the inverse of the matrix to x
#   getinverse():  Get the inverse of the matrix (if calculated)
# An example of using this function to create a cached matrix from matrix x
# and getting back the matrix:
#   y <- makeCacheMatrix(x)
#   z <- y$get()
makeCacheMatrix <- function(thematrix = numeric()) {

	# Set the matrix inverse inside the environment (initially set to NULL
	# since we only want to compute the inverse the first time it is
	# requested
	inverse <- NULL
	
	# Set a new function to invert - this sets matrix in the parent
	# (makeCacheMatrix) environment and resets the inverse to NULL
	# since we'll need to compute the inverse of the new matrix if
	# required
	set <- function(matrix_in) {
		thematrix <<- matrix_in
		inverse <<- NULL
	}
	
	# Get the matrix
	get <- function() thematrix   # return the matrix
	
	# Set the inverse of the matrix in the makeCacheMatrix environment
	setinverse <- function(inv_in) inverse <<- inv_in
	
	# Get the matrix inverse (may be NULL if it hasn't been calculated yet)
	getinverse <- function() inverse
	
	# Return a list of functions so they can be accessed by the calling
	# environment
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}

# cacheSolve takes in a cached matrix created using makeCacheMatrix environment
# above and returns the inverse.  The inverse is retrieved from the cached
# matrix if it has already been computed, otherwise it computes the matrix
# and stores it in the cached matrix
# For example, if y is a cached matrix, cacheSolve(y) returns the inverse
cacheSolve <- function(cachedMatrix,...) {

	# Get the inverse matrix from the cached matrix
	inv <- cachedMatrix$getinverse()
	
	# If it is not NULL, we have already computed it so retrieve the cached
	# value and return it
	if(!is.null(inv)) {
		message("getting cached inverse") # debugging message
		return(inv)
	}
	
	# Otherwise, we need to compute the inverse:
	# Get the cached matrix value
	thematrix <- cachedMatrix$get()
	# Invert it
	inv <- solve(thematrix,...)
	# Store it in the cached matrix
	cachedMatrix$setinverse(inv)
	# Return the inverse
	inv
}
