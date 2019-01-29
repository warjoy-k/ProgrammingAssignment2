## Following functions provide method of constructing special CacheMatrix, 
## that can cache its inverse. Function cacheSolve, once proper CacheMatrix is provided, 
## calculates its inverse or returns cached value if it is available.

## Creates a special CacheMatrix object, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix
	getInverse <- function() cachedInverse
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Computes the inverse of provided CacheMatrix (returned by makeCacheMatrix).
## If the inverse has already been calculated (and the matrix has not changed), 
## then cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if (!is.null(matrixInverse)) {
        	message("getting cached data")
        	return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data, ...)
        x$setInverse(matrixInverse)
        matrixInverse
}
