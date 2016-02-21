## Create a cached matrix from a regular matrix
## The cached matrix can cache the result of an
## inverse operation so it will be instantaneous
## the second time the solve() function is called.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(s) m <<- s
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## Calculate the inverse of the matrix
## make use of the stored cached value if
## available.
## x must be of the cacheMatrix type.

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("Getting cached data")
        	return(m)
        	}
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
