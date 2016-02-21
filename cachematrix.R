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
	setsolve <- function(s) m <<- s
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)

}


## Calculate the inverse of the matrix
## make use of the stored cached value if
## available.
## x must be of the cacheMatrix type.

cacheSolve<- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if (!is.null(m)) {
        	message("Getting cached data")
        	return(m)
        	}
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

