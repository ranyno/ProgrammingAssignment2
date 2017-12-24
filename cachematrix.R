## Put comments here that give an overall description of what your
## functions do

## make Cache Matrix will have columns for getsolve, setsolve,
## as well as the typical getter and setters

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	## get will simply return x
	get <- function() x
	setsolve <- function(solve) inv <<- solve
	getsolve <- function() inv
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve will return the cached value if it exists. Otherwise
## it will calculate the value using solve and set it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getsolve()
	if (!is.null(inv)) {
		message("Getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setsolve(inv)
        inv	
}
