## Put comments here that give an overall description of what your
## functions do

## This function creats a special matrix object tha can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) m << - inverse
	getinverse <- function () m
	list(set = set, get = get,
		 setinverse = setinverse, 
		 getinverse = getinverse)
}


## This function computes inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
	cachesolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(m)
	}	
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(s)
	s
}
