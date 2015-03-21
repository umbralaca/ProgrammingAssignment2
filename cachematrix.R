## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
	set<-function(y) {
	      x<<-y
	      i<<-NULL
	}
	get<-function() x
	setinv<-function(inv) i<<-inv
	getinv<-function() i
	list(set = set, get = get,
	      setinv = setinv,
	      getinv = getinv)
}


## The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. Otherwise, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        i<-x$getinv()
	if(!is.null(i)) {
	      message("getting cached data")
	      return(i)
	}
	data<-x$get()
	i<-solve(data, ...)
	x$setinv(i)
	i
}
