## The following two functions together can help cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <-function(y){
		x<<-y
		xinv <<-NULL
	}
	get <- function() x
	setInverse <- function(xinv2) xinv <<- xinv2
	getInverse <- function() xinv
	list(set=set,get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	xinv<-x$getInverse()
	if(!is.null(xinv)){
		message("getting cached data")
		return(xinv)
	}
	data <-x$get()
	xinv<-solve(data,...)
	x$setInverse(xinv)
	xinv	
}
