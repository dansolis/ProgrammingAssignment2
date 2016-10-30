## The two functions below are used to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix function creates a list containing functions to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv  <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function calculates inverse of matrix created in above functin. First, it checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
