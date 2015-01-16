## Functions to provide inverse functionality using the R 'solve' function
## Uses a caching methodology to avoid recalculating unnecessarily

## Creates a matrix for caching to avoid recalculation where possible.

makeCacheMatrix <- function(x = matrix()) {
		
		## create individual actions for set, get, setinv and getinv

		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv

		## create the action list

		list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Creates a call to the cache to check for an existing inverse matrix, and returns either the cached matrix
## or a recalculated inverse which is then saved to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()

        ## Check the cache and message out whenever cache is being used

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the cache is null, create the inverse matrix and set the cache for future requests

        data <- x$get()
        inv <- solve(data, ...)
	        x$setinv(inv)
	        inv
	}