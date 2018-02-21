## The functions here create an matrix and cache the inverse of that matirx


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
   setinverse = setinverse,
   getinverse = getinverse)
}


## This function computes the inverse of the special "matrix' returned by makeCacheMatrix
## above. If the nverse has already been calculated, then cacheSolve should retrieve the ## inverse from the cache

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
