## This function retruns a list with setters and getters.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

## sets the matrix to super variable x. 
 	set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i

## retrun the list with all the functions.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function returns a inverse Matric created by the 
## function makeCacheMatrix. If the inverse is already in the 
## cache (global env) it will retrun the same from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
	 i <- x$getInverse()
## check if cached
	 if(!is.null(i)) {
		 message("getting cached data")
## retrun from the cache
		 return(i)
	 }
	 data <- x$get()
## if not cached reverse the matrix.
	 i <- solve(data, ...)
	 x$setInverse(i)
## return the inverse of the matrix.
	 i
        
}
