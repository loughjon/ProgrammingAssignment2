## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse 


makeCacheMatrix <- function(x = matrix(data = c(2,5,7,1),nrow = 2,ncol = 2)) {
         m <- NULL
	 set <- function(y = matrix(data = c(3,5,7,1),nrow = 2,ncol = 2)) {
                 x <<- y
		 m <<- NULL
	 }
	 get <- function() x
	 setsolve <- function(solve) m <<- solve
	 getsolve <- function() m
	 list(set = set, get = get,
              setsolve = setsolve,
	      getsolve = getsolve)
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix above

cachesolve <- function(x = matrix(data = c(3, 5, 7, 1), nrow = 2, ncol = 2), ...) {
         m <- x$getsolve()
	 if(!is.null(m)) {
	        message("getting cached data")
	        return(m)
	 }
	 data <- x$get()
	 m <- solve(data, ...)
	 x$setsolve(m)
	 m
}
											
