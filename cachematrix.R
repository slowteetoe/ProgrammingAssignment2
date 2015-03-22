## The pair of these functions are used to cache the results of inverting a matrix,
## which is potentially a time-intesive operation.  The first time a matrix is inverted,
## the result is stored in an environment.  'makeCacheMatrix' is used to set up the object
## used to cache results.  Subsequent requests to 'cacheSolve' with an identical matrix
## will return the *cached* inverted matrix and not recompute the inverse

# Expected usage:

# rm(list=ls()) # clear the environment
# source("cachematrix.R") # source this code
# mymatrix = matrix( c(4,2,7,6), nrow=2, ncol=2)
# m1 <- makeCacheMatrix(mymatrix)	# create an object to cache the inverse 
# cacheSolve(m1) 	# should NOT see "getting cached data" output to console
# cacheSolve(m1)	# should see "getting cached data"

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL	# intitialize local variable to null to store the inverted matrix
    list(
    	set = function(y) {
            x <<- y
            m <<- NULL },
    	get = function() {x},	# return the matrix passed in to this function
        setsolution = function(solution) { m <<- solution }, # store the inverted matrix
        getsolution = function() {m})	# get the inverted matrix, it will be NULL if that particular matrix hasn't been computed yet
}

 ## Return a matrix that is the inverse of 'x' using a cache.  If the
 ## matrix HAS BEEN inverted previously, it returns a cached result and
 ## does not recompute the inverse.

cacheSolve <- function(x, ...) { # x is really the cache environment
	# m will be the inverted matrix
    m <- x$getsolution()	# see if there's a result in the cache
    if(!is.null(m)) {		# if there is, return it and print a message
            message("getting cached data")
            return(m)	
    }
    # otherwise, this is the first time we've attempted to create the inverse of this matrix
    data <- x$get()	# retrieve the matrix from the environment (since x is the environment in THIS function)
    m <- solve(data, ...)	# caclulate the inverse of the matrix, passing along any extra parameters
    x$setsolution(m)	# store the solution in the cache environment
    m  # return the solution
}

