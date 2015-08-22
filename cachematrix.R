## Add caching of the matrix inverse to a matrix,
## so that if you've already computed the inverse,
## you just return the cached value instead of
## computing it over again (which can be expensive).
##
## NOTE: For this assignment we can assume that the
## matrix is square and invertible.  Only add checking
## if there's extra time.

## Create 4 utility functions to implement the above.
## This follows the example code as closely as possible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	## Function to set the matrix itself
        set <- function(y) {
                x <<- y		## Set the matrix.
                m <<- NULL	## Forget any previously stored inverse.
        }
	## Function to get the cached matrix
        get <- function() x
	## Function to set the cached inverse
        setinverse <- function(inv) m <<- inv
	## Function to get the cached inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return the inverse of x, using cached value if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## Check if we have a stored inverse, and if so return it.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## else compute it and store it for next time
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Test the above code.
testCacheSolve <- function(reps=100,size=2) {
	## reps = number of tests to perform
	## size = number of rows (and columns) of the square matrices
	errors = 0
	id = diag(size)	## identity matrix
	## Test basic solving
	for (i in 1:reps) {
		a = replicate(size,rnorm(size))	## random square matrix
		ac = makeCacheMatrix(a)
		ai = cacheSolve(ac)
		## A matrix times its inverse should equal the identity matrix.
		if (!all.equal((ai %*% a),id)) {
			errors = errors + 1
		}
	}
	## Print test results.
	## Note that cat() does what we want here while print() doesn't.
	if (errors > 0) {
		cat(paste(errors,"errors out of",reps,"tests"))
	} else {
		cat(paste("all",reps,"tests passed"))
	}
	## Note that none of this tests the caching.
}
