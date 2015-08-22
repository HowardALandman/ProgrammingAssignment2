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
        setinverse <- function(mean) m <<- mean
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
testCacheSolve <- function() {
	errors = 0
	id = diag(2)	## 2x2 identity matrix
	## Test basic solving
	for (i in 1:100) {
		a = replicate(2, rnorm(2))	## random 2x2 matrix
		ac = makeCacheMatrix(a)
		ai = cacheSolve(ac)
		## A matrix times its inverse should equal the identity matrix.
		if (!all.equal((ai %*% a),id)) {
			warning("Inverse times matrix not equal to identity")
			errors = errors + 1
		}
	}
	if (errors > 0) {
		print(errors," errors")
	} else {
		print("all passed")
	}
	## Note that none of this tests the caching.
}
