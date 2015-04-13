## The two functions cache the inverse of a matrix  rather than compute it repeatedly


# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
				inverse.matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse.matrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(m) inverse.matrix <<- m
        getInverseMatrix <- function() inverse.matrix
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# 	then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverseMatrix(m)
        m
}
