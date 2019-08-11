
## cachematrix.R
## source("cachematrix.R")


makeCacheMatrix <- function(x = matrix()) {
        
        # makeCacheMatrix: This function creates a special "matrix" object 
        # that can cache its inverse.
        
        # makeCacheMatrix creates a list containing a function to
        # 1. set the value of the matrix
        # 2. get the value of the matrix
        # 3. set the value of inverse of the matrix
        # 4. get the value of inverse of the matrix
        
        cacheMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        
        getMatrix <- function() x
        setCache <- function(inverse) cacheMatrix <<- inverse
        getCache <- function() cacheMatrix
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setCache = setCache,
             getCache = getCache)
}


cacheSolve <- function(x, ...) {
        cacheMatrix <- x$getCache()
        if (!is.null(cacheMatrix)) {
                message("getting cached data...")
                return(cacheMatrix)
        } else {
                dMatrix <- x$getMatrix()
                cacheMatrix <- solve(dMatrix, ...)
                x$setCache(cacheMatrix)
                return(cacheMatrix)
        }
}