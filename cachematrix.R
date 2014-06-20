# Defines new CacheMatrix object which stores a matrix, and also caches its inverse
# for future use once computed.

# Function makeCacheMatrix()
# Builds a new CacheMatrix object with
#
# ARGS
#  x matrix defining data for the matrix.
# 
# RETURNS
#  List of functions allowing access to the contents of this object.
#  set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        # Changes value of matrix to y.
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        # Returns current value of the matrix
        x
    }
    
    setinverse <- function(inv) {
        # Sets the inverse of the matrix.
        inverse <<- inv
    }
    
    getinverse <- function() {
        # Returns the previously stored inverse of the matrix.
        inverse
    }
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Function cacheSolve()
# Computes the inverse of a matrix, either computing directly and caching in 
# CacheMatrix object or retrieving the cached version from the CacheMatrix.
#
# ARGS
#   x   CacheMatrix object to be inverted
#   ... Optional additional args to pass to solve() if required
#
# RETURNS
#   Inverse of x as matrix.
cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("Cached inverse exists.")
        return(inverse)
    }
    
    message("No cached inverse - computing now.")
    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setinverse(inverse)
    inverse
}