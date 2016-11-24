## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation. It is a good idea to cache
# the Matrix inversion. The below makeCacheMatrix and cacheSolve functions are 
# used to cache the inversed matrix.

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# The cacheSolve function returns the inverse of the matrix. Firstly, it checks 
# if the inverse has already been computed. 
# If yes, cacheSolve retrieves the inverse from the cache and skips the computation. 
# If no, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


