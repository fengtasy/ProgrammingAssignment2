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
        # m stores the cached inverse matrix
        m <- NULL # Initialise to NULL if cacheSolve ran previously
        
        # setter for the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL # Initialise to NULL if cacheSolve ran previously
        }
        
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse, using 'solve' function
        setinverse <- function(solve) m <<- solve
        
        # Getter for the inverse
        getinverse <- function() m
        
        # Returns the matrix with newly defined functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

# The cacheSolve function returns the inverse of the matrix returned by makeCacheMatrix.
# Firstly, it checks if the inverse has already been computed. 
# If yes, cacheSolve retrieves the inverse from the cache and skips the computation. 
# If no, it computes the inverse, sets the value in the cache via setinverse function.

# Inverse of a square matrix can be achieved with the 'solve' function in R.
# X is a square invertible matrix: solve(X) returns X's inverse.

## We assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of "x" from cache
        m <- x$getinverse()
        
        # Check if "m" is not null 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Else statement: if m is null, get the matrix "x", and put it in "data" 
        data <- x$get()
        
        # Use 'solve' function to compute the inverse of the matrix "data",
        # and put the result into "m"
        m <- solve(data, ...)
        
        # Call function setinverse() from makeCacheMatrix to cache the inverse
        x$setinverse(m)
        
        # Return the inversed result "m"
        m
}

## TEST ##

# Create a square matrix
x <- diag(5,6); x

# Create the matrix to be inverted and puts it into "y"
y <- makeCacheMatrix(x)

# Return the matrix
y$get()

# Return the inverse of the matrix "y"
cacheSolve(y)

# Calls again the inverse, so it gets cached data
cacheSolve(y)


