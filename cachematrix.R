## Put comments here that give an overall description of what your
## functions do
## Provided a matrix, this function allows returning the inverse of the given matrix by:
## 1) Estimating the inverse of the matrix (using solve()function) and storing it in the cache memory
## 2) In the case the inverse matrix is already cached, skipping the computation of the inverse matrix again and directly returning the inverse matrix from the cache memory

## Write a short comment describing this function
## This function defines the tools or better said the functions to estimate the inverse of the matrix assigning to the object "m" the value of the inverse matrix.
## In other words, it can estimate the inverse of the matrix (using solve()function) and store it in the cache memory
## This first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## Write a short comment describing this function
## This function checks if the inverse matrix is already stored in the cache memory.
## In the case the inverse matrix is already cached, it skips computing the inverse matrix again and directly returns it from the cache memory
## If not, it calls the functions to do so and finally return the inverse matrix.

cacheSolve <- function(x, ...) {
                m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
