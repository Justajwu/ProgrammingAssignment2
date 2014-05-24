## These two functions work in conjuncture to potentially speed up a matrix inverse calculation by using 
## previously cached data instead of solving it out each time. The makeCacheMatrix function stores the values of
## both the inputted matrix and its inverse matrix should it had been solved before. The cacheSolve checks
## to see if there was an inverse matrix value stored in the makeCacheMatrix, and if not, solves for the inverse
## matrix itself.

## This function stores all the cached values. If there isn't a cached inverse matrix, it will return NULL.

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) { 
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) i <<- inverse
                getinverse <- function() i
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }



## This function checks the makeCacheMatrix for a stored value in "getinverse" variable. If it returns NULL
## then this function would solve for the inverse matrix (by first retreiving the matrix from makeCacheMatrix) 
## and store that value in the makeCacheMatrix (by assigning a value for the setinverse variable of the makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
