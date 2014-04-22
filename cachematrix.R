## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #Giving the uncalculated inverse to a NULL value then caching it with a
    #inverse setting function
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setinverse <- function(InverseMatrix) invM <<- InverseMatrix
    getinverse <- function() invM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the 
#inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinverse(invM)
    invM
    
}
