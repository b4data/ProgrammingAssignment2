## The following two functions cache and compute the inverse of a matrix.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   # initialize matrix inverse
   inv <- NULL
   
   # setter for the matrix
   setMatrix <- function(y){
      x <<- y
      inv <<- NULL
   }
   
   # getter for the matrix
   getMatrix <- function() x
   
   # setter for the inverse of the matrix
   setInverse <- function(inverse) inv <<- inverse
   
   # getter for the inverse of the matrix
   getInverse <- function() inv
   
   # return the list of methods
   list(setMatrix = setMatrix, 
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object returned by function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    # return inverse of matrix object
    inv <- x$getInverse()
	
    # returns the inverse if it was cached
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
	
    # get the matrix
    data <- x$getMatrix()
	
    # calculate the inverse of the matrix
    inv <- solve(data)
	
    # set the inverse
    x$setInverse(inv)
	
    # return the matrix
    inv
}
