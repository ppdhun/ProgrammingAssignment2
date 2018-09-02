## The two functions here are to create a special object that stores a matrix and its inverse
## functions do

## The first function below, makeCacheMatrix creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}

## The function below calculates the inverse of the special "matrix" created with the 
# function above. However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
# via the setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

