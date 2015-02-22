## A pair of functions that cache the inverse of a matrix 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Asiggning the new value as the stored value
  ##Inverse calculation is reset here
  set <- function(y) {
    x <<- y
    inv <<- NULL  ## Inverse start as NULL
  }
  ## returns the internal object
  get <- function() x
  
  ## Get & Set functions for Inverse
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
  
  }
  data <- x$get()
  inv <- solve(data)  ## Calculates the inverse
  x$setInverse(inv)
  inv
  
}
