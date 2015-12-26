## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates an instance of the CacheMatrix object and initializes
## it with the supplied matrix.  The instance is returned as a list of method 
## references to the anonymous set, get, setInverse, getInverse methods.
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  ## Sets the raw matrix data and clears the inverse data if
  ## it was previously cached
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Returns orginal/raw matrix data
  get <- function() x
  
  ## Stores the inverse data
  setInverse <- function(s) i <<- s
  
  ## Returns the cached inverse data, or NULL if the
  ## inverse data has not been generated
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## Returns the inverse of the raw matrix in the supplied
## CacheInverse instance or will generate the inverse and
## store it in the instance's cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
