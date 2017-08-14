## This file contains 2 functions  
## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve will retrieve the inverse from the cache.

## The function to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## setting the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getting the value of the matrix
  get <- function() x
  
  ## setting the value of the inverse
  setInverse <- function(Inverse) inv <<- Inverse
  
  ## getting the value of the inverse
  getInverse <- function() inv
  
  #returns a list with elements that set & get the value of the matrix and 
  # set & get the value of the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above 

cacheSolve <- function(x, ...) {
  
  ## the cachesolve retrieves the inverse from the cache
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x' from cache
    return(inv)
  }
  ## Computing the inverse of the special "matrix"
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x' by computing it
  inv
        
}
