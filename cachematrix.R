## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Below function 'makeCasheMatrix' creates a special Matrix Object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setReverse <- function(reverse) m <<- reverse
  getReverse <- function() m
  list(set = set, get = get,
       setReverse = setReverse,
       getReverse = getReverse)
  
  
}


## Write a short comment describing this function

## Below function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getReverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setReverse(m)
  m
  
  }
