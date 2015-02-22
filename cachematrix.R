## Using the provided example (caching mean of a vector) for this assigment 
## as a base, modified those two functions to create one function
## that creates a special "matrix" object and another function that
## computes the inverse of this special "matrix" object

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  sm <- NULL
  set <- function(y) {
    x <<- y
    sm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) sm <<- inverse
  getinverse <- function() sm
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated
## and is unchanged, it returns the inverse from the cache
## instead of wasting time trying to recompute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  sm <- x$getinverse()
  if(!is.null(sm)) {
    message("getting cached inverse of matrix")
    return(sm)
  }
  data <- x$get()
  sm <- solve(data, ...)
  x$setinverse(sm)
  sm
}
