## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value of the matrix
## get the value of the matrix
## set the value of the solve
## get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Checks if the solve has already been calculated. 
## If so, gets the solve from the cache, otherwise calculates the solve of the data and sets the value cache.
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

