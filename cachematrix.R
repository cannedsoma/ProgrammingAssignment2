## The first function creates a list of functions that cache a matrix input,
## creating a function list for the second function to interact with.
## The second function calls on the sub-functions of the first function
## to either solve the matrix and cache it or pull already cached data.

## Creates a list of functions that: store a matrix, retrieve a matrix, store an
## inversed matrix or retrieve an inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(c) {m <<- c}
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function solves a matrix and caches it, but only if there is not already
## an object in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setsolve(c)
  c
}
  
## Return a matrix that is the inverse of 'x'
