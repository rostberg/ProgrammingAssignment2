## Matt Ostberg 8/13/2017
## Programming Assignment 2
## write functions to create matrix with cache

## This creates a matrix with cache.
## It's a cargo-cult implementation (I took the example function and modified it)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This also borrows heavily from the example and substitutes solve() for mean()

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if(det(data) == 0) {print("Singular matrix, no inverse.")}
  else { 
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
}
