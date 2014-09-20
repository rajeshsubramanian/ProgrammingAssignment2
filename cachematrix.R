## Put comments here that give an overall description of what your
## functions do

## Creates a custom Matrix with get and set methods
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}


## Solves to find the inverse of a matrix.
## Checks to see if the inverse is cached and returns it if it is
## Calculates the inverse and caches it if it isn't cached
cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
