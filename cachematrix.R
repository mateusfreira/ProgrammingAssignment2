## Put comments here that give an overall description of what your
## functions do

## Just make a object with the methods  set, get, setSolve, getSolve based on the 'x' matrix passed by parameter
#
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(
          set = set, 
          get = get,
          setSolve = setSolve,
          getSolve = getSolve
      )
}


## try get solve value from cache and if the cache values return NULL calculate the solve and store on cache for future requests, then return the solve value
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(is.null(m)) {
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m) 
  }
  m
}