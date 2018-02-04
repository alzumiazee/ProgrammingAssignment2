makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinvr <- function(inverse) invr <<- inverse
  getinvr <- function() invr
  list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)
}


cacheSolve <- function(x, ...) {
  invr <- x$getinvr()
  if(!is.null(invr)) {
    message("getting cached result")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinvr(invr)
  invr
}
