#An overall description of what functions do
# This function creates a special "matrix" object that can cache its inverse.
# There some benefit to caching the inverse of a matrix rather than compute it repeatedly, a matrix
# x is taken and its inverse is calculated and saved as cache

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


#A short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated and the matrix has not changed, then the cachesolve should retrieve the inverse from the cache.
# Return a matrix that is the inverse of 'x'

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
