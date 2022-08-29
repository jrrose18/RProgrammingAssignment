makeCacheMatrix <- function(d = matrix()) {
  inv <- NULL
  set <- function(r) {
    d <<- r
    inv <<- NULL
  }
  get <- function() d
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheinverse <- function(d, ...) {
  inv <- d$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- d$get()
  inv <- solve(matrix_to_invert, ...)
  d$setinverse(inv)
  inv
}