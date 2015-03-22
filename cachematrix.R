## Put comments here that give an overall description of what your
## functions do

## makes a list containing functions to get & set the matrix,
## and get & set the inverse of the matrix for cacheing

makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  get <- function() x
  setcached <- function(new_val) cached <<- new_val
  getcached <- function() cached
  list(set = set, get = get,
       setcached = setcached,
       getcached = getcached)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getcached()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setcached(inverse)
  inverse
}
