## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_mat <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(inv_mat) inverse <<- inv_mat
  get_inv <- function() inverse
  list(set = set_mat, get_mat = get_mat, set_inverse = set_inv, get_inverse = get_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$get_inv()
  if(!is.null(inverse)) {
    message("Getting Cached Matrix.")
    return(inverse)
  }
  mat <- x$get_mat()
  inverse <- solve(mat, ...)
  x$set_inv(inverse)
  inverse
}