## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(solve) inverse <<- solve
  get_inverse <- function() inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
  

## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the 'set_inverse' function.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get_matrix()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}
