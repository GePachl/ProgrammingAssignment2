## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      matrix_inverse <- NULL
      set_matrix <- function(y) {
            x <<- y
            matrix_inverse <<- NULL
      }
      get_matrix <- function() x
      set_inverse <- function(solve) matrix_inverse <<- solve
      get_inverse <- function() matrix_inverse
      list(set_matrix = set_matrix, get_matrix = get_matrix,
           set_inverse = set_inverse,
           get_inverse = get_inverse)      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matrix_inverse <- x$get_inverse()
      if(!is.null(matrix_inverse)) {
            message("getting cached data")
            return(matrix_inverse)
      }
      data <- x$get_inverse()
      matrix_inverse <- solve(data, ...)
      x$set_inverse(matrix_inverse)
      matrix_inverse      
}