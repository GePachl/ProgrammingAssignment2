## function: makeCacheMatrix
## creates and stores a matrix, then creates the inverse version and caches it for later use by other functions and programs 
## also implements and returns back a list of several subfunctions set_matrix, get_matrix, set_inverse, get_inverse 

makeCacheMatrix <- function(x = matrix()) {
      
      matrix_inverse <- NULL
      
      ## creates and stores a matrix      
      set_matrix <- function(y) {
            x <<- y
            matrix_inverse <<- NULL
      }
      ## reads the created matrix - only for looking up, if matrix is created
      get_matrix <- function() x
      
      ## now main execution - the matrix will be transformed by the 'solve' function to its inverse form and cached in object 'matrix_inverse'
      set_inverse <- function(solve) matrix_inverse <<- solve
      
      ## reads the inverse matrix - only for looking up, if it is created
      get_inverse <- function() matrix_inverse
      
      ## returns back a list of the following subfunctions 'set_matrix', 'get_matrix', 'set_inverse', 'get_inverse'
      list(set_matrix = set_matrix, get_matrix = get_matrix,
           set_inverse = set_inverse,
           get_inverse = get_inverse)      
}


## function: cacheSolve
## returns a matrix that is the inverse of 'x' and stores it in the cache if not already stored

cacheSolve <- function(x, ...) {
      
      ## first trying to look up, if inverse matrix of 'x' is available in the cache
      matrix_inverse <- x$get_inverse()
      if(!is.null(matrix_inverse)) {
            message("getting cached data")
            
      ## if cached inverse matrix is found, give it back and the processing ends here             
            return(matrix_inverse)
      }
      
      ## if not available then go on ... get the matrix and assign it to object variable data      
      data <- x$get_inverse()
      
      ## process the function 'solve' to get the inverse form of matrix 'data'
      matrix_inverse <- solve(data, ...)
      
      ## save the inverse matrix in the cache
      x$set_inverse(matrix_inverse)
      
      ## return the inverse matrix
      matrix_inverse      
}