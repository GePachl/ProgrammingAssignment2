## function: makeCacheMatrix 
## creates and stores a base matrix and creates an object 'mat_inv' for storing its inverse matrix ()
## also implements and returns a list of several subfunctions set_matrix, get_matrix, set_matrix_inv, get_matrix_inv 

makeCacheMatrix <- function(x = matrix()) {
      
      mat_inv <- NULL
      
      ## subfunction: creates a matrix      
      set_matrix <- function(y) {
            x <<- y
            mat_inv <<- NULL
      }
      ## subfunction: read base matrix values of x - check if matrix x is created with the values 
      get_matrix <- function() {x}
      
      ## subfunction: creates object 'mat_inv' for storing inverse matrix
      set_matrix_inv <- function(solve) {mat_inv <<- solve}
      
      ## subfunction: reads the object 'mat_inv' - only for check if object is created
      get_matrix_inv <- function() {mat_inv}
            
      ## returns back a list of the following subfunctions
      list( set_matrix = set_matrix, 
            get_matrix = get_matrix,
            set_matrix_inv = set_matrix_inv, 
            get_matrix_inv = get_matrix_inv )      
}


## function: cacheSolve
## returns a matrix that is the inverse of 'x' and stores it in the cache if not already stored

cacheSolve <- function(x, ...) {
      
      ## first trying to look up, if inverse matrix of 'x' is already available in the cache
      mat_inv <- x$get_matrix_inv()
      if(!is.null(mat_inv)) {
            message("getting cached data")
            
      ## if cached inverse matrix of 'x' is found, give it back and the processing ends here             
            return(mat_inv)
      }
      
      ## otherwise process the function 'solve' to get the inverse object 'mat_inv' of the base matrix of 'x'
      mat_inv <- solve(x$get_matrix())
      
      ## save the inverse matrix in the cache
      x$set_matrix_inv(mat_inv)
      
      ## return the inverse matrix data
      return(mat_inv)      
}