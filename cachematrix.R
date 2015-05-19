## Theese two functions below demonstrate the way
## inversed matrix may be cached in RAM for more efficient use.

## 'makeCacheMatrix' creates a "matrix object" that is able to store inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## inverse matrix variable
  i_inverse <- NULL
  
  # Set function, drops inv variable each time called
  set <- function(y) {
    x <<- y
    i_inverse <<- NULL
  }
  
  # Get method
  get <- function() x
  
  # method that stores inverse matrix in inv variable
  set_inverse <- function(inverse) i_inverse <<- inverse
  
  # Gets cached inverse
  get_inverse <- function() i_inverse
  
  # Returns interface object
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## 'cacheSolve' operates "matrix object" to retreive inverse matrix.
cacheSolve <- function(x, ...) {
  # Getting existing inverse
  inverse <- x$get_inverse()
  
  # Decide to return or to calculate
  if (is.null(inverse)){
    message("Calculating inverse matrix")
    inverse <- solve(x$get())
    x$set_inverse(inverse)
  }
  
  # Now it is cached and we can report that we are returning cached value.
  message('Getting cached inverse value')

  ## Return a matrix that is the inverse of 'x'
  inverse  
}
