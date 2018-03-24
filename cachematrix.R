### Data Science Specialization by Johns Hopkins University
### Course No 2 titled: 'R Programming', Week: 3
### Peer-graded Assignment - Programming Assignment 2: Lexical Scoping
### Marcin Grabarczyk
 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the value of the matrix.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the value of the matrix.
  get <- function() x
  
  # Set the value of inverse of the matrix.
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the value of inverse of the matrix.
  getinverse <- function() inv
  
  # Returns list of all four handles to functions.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  
  # Checks if the inverse has already been calculated (and the matrix has not changed).
  if(!is.null(inv)) {
  
  # If TRUE, then function gets the result and skips the computations.
    message("getting cached data.")
    return(inv)
  }
  
  # If FALSE, function computes the inverse, sets the value in the cache via setinverse function.
  data <- x$get()
  inv <- solve(data)
  
  x$setinverse(inv)
  
  # Returns the inverse of the special "matrix" made by makeCacheMatrix.
  inv
}
