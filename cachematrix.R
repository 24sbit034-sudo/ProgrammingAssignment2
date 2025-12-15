makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initially, inverse is NULL
  
  # Set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix is changed
  }
  
  # Get the matrix value
  get <- function() x
  
  # Set the inverse value
  setinv <- function(inverse) inv <<- inverse
  
  # Get the inverse value
  getinv <- function() inv

  # Return the list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse using solve() function
  x$setinv(inv)  # Cache the computed inverse
  inv  # Return the inverse
}


