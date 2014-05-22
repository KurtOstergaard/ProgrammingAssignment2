## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # m will store the cached inverted matrix
  m <- NULL
  
  # The set function stores the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # the get function retrieves the matrix
  get <- function() x
  
  # set the inverse
  setsolve <- function(solve) m <<- solve

  # get the inverse
  getsolve <- function() m
  
  # return the matrix with new functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve computes the invers of a matrix, however
# if the inverse is already calculated, it returns the 
# cached inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  # if the inverse is in cache return that
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # cache the inverse
  x$setsolve(m)
  
  # return the inverse
  m
}
