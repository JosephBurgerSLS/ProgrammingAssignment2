
## This function initializes the inverse property, sets the matrix,
## gets the matrix, sets the inverse, gets the inverse and returns the list

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invert <<- solveMatrix
  getInverse <- function() invert
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function generates the inverse of the matrix by getting the matrix,
## calculating the inverse, setting the inverse to the object, and printing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invert <- x$getInverse()
  if(!is.null(invert)){
    message("retrieving data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setInverse(invert)
  invert
}
