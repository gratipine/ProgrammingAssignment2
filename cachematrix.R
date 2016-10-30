## Contains two functions which serve to cache 
## the inverse of a matrix the first time it is calculated and just call up the
## cached result the next time it is needed

## creates a list of functions for access to a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x<<-y
    inverse <<-NULL
  } 
  getMatrix <- function() x
  setInverse <-function(inv) inverse <- inv
  getInverse <-function() inverse
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Tries to get the inverse of a matrix. If it has not been calculated yet,
## it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  data<-x$getMatrix()
  inverse<-solve(data)
  x$setInverse(inverse)
  inverse
}
