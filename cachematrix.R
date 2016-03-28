## Put comments here that give an overall description of what your
## functions do

## This function assigns values to the matrix. All the functions are stored in a list.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  set <- function(x) {
    m <<- x
    inv <<- NULL
  }
  
  get <- function() m
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse= setInverse, getInverse= getInverse)
  
}


## This function calculates the inverse of the matrix if it is not already available

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-m$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mValues <- m$get()
  inv<-solve(mValues)
  m$setInverse(inv)
  return (inv)
}
