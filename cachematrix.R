## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than computing it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# My assignment is to write a pair of functions that cache the inverse of a matrix.
##

## Write a short comment describing this function
# input:
#   x: a matrix
# output:
#   makeCacheMatrix: a list of functions
# This structured list of funcitons act like a class with basic constructer and 
# basic function
## comment over
makeCacheMatrix <- function(x = matrix()) {
  # set default invM to NULL
  invM <- NULL
  # set func
  set <- function(yM)
  {
    x <<- yM
    invM <- NULL
  }
  # get func
  get <- function() x
  # setinv func
  setinv <- function(outInvM) invM <<- outInvM
  # getinv func
  getinv <- function() invM
  # return the func list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# input: 
#   x: a list of function, like a class
# output:
#   caheSolve: the invered mat
# This function is the paired funtion of the last one,
# typically serves the class-like list structure of a mat
## comments off
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get saved inv frist
  invM <- x$getinv()
  # if exist, return
  if(!is.null(invM))
  {
    message("Cached inv mat got")
    return(invM)
  }
  # if not saved, cal it
  dat <- x$get()
  invM <- solve(x$get(),...)
  x$setinv(invM)
  return(invM)
}
