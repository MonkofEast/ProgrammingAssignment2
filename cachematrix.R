## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
