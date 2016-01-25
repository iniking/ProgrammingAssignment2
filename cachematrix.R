## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list containg functions to set and get a matrix.
  ## This function also contains functions to set and get the inverse of the matrix.
  invMatrix = NULL
  set = function(y) {
    
    x <<- y
    invMatrix <<- NULL
  }
  get = function() x
  setInv = function(inverse) invMatrix <<- inverse 
  getInv = function() invMatrix
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix = x$getInv()
  
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  data = x$get()
  invMatrix = solve(data, ...)
  
  x$setInv(invMatrix)
  
  return(invMatrix)
}
