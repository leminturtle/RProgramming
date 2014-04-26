## Caching the Inverse of a Matrix

## The makeCacheMatrix function is used to set and get the value of the matrix
## It also used to set and get the inverse of a matrix to allow a faster operation
## of the code without recalculating the inverse again if the same matrix is queried

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function (smatrix) {
    x <<- smatrix
    inverse <<- NULL
  }
  
  get <- function () x
  
  setinverse <- function(imatrix) inverse <<- imatrix
  
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
  
}


## The cacheSolve function is used to cache the inverse of a matrix.
## If the inverse of a certain matrix has been calculated before, it will
## directly retrieve it from cache rather than recalcuate it again

cacheSolve <- function(x, ...) {
  
  checkcache <- x$getinverse()
  
  if (!is.null(checkcache)) {
    
    message("getting cached data")
    
    return(checkcache)
  }
  
  getmatrix <- x$get()
  
  inversematrix <- solve(getmatrix)
  
  x$setinverse(inversematrix)
  
  inversematrix
}
