##The following function creates a "special" matrix that has the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       
  specialmatrix <- NULL
  set <- function(y) {
    x <<- y                         ## assigns y to x in a seperate environment
    specialmatrix <<- NULL          ## assigns NULL to specialmatrix in a seperate environment
  }
  
  get <- function() x
  setinverse <- function(solve) specialmatrix <<- solve
  getinverse <- function() specialmatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # This makes a list containing a function to set the value of the matrix, get the value of the
  # matrix, set the value of the inverse matrix and get the value of the inverse matrix.
}


## The following function will first check to see if the inverse of the "set" matrix has already
## been calculated, if it has then it will "get" the result from the cache. If not then it will
## compute the inverse and cache it.

cacheSolve <- function(x, ...) {
  
  specialmatrix <- x$getinverse()          ## So this section will seek to return a cached
  if(!is.null(specialmatrix)) {            ## solution for the inverse of the matrix
    message("getting cached data")         ## as long as the solution does not return a 
    return(specialmatrix)                  ## NULL response.
  }
  
  matrixdata <- x$get()                       ## Should a NULL response be the case, this section
  specialmatrix <- solve(matrixdata, ...)     ## will compute the inverse, cache the result using 
  x$setinverse(specialmatrix)                 ## x$setmatrix and return the result.
  specialmatrix
}
