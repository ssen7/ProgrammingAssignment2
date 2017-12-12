## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  
  i <- matrixinverse(data, ...)
  x$setmean(i)
  i
}

matrixinverse <- function(x, ...) {
  if(!is.matrix(x)){
    message("input is not a matrix")
    return()
  }
  if(det(x) == 0){
    message("input is a singular matrix")
    return()
  }
  solve(x, ...)
  
}