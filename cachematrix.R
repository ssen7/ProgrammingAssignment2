## This function generates a special matrix that caches 
## the inverse of that same matrix

## Generates a list containing the matrix, 
## its inverse and setter/getter functions for the same

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


## Generates the inverse of a matrix and stores it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  
  i <- matrixinverse(data, ...)
  x$setinverse(i)
  i
}

## Calculates inverse of the matrix
## Not necessary for the assignment but I wanted to test what return() does
## and NULL/type checks are always good programming practice ;)

matrixinverse <- function(x=matrix(), ...) {
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