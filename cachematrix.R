## We have two functions: makeCacheMatrix, and cacheSolve
## makeCacheMatrix creates a matrix with the ability to store its inverse
##
## cacheSolve computes the inverse of the matrix created by makeCacheMatrix.
## Or if the matrix has already been solved for the inverse, it will retrive
## the inverse.

## made the inver a NULL object
## made a function to get matrix x and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <- NULL
  }
  get <- function()x
  setinv<- function(inverse)inver <<- inverse
  getinv<- function(){
    inv <- ginv(x)
    inv%*%x
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function gets the cache.

## The x$getinv checks if inver is NULL then returns the inverse value given.

## We then calculate the inverse value and return a matrix that is the
## inverse of x.


cacheSolve <- function(x, ...) {
  inver <- x$getinv
  if(!is.null(inver)){
    return(inver)
  }
  data <- inver$get()
  inver <- solve(data, ...)
  x$setinv(inver)
  inver
}
