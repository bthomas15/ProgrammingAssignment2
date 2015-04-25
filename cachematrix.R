## The functions will create a matrix from the input
## provided and then calculate the inverse of that matrix 
## to be given as the output when called. 

## This function will create a matrix from the input given.
##setinverse will only store the value of the input and 
##getinverse will return it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<- function(y) {
    x <<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function()i
  list(set=set, get=get, setinverse=setinverse, 
       getinverse=getinverse)
}
## cacheSolve will take in the matrix object as the input 
## and calculate the inverse of that matrix.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
