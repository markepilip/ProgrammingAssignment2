## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This creates a cached version of the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
          x<<- y
          inv<<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) inv<<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This creates the inverse of the matrix, if it is already solved it returns the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
