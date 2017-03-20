## Put comments here that give an overall description of what your
## functions do

## function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {         #defining function argument  of 'x' as matrix
  invmat <- NULL                                    #invmat initally set to NULL, will hold value of inverse matrix
  set <- function(y) {                              #assigns input and value of function(y) to parent environment
    x <<- y
    invmat <<- NULL
  }
  get <- function() x                              #defines matrix arg for 'x' (utilizes lexical scoping)
  setinverse <- function(solve) invmat <<- solve   #setting the inverse of the matrix
  getinverse <- function() invmat                  #assigns functions to a list and stores in the parent environment
  list(set = set, get = get,                       
       setinverse = setinverse,
       getinverse = getinverse)
}


## populate or retrieve inverse matrix from 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  invmat <- x$getinverse()                      
  if(!is.null(invmat)) {                        #if inverse matrix has already been calculated, get previously calculated data
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}
