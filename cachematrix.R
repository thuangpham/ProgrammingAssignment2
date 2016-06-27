## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## Usage:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x) 

makeCacheMatrix <- function(x = matrix()) {
  # initialize a local var inv to store the inverse
  inv <- NULL
  
  #set the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the matrix
  get <- function() x
  
  #set the inverse of the matrix and cached
  setinverse <- function(inverse) inv <<- inverse
  
  #get the inverse of the matrix and cached
  getinverse <- function() inv
  
  #store the functions into a list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## usages:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get matrix 
  inv <- x$getinverse()
  
  #check to see if inv has value, that means it returned from cache
  if(!is.null(inv)) {
    message("Cached data.")
    return(inv)
  }
  
  #if inv is null, then get data from the get
  data <- x$get()
  
  #manually use find the inverse
  inv <- solve(data)
  
  # cache the result
  x$setinverse(inv)
  
  #return the result
  inv
}
