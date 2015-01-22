## Put comments here that give an overall description of what your
## functions do
## Functions calculate the inverse of a matrix and store 
## in a cache

## Write a short comment describing this function
## Makes a special matrix being a list containing 
## a function to set the matrix, get the matrix,
## set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Checks to see if the inverse is cached and returns, 
##otherwise calculates the inverse and stores in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
