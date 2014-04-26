## Put comments here that give an overall description of what your
## functions do

## Creates an enhanced matrix vector, including functions to 
## get and set the value of the matrix, as well as to get and 
## set the value of its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Return the inverse of a (supposedly invertible) matrix,
## getting it from cache if available, or calculating it
## if not available in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
