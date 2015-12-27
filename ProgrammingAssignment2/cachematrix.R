## The function makeCacheMatrix allows you to create an oblect of a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverted_matrix <- NULL 
  set <- function(y) {
    x <<- y
    inverted_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverted_matrix <<- inv
  getinv <- function() inverted_matrix

  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve computes the inverted matrix.
## In case it has already been computed earlier this function returns cached value.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverted_matrix <- x$getinv()
  
  if(!is.null(inverted_matrix)) {
    message("getting cached inverse")
    return(inverted_matrix)
  }
  
  data <- x$get()
  inverted_matrix <- solve(data,...)
  x$setinv(inverted_matrix)
  inverted_matrix
}