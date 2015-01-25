## Caching the inverse of a matrix rather than computing it repeatedly
## Functions below caches the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(matrix) m <<- matrix
  getInvMatrix <- function() m
  list(set = set, get = get,setInvMatrix = setInvMatrix,getInvMatrix = getInvMatrix)
  
}

## Computing the inverse of a square matrix

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInvMatrix(m)
  m
}
