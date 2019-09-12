## Attempt to cache inverse of a matrix

## This function creates a special matrix object
## That can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    x <<- NULL
  }
  
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    x <<- inverse
  }
  getInverse <- function() {
    x
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function calculates the inverse
## Of the special matrix created in makeCacheMatrix

cacheSolve <- function(a, ...) {
  m <- a$getInverse()
  if( !is.null(m) ) {
    return(m)
  }
  
  data <- a$get()
  
  m <- solve(data) %*% data
  
  a$setInverse(m)
  
  m
}
