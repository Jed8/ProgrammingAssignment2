## Functions to create a cache for a matrix and return its inverse

## Make a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
          x <<- y
          i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)){
          message("getting cached data")
          return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
