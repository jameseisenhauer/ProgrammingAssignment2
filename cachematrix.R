## R Programming Programming Assignment 2


## name: makeCacheMatrix
## purpose: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
             x <<- y
             m <<- NULL
      }
      get <- function() x
      # sets inverse of matrix
      setinverse <- function(solve) m <<- solve
      # gets inverse of matrix
      getinverse <- function() m
      # returns list of functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)


}


## name: cacheSolve
## purpose: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of x
        m <- x$getinverse()
        # Check if value is cached
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }
        data <- x$get()
        # inverse the matrix
        m <- solve(data, ...)
        # set cache value
        x$setinverse(m)
        m
}
