## The two functions below can mitigate the computation cost of repetative inversion
## of a matrix. The two functions combine to, first, create a version of a known matrix
## that can cache its inverse. Then, the second function is used to check the cache for
## matrices that have previously been inverted and stored in the cache. If the inverted
## matrix is found, it is returned. If the inverted matrix is not found (i.e. this inverted
## matrix has not previously been calculated), the inverted matrix is calculated and returned.

## makeCacheMatrix creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cachesolve will retrieve the inverse 
## from the cache. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
