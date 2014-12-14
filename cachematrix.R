# Functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## x <- rbind(c(1, -1/5), c(-1/5, 1))
## m <- makeCacheMatrix(x)
## m$get()
##       [,1]  [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##            [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667




