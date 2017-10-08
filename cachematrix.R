## This is about making a matrix caching its inverse
## using the guidelines in coursera.

## "makeCasheMatrix" function is to make a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- null
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## "cacheSolve" function computes the inverse of the matrix made by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
                }
        data <- x$getInverse()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
