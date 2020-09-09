## Creating a matrix that will store inverse in cache memory
makeCacheMatrix <- function(x = matrix()) {
  temp_inv <- NULL
  set <- function(y){
    x <<- y
    temp_inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) temp_inv <<- inverse
  getInverse <- function() temp_inv 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## The next function will check if the matrix is already solved or not. If solved, it will fetch fromcache,or else, it will solve.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  temp_inv <- x$getInverse()
  if(!is.null(temp_inv)){ ## If matrix is cached, return the previously evaluated inverse of the matrix
    message("getting cached data")
    return(temp_inv)
  }
  mat <- x$get() ## If matrix is not cached, evaluate the inverse of the matrix and return the value
  temp_inv <- solve(mat,...)
  x$setInverse(temp_inv)
  temp_inv
}
