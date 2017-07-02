## Cashing of the Inverse of a Matrix
## The below two functions allows us to avoid a costly inverse computation
## by checking if the inverse already exists
## Assumption: Matrix supplied if always invertible

## This function can cache the inverse of a special "matrix" object 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## his function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. It first checks if the inverse has already been calculated
## and then retrieves the inverse from the cache if available.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
