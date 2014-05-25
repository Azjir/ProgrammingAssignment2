## Creates a square invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  ##sets matrix x to matrix argument passed.
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  ##returns matrix x
  get <- function() x
  ##sets the inverse to the variable I
  setInverse <- function(Inverse) I <<- Inverse
  ##returns the inverse, I.
  getInverse <- function() I
  ##returns a list of the preceeding functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## If the inverse has already been computed, it returns the cached value
cacheSolve <- function(x, ...) {
  ## sets I to the inverse, if it already exists.
  I <- x$getInverse()
  ## checks to see if inverse already exists, returns it if it does.
  if(!is.null(I)) {
    message("getting cached inverse")
    return(I)
  }
  ## sets data to the matrix created.
  data <- x$get()
  ## computes the inverse of the matrix data and sets the value to I
  I <- solve(data)
  ## caches the inverse computed.
  x$setInverse(I)
  ## returns inverse.
  I
}
