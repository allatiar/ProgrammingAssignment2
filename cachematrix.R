## Compute the inverse of a matrix and cache the result
## usage : m <- matrix(...)
##         mCache <- makeCacheMatrix(m)
##         cacheSolve(mCache)

## Create an object containing its inverse if cache, with accessors
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the value of the matrix
  set <- function(y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get the value of the matrix
  get <- function()
    x
  
  ## set the value of the inverse
  setInverse <- function(inverse = matrix())
    i <<- inverse
  
  ## get the value of the inverse
  getInverse <- function()
    i
  
  ##return the accessors
  list(
    set = set, get = get, setInverse = setInverse,
    getInverse = getInverse
  )
}

## Return a matrix that is the inverse of x, caching the value in x
cacheSolve <- function(x, ...) {
  ## get the cached inverse and return if not null
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the matrix and compute inverse
  data <- x$get()
  i <- solve(data)
  
  ## cache inverse and return
  x$setInverse(i)
  i
}
