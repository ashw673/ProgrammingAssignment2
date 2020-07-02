## Two functions are created to  calculate the inverse of a matrix and store it
## in cache

## The first function (makeCacheMatrix) creates a special vector of a matrix
## to store the inverse of the matrix in a cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following function calculates the inverse of the special "vector" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  print("calculating inverse")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
