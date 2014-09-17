## The pair of below functions cache the inverse of a matrix

## the function create a list of functions to 1.set value of the matrix
## 2. get the value of the matrix  3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix to NULL
  inv <- NULL
  # function to set up the matrix in cache, and update with changes
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get inverse matrix
  get <- function() x
  # calculates the inverse using "solve"
  setinverse <- function(solve) inv <<- solve
  # get the inverse the value of the inverse
  getinverse <- function() inv
  # summarize the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix
## It will first check if the inverse stores in cache. If yes, get the inverse
## and skip the computation. Otherwise, calculate the inverse and set the inverse
## in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # check if the inverse is in cache. if yes, get it.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # calculate the inverse if not already in cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
