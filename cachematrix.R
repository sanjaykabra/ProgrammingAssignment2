## Functions are used to calculate inverse of matrix using caching. If a calculated
## inverse of a earlier computed matrix exists and is not changed cached value is 
## returned

## This function creates a list of functions to set and retrieve the matrix
## and its inverse from/to cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the created matrix in above function and finds the inverse
## of it in cache, if found then it retrieves and returns from cache but if not
## solve function of R is used to calculate the inverse of given matrix and set 
## it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
