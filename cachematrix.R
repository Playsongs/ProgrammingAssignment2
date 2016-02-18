## This program caches the inverse of a matrix via two functions,
## This assumes the matrix is square

## The first function, makeCacheMatrix 
## This function creates a special "matrix" object 
## that can cache its inverse.
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The 2nd function caches the inverse of a matrix
## created with the above function. 
## However, it first checks to see if the inverse has already been cached. 
## If so, it gets the inverse from the cache. 
## Otherwise, it determines the inverse of the matrix via the solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
