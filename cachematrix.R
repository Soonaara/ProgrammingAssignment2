## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

##makeCacheMatrix creates a special matrix, which is really a list containing a function to
##1.set value of the vector; 2.get the value of the vector; 3.set the value of the mean; 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }   
  get <- function()x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function()i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## Computes the inverse of created matrix.First checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise calculates and sets the value
## in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {    ##checks to see whether result is NULL
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
#B <- matrix(c(5,8,9,3),2,2)   ##input matrix
#B1 <- makeCacheMatrix(B)
#cacheSolve(B1)
