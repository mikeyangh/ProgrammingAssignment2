################################################################
##  Matrix inversion is usually a costly computation and their
##  may be some benefit to caching the inverse of a matrix 
##  rather than compute it repeatedly. 
##  These two functions can calculate the inverse of a matrix
##  and store the result.

################################################################
## This function creates a special object(matrix) 
## which is really a list contain a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix
makeCacheMatrix<- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
###############################################################
## The following function calculates the inverse of
## the special "vmatrix" created with the above function(makeCacheMatrix).
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse
## from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating data")
  data <- x$get()
  inv <- solve(data, ...)   
  x$setInverse(inv)
  inv
}
