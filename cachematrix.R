## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of fuctions can cache the inverse of a matri and avoid unnecessary 
## calculation. 


## THis is the first fuction in the "Caching the Inverse of a Matrix" fuction pair.
## The main role of the first function in this pair fuction is to creat a list to 
## store 4 stadge of the calculation.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) i <<- inverse
  getInverse<- function() i
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## This is the fuction part of this pair fuction.It can calculate the 
## Inverse of the fuction, and after the calculation,it will store the 
## result in the environment created by the first fuction to avoid 
## computing repeatedly.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
