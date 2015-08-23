##  Below is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that generates a set of functions stored in the form of a list, which will be 
## the input of the next function cacheSolve. In this function, x is the matrix we input and the calculation 
## of its inverse matrix will be stored in m.

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


## cacheSolve function will first check whether the inverse matrix is stored in m already. If so, the result will be returned 
## immediately with a message saying "getting cached data".If not, the inverse matrix will be calculated using the Solve 
## function and then the result is saved in m as a cache.


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

## How to use the funcions:
## > z<-makeCacheMatrix(matrix(c(1,2,0,2,3,4,-2,-3,3),3,3))
## > cacheSolve(z)
           [,1]       [,2]      [,3]
[1,] -3.0000000  2.0000000 0.0000000
[2,]  0.8571429 -0.4285714 0.1428571
[3,] -1.1428571  0.5714286 0.1428571
## > cacheSolve(z)
getting cached data
           [,1]       [,2]      [,3]
[1,] -3.0000000  2.0000000 0.0000000
[2,]  0.8571429 -0.4285714 0.1428571
[3,] -1.1428571  0.5714286 0.1428571

