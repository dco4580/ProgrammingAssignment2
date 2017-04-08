## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:
##   1. makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
##   2. cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the  solve  function in R. For example, if  X  is a square invertible matrix, then  solve(X)  returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

  

## Write a short comment describing this function

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


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
  
}

## Sample run: 
## > x = rbind(c(1, -1/8), c(-1/8, 1)) 
## > m = makeCacheMatrix(x) 
## > cacheSolve(m) 
## [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730

## Retrieving from the cache in the second run 
## > cacheSolve(m) 
## getting cached data. 
## [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
##

