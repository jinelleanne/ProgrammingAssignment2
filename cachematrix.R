## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix:
## This function is able to cache potentially time-consuming computations
## Matrix inversion is a costly computation but it may benefir to caching the inverse of a matrix rather than computing it repeatedly
## For this exercise our objective is to write a pair of function, namely "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix 

## Write a short comment describing this function 

## makeCacheMatrix is a function which creates a special "matrix" object that can  
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##---------------Checking the program------------------------

## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
##cacheSolve(m1)
##   [,1]      [,2]       [,3]        [,4]
## [1,] -1.098928  1.094188 -0.4900834 -0.04802869
## [2,]  4.003084 -4.836711  0.8073311  0.57751681
## [3,] -5.764512  7.429300 -1.7064557  0.08279431
## [4,]  1.229060 -2.074932  0.6120376  0.22765954
##save.image("C:\\Users\\jinelle.bacani\\Documents\\GitHub\\ProgrammingAssignment2\\.RData")

