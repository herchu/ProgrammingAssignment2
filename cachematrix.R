

## Coursera - R Programming
## Programming Assignment 2
## Creating a matrix object with a cacheable inverse value

## Example to test this functionality:
##    a <- makeCacheMatrix(example)
##    a$get()
##    a$getinv()
## (returns NULL, not calculated)
##    cacheSolve(a)
## (prints the inverse without additional messages)
##    cacheSolve(a)
## (prints the same inverse but also the message "using cached data")

## Some test data. This is an invertible matrix
example <- matrix(c(1,2,3,11,12,13,21,22,21), nrow=3, ncol=3)

## makeCacheMatrix creates a matrix wrapper that can hold the inverse
## value in cache. The value is invalidated whenever the matrix changes.
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(x) {
    mat <<- x
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(m) inv <<- m
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix, using the wrapper
## object created by makeCacheMatrix. First it checks if the inverse
## has been calculated already, this must not be done twice. If it is 
## not already calculated, it uses standard R's solve() function and
## caches the result.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("returning cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}


## END OF ASSIGNMENT'S SOLUTION...
## EXAMPLE CODE BELOW -- NOT TO BE EVALUATED


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}