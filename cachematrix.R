## Two functions (see individual functions below for detailed description)
## that chache the invesre of a matrix. These function are intended to overcome
## the costly computation of an inverse matrix by caching and returning
## it (where appropriate) rather than requiring repeated calculation.


## A function (makeCacheMatrix) that creates a special 'matrix' (really a list)
## which is really a list containing a function to 
## 1) set the values of the matrix
## 2) get the values of the matrix
## 3) set the values of the inverse matrix
## 4) get the values  of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function that solves (calculates the inverse matrix) of the special 'matrix'
## created by the makeCacheMatrix function above. 
## It first checks if the inverse has already been calculated and
## if the matrix has not changed it retrives this inverse matrix
## from the cahce.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}