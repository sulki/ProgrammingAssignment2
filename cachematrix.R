## The follow functions compute the inverse of a square matrix cashing the result.
## How to use it:
## 1. Create a "special matrix" passing a square matrix as argument to makeCacheMatrix()
##    e.g.: a<-makeCacheMatrix(matrix(c(1:4),2))
## 2. Get the matrix inverse using cacheSolve()
##    e.g.: cacheSolve(a)


## Create a "special matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Compute the inverse of the "special matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x,...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
        ## Return a matrix that is the inverse of 'x'
}
