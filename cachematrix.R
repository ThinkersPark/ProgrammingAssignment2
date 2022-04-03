## Functions created for Coursera/ R Programming by Johns Hopkins University
## Programming Assigment 2

## This function creates a special object CacheMatrix
## With 2 data objects and 4 functions defined within
## this special object's environment:
## Data objects
## 1. x: the matrix 
## 2. invx: the inverse matrix of x
## Functions
## 1. Set: Set the matrix x
## 2. Get: Get the matrix x
## 3. Setinverse: Set the inverse matrix invx
## 4. Getinverse: Get the inverse matrix invx

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- as.matrix(y)
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(i) invx <<- as.matrix(i)
  getinverse <- function() invx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function uses CacheMatrix object as argument
## And returns a matrix that is the inverse of CacheMatrix object x
## If the inverse matrix exists in cache, it is retrieved from there
## If not, it is calculated and assigned to CacheMatrix object invx
## using its setinverse() function
 
cacheSolve <- function(x, ...) {
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data,...)
  x$setinverse(invx)
  invx
}
