## A set of function acquiring a matrix and computing its
## inverse

## Create a special matrix containing functions to:
## 1-set its value  2-get its value
## 3-set its inverse value 4-get its inverse value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculate the inverse of the special matrix. Though first
## it checksto see if the inverse it has already been computed.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}
