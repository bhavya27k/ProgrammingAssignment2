 makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(x) inv <<- Inverse(x)
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }
 
 cachesolve <- function(x, ...) {
   inv <- x$getinverse()
     message("getting cached data")
     return(inv)
   
   data <- x$get()
   inv <- Inverse(x, ...)
   x$setinverse(inv)
   inv
 }
 