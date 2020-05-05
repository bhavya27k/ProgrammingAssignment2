
## makeCacheMatrix is a function to set a matrix, get it
## and then set its inverse and get it
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##  get the value of the matrix
  get <- function() x
  ##  set the value of inverse of the matrix
  setinverse <- function(x) inv <<- Inverse(x)
  ##  get the value of inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }
 ##The following function calculates inverse of matrix
 ##created with the above function. However, it first 
 ##checks if the inverse has already been calculated. 
 ##If so, it gets the inverse from the cache and skips the 
 ##computation. Otherwise, it calculates the inverse of the
 ##matrix using the solve function and sets the inverse in the 
 ##cache via the setmat function.
 
 cachesolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)){
     message("getting cached data")
     return(inv)
   }
   
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
 }
 